#load the data in R
getwd()
setwd("G:/UCSC/Data Analysis & R/Project/ProjectDataUse/")
library(plyr)
library(dplyr)
order_item_data = read.csv("order_products__prior.csv",nrows = 60000)
#tableau
#write.csv(order_item_data,"order_item_data.csv", row.names = TRUE)
#remember, this data is very huge so take only the first 10 items
head(order_item_data,10)
#quiickly check for dimensions for the amount of data
dim(order_item_data)
#see, if there are any negative values
summary(order_item_data)

#repeat it for aisles
aisles = read.csv("aisles.csv")
dim(aisles)
summary(aisles)

#departments
departments = read.csv("departments.csv")
dim(departments)
summary(departments)
head(departments,10)

orders = read.csv("orders.csv",nrows = 900000)
dim(orders)
summary(orders)
head(orders,10)
#days_since_prior_order have lots of NA. We need to check it later on.

items = read.csv("products.csv")
dim(items)
summary(items)
head(items,10)

#our training or test data set
Order_Items_Train_Data = read.csv("order_products__train.csv",nrows =80000)
dim(Order_Items_Train_Data) # 80000 rows
summary(Order_Items_Train_Data)
#there are no NAs and -ve values


sampSub= read.csv("sample_submission.csv",stringsAsFactors = F)
#this file has products as strings, which I don't need as factors
dim(sampSub)
str(sampSub)
head(sampSub,10)


#let's check the NA in orders data
summary(orders)
nrow(orders[is.na(orders$days_since_prior_order),])
#remove these NA values
orders = na.omit(orders)
#these NA's are removed. There are no -ve values so need to handle that

 
#now the NAs are removed. We only focus on returning customers with unique orderIds
length(orders$order_id)#845953
length(unique(orders$order_id)) #845953
#there are no duplicate order_id 
hist(orders$order_id,
     main = "No. of Orders",
     xlab = "Orders",
     ylab = "count",
      col = "light blue", border ="black")

#distinctOrders1 = orders %>% group_by(order_id) %>% summarise() %>%
#  select(unique.order_id=order_id)
#View(distinctOrders1)

#let's see orders per each customers

OrdersPerCustomer <- orders %>%
  group_by(user_id) %>%
  mutate(distinctOrders=n_distinct(order_id)) %>% 
  arrange(desc(distinctOrders))
head(OrdersPerCustomer)


#filter Out user_id where orders placed are more than 90
TopCustomerCount = OrdersPerCustomer %>% group_by(eval_set,user_id,distinctOrders)%>% 
  filter(distinctOrders>90)%>% summarise(count = n())%>%filter(count>=98)
 #length(TopCustomerCount$user_id)
TopCustomerCount = OrdersPerCustomer %>% group_by(eval_set,user_id,distinctOrders)%>% 
  filter(distinctOrders>90)
head(TopCustomerCount)
Top1CustomerCount = TopCustomerCount[which(TopCustomerCount$user_id==210),]
head(Top1CustomerCount)
#Top10CustomerCount = Top10CustomerCount %>% mutate(user_id = as.factor(user_id))
str(Top1CustomerCount)
hist(Top1CustomerCount$order_dow,
     main = "User 210 Sales",
     xlab = "Order dow",
     ylab = "count",
     col = "orange", border ="black")


Top2CustomerCount = TopCustomerCount[which(TopCustomerCount$user_id==964),]
head(Top2CustomerCount)
#Top10CustomerCount = Top10CustomerCount %>% mutate(user_id = as.factor(user_id))
str(Top2CustomerCount)
hist(Top2CustomerCount$order_dow,
     main = "User 964 Sales",
     xlab = "Order dow",
     ylab = "count",
     col = "lightblue", border ="black")

#combine both ggplots together
#install.packages('ggplot2')
library(ggplot2)

Top2CustomerCount = Top2CustomerCount %>% mutate(cust = 964)
head(Top2CustomerCount)
Top1CustomerCount = Top1CustomerCount %>% mutate(cust = 210)
head(Top1CustomerCount)
CustomerDayPatterns = rbind(Top1CustomerCount, Top2CustomerCount)
head(CustomerDayPatterns)
#ggplot(CustomerDayPatterns, aes(length, fill = cust_id)) + geom_density(alpha = 0.2)
#plot(Top1CustomerCount$order_dow,Top1CustomerCount$order_number,type = "h",lwd = 10,
#     col = "blue",xlab="dow",ylab="no.ofOrders")


#Merge orders from orders.csv and order_products_prior.csv. match on order_id
orderDetails= merge(x=distinctOrders, y=order_item_data, by="order_id")
orderDetails = orderDetails %>% group_by(eval_set)
head(orderDetails,100)
tail(orderDetails,100)

#Check how frequent customers make orders and how recent did they make the orders

Insta_cust_RFM <- orderDetails %>%
  group_by(user_id) %>%
  mutate(frequency=n_distinct(order_id),
         recency=min(days_since_prior_order),
         monetary = n_distinct(product_id)) 
head(Insta_cust_RFM)
summary(Insta_cust_RFM)
dim(Insta_cust_RFM)
cor(Insta_cust_RFM[,c(4:7)])
#Insta_cust_RFM[,c(-1,-2,-3,-8,-10)]
cor(Insta_cust_RFM[,c(-1,-2,-3,-8,-10)])
pairs(Insta_cust_RFM[,c(-1,-2,-3,-8,-10)])

orderDetails[(orderDetails$user_id ==66),]
HigherFrequencyCust = Insta_cust_RFM[(Insta_cust_RFM$user_id ==41591),]

hist(HigherFrequencyCust$order_dow,
     main = "User 41591 Sales",
     xlab = "Order dow",
     ylab = "count",
     col = "lightblue", border ="black")

#now pull only those userIds which have higher frequency order by monetary 
Insta_cust_HighestFreq = Insta_cust_RFM %>% group_by(user_id) %>%
  filter(frequency>1) %>% arrange(desc(monetary)) %>% 
  filter(recency< 10) %>% ungroup() %>% sample_n(10) %>% arrange(desc(monetary))
head(Insta_cust_HighestFreq)

Insta_cust_HighestFreqPlot = Insta_cust_RFM[which(Insta_cust_RFM$user_id==6409),]
head(Insta_cust_HighestFreqPlot)
#Top10CustomerCount = Top10CustomerCount %>% mutate(user_id = as.factor(user_id))
str(Top1CustomerCount)
hist(Insta_cust_HighestFreqPlot$order_dow,
     main = "User 6409 Orders",
     xlab = "Order dow",
     ylab = "count",
     col = "light green", border ="black",
     xlim = c(0,6))

Insta_cust_HighestFreqPlot2 = Insta_cust_RFM[which(Insta_cust_RFM$user_id==17155),]
hist(Insta_cust_HighestFreqPlot2$order_dow,
     main = "User 17155 Orders",
     xlab = "Order dow",
     ylab = "count",
     col = "light yellow", border ="black",
     xlim =c(0,6))


Insta_cust_HighestFreqPlot3 = Insta_cust_RFM[which(Insta_cust_RFM$user_id==26938),]
head(Insta_cust_HighestFreqPlot3)
hist(Insta_cust_HighestFreqPlot3$order_dow,
     main = "User 26938 Orders",
     xlab = "Order dow",
     ylab = "count",
     col = "light pink", border ="black",
     xlim =c(0,6))



Insta_cust_HighestFreqPlot5 = Insta_cust_RFM[which(Insta_cust_RFM$user_id==45587),]
hist(Insta_cust_HighestFreqPlot5$order_dow,
     main = "User 45587 Orders",
     xlab = "Order dow",
     ylab = "count",
     col = "grey", border ="black",
     xlim =c(0,6))

#Customer 45587 again showed a pattern as it usually buys on 6th day i.e. Sunday


set.seed(42)
clus = kmeans(Insta_cust_RFM[,11:13], 4)
clus$cluster <- as.factor(clus$cluster)
(clus$centers)

#remove.packages(c("ggplot2", "data.table"))
#install.packages('rlang', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)
#library(ggplot2)

#dow of all the orders
ggplot(Insta_cust_RFM, aes(order_dow)) +
  geom_histogram()

#here one can plot the order count for prior and train
ggplot(Insta_cust_RFM, aes(monetary,color ="grey")) +
  geom_histogram()

ggplot(Insta_cust_RFM, aes(monetary,frequency,color =clus$cluster)) +
  geom_point()

#this doesn't work
ggplot(Insta_cust_RFM, aes(x=user_id, y=order_dow)) +
  geom_point(alpha=0.02, aes(size=monetary)) +
  geom_smooth(method = "auto") +
  theme(panel.grid.major = element_line(colour = "tan1"))


#Now we'll study the relationships between orders and products
order_Item_train_set = merge(orderDetails,items, by = "product_id")
head(order_Item_train_set)


#Insta_prod_RFM <- order_Item_train_set %>%
#  group_by(user_id,product_id) %>%
#  mutate(frequency=n(),
#         recency=min(days_since_prior_order),
#         monetary = n_distinct(order_id)) 
#View(Insta_prod_RFM)

Insta_prod_RFM <- order_Item_train_set %>%
  group_by(product_id) %>%
  mutate(frequency=n_distinct(order_id),
         recency=min(days_since_prior_order),
         monetary = n()) 
head(Insta_prod_RFM)

#test the data returmed from above query
#ItemCount = Insta_prod_RFM[((Insta_prod_RFM$user_id==22963) & (Insta_prod_RFM$product_id==117)),]
#View(ItemCount)

#TopItem = Insta_prod_RFM[(Insta_prod_RFM$product_id==24852),]
#View(ItemCount)

#now pull only those items which have higher monetary order by frequency 
Insta_item_HighestFreq = Insta_prod_RFM %>% group_by(product_id) %>%
  filter(frequency>1) %>% arrange(desc(monetary))%>% 
  ungroup() %>% sample_n(10) %>% arrange(desc(monetary))
head(Insta_item_HighestFreq)

TopItem1= Insta_prod_RFM[(Insta_prod_RFM$product_id==24852),]
head(TopItem1)
hist(TopItem$order_dow,
     main = "Product 24852 pattern",
     xlab = "Product dow",
     ylab = "count",
     col = "light pink", border ="black",
     xlim =c(0,6))


TopItem2= Insta_prod_RFM[(Insta_prod_RFM$product_id==21137),]
head(TopItem2)
hist(TopItem2$order_dow,
     main = "Product 21137 pattern",
     xlab = "Product dow",
     ylab = "count",
     col = "light pink", border ="black",
     xlim =c(0,6))
#All days

#44632
TopItem3= Insta_prod_RFM[(Insta_prod_RFM$product_id==44632),]
head(TopItem3)
hist(TopItem3$order_dow,
     main = "Product 44632 pattern",
     xlab = "Product dow",
     ylab = "count",
     col = "orange", border ="black",
     xlim =c(0,6))

#lowest item
#21333
LastItem= Insta_prod_RFM[(Insta_prod_RFM$product_id==21333),]
head(LastItem)
hist(LastItem$order_dow,
     main = "Product 21333 pattern",
     xlab = "Product dow",
     ylab = "count",
     col = "red", border ="black",
     xlim =c(0,6))

#apriori algorithm
#group the items by order_id
Insta_prod_aprio = order_Item_train_set %>%
  group_by(order_id)%>% arrange(order_id) %>% select(user_id,order_id,product_id,order_dow,order_hour_of_day,product_name)
head(Insta_prod_aprio,3)

#Now make the order_ids as numeric
Insta_prod_aprio_sorted = Insta_prod_aprio[order(Insta_prod_aprio$order_id),]
str(Insta_prod_aprio_sorted)
Insta_prod_aprio_sorted$order_id = as.numeric(Insta_prod_aprio_sorted$order_id)
head(Insta_prod_aprio_sorted)

#based on order id, combine all the itens
Insta_prod_aprio_List = ddply(Insta_prod_aprio_sorted,c("order_id"), 
                     function(df1)paste(df1$product_name, 
                                        collapse = ","))
colnames(Insta_prod_aprio_List) = c("order_no","All Items")

#write this file to a csv
write.csv(Insta_prod_aprio_List,"InstaProdOrder.csv", row.names = TRUE)

#apriori algorithm
#checkin the orders w.r.t user id. Earlier we checked w.r.t order_id
#now pull only those userIds which have higher frequency order by monetary 
HighestFreqCust = Insta_cust_RFM %>% group_by(user_id) %>%
  filter(frequency>1) %>% arrange(user_id) %>% 
  ungroup() %>% sample_n(10) %>% arrange(user_id)
head(HighestFreqCust)
HighestFreqCust[HighestFreqCust$user_id==17155,]

#Check this user id in our dataset
Insta_prod_user_prio = order_Item_train_set %>%
  group_by(user_id,order_id)%>% arrange(user_id,order_id) %>% select(user_id,order_id,product_id,order_dow,order_hour_of_day,product_name)
Insta_prod_user_prio[Insta_prod_user_prio$user_id==17155,]

Insta_prod_user_aprio_sorted = Insta_prod_user_prio[order(Insta_prod_user_prio$order_id),]
Insta_prod_user_aprio_sorted$order_id = as.numeric(Insta_prod_user_aprio_sorted$order_id)

Insta_prod_order_List_new = ddply(Insta_prod_user_aprio_sorted,c("user_id","order_id","order_dow"), 
                              function(df1)paste(df1$product_name, 
                                                 collapse = ","))
Insta_prod_order_List_new = Insta_prod_order_List_new %>% arrange(user_id,order_id)

#View(head(Insta_prod_order_List_new,10))
colnames(Insta_prod_order_List_new) = c("user_id","order_id","order_dow","All Items")
#write this file to a csv
write.csv(Insta_prod_order_List_new,"Insta_Order_User_Products.csv", row.names = TRUE)



#install.packages("arules", dependencies=TRUE)
library(arules) 
require(arules)

#There is a problem with commas in the file so please change commas to / in notepad and save as .csv
txn4 = read.transactions(file="InstaProdNotepad.csv",sep="/")
txn4
summary(txn4)
inspect(txn4[1:3])

#txn4@itemInfo$labels = gsub("\"","",txn4@itemInfo$labels)
itemFrequencyPlot(txn4,topN=10,type="absolute")

#support: what % of transactions does the product show up.
#total there was 1473(rows)*5926(cols)*(0.001650476)density= 13966.3968
itemFrequency(txn4[,1])
#0% Greek Strained Yogurt 
#0.0006784261 * 13966.3968 = 9.47

itemFrequency(txn4[,1:10])

#10% of transactions, this product is shown
itemFrequencyPlot(txn4,support=0.10)

basket_rules = apriori(txn4,parameter = list(sup = 0.01, conf = 0.01,target="rules"))
#detach("package:arules", unload=TRUE)
#library("arules", lib.loc="[wherever your R libraries go]")
basket_rules1 = apriori(txn4,parameter = list(support = 0.003, confidence = 0.25,minlen=2))
basket_rules1

summary(basket_rules1)

inspect(basket_rules1[1:4]) #good for smaller no. of rows

#Let's sort and view some items
inspect(sort(basket_rules1,by = "lift")[1:8])
inspect(sort(basket_rules1,by = "support")[1:8])
inspect(sort(basket_rules1,by = "confidence")[1:8])

my_final_cart = as(basket_rules1,"data.frame")
my_final_cart
library(arulesViz)
plot(my_final_cart)

#install.packages("arulesViz", type = "source")

#Please view the items for each customer
txn5 = read.transactions(file="Insta_Order_User_Products.csv",sep="/")
txn5
summary(txn5)
inspect(txn5[1:2])
#txn4@itemInfo$labels = gsub("\"","",txn4@itemInfo$labels)
itemFrequencyPlot(txn5,topN=10,type="absolute")

#support: what % of transactions does the product show up.
#total there was 1473(rows)*5926(cols)*(0.001650476)density= 13966.3968
itemFrequency(txn5[,1:5])
#0% Greek Strained Yogurt 
#0.0006784261 * 13966.3968 = 9.47

itemFrequency(txn5[,1:10])

#10% of transactions, this product is shown
itemFrequencyPlot(txn5,support=0.10)

basket_rules_for_user = apriori(txn5,parameter = list(support = 0.003, confidence = 0.25,minlen=2))
basket_rules_for_user

summary(basket_rules_for_user)

inspect(basket_rules_for_user[1:4]) 
#Let's sort and view some items
inspect(sort(basket_rules_for_user,by = "lift")[1:8])
inspect(sort(basket_rules_for_user,by = "support")[1:8])
inspect(sort(basket_rules_for_user,by = "confidence")[1:8])

my_final_cart_user = as(basket_rules_for_user,"data.frame")
# my_cart = as.data.frame(basket_rules)
# DATAFRAME(basket_rules)

#install.packages("arulesViz")
library(arulesViz)
plot(my_final_cart_user)



#Now make User-item percentages
#partition the training set and select 20% for validation
#From main orders, filter the test orders and then set 20% for validation. Sample seed of size =1 and prob=0.8
trainOrders = orders %>% filter(eval_set =="train") 
set.seed(9383)
trainSet = as.logical(
  rbinom(
    n = nrow(trainOrders), 
    size = 1, 
    prob = 0.8
  )
)
head(trainOrders,5)
trainingIDs = trainOrders %>% filter(trainSet)
head(trainingIDs,5)
validationIDs = trainOrders %>% filter(!trainSet)
head(validationIDs,10)
nrow(trainingIDs) #27428
nrow(validationIDs) #6799 

#Now, see which products are listed in my sample submission form
str(sampSub)
head(sampSub,10)
#I can see that products are listed as strings under products which itself is in sampSub list
sampResults = strsplit(sampSub$products[1],"\\ ") 
sampResults = as.numeric(sampResults[[1]]) #first element of first list
sampResults
#Now product ids are seperated and converted into Numeric

#let's see which items are these
head(items,2)
items[(items$product_id == 39276),]
#bananas
items[(items$product_id == 29259),]
#Baby Bananas


#let's validate these products in my validation Order ids if they actually contain these products
#I'll pass the validationIds and products from my sampleSubmission. Just one row is fine
#as all products are duplicate
validateProducts = data.frame(order_id = validationIDs, products = sampSub[1,2])
head(validateProducts,10)
str(validateProducts)
#Again the products are factors. Convert them to char
validateProducts[,2] = as.character(validateProducts[,2])
#Now, converted into char
head(validateProducts,2)

#Now join orders from Orders data set with Training Order Items
#We want all orderIds from Training Order Items and matching ones in training set
#Also, only those products which are reordered i.e. reordered =1
TrainModel = filter(orders, eval_set == "train") %>% left_join(Order_Items_Train_Data,by = "order_id") %>% 
  filter(reordered == 1) %>% select("order_id","product_id","add_to_cart_order")%>%
  group_by(order_id) %>%
  mutate(productNames = paste0(product_id, collapse = " ")) 

# ordersTest = orders[orders$user_id==2175,] ;ordersTest
# ordersTestTrain = Order_Items_Train_Data %>% inner_join(ordersTest,by ="order_id")
# ordersTestTrain
#More than one order for an user in train data set
MoreThanOneOrder = Order_Items_Train_Data %>% inner_join(orders,by ="order_id") %>%
group_by(user_id) %>% mutate(no_orders = n_distinct(order_id)) %>%
  select("user_id","order_id","no_orders")%>% arrange(desc(no_orders))
head(MoreThanOneOrder,100)



#take out the count of all distinct orders for customers in Train and Orders
nOrdersTrain = Order_Items_Train_Data %>% inner_join(orders,by ="order_id") %>% 
  group_by(user_id) %>% mutate(no_orders = n_distinct(order_id)) %>%
  select("user_id","order_id","no_orders") %>% arrange(desc(no_orders))

#take the prior order set
nOrders = order_item_data %>% inner_join(orders,by ="order_id") %>% 
  group_by(user_id) %>% mutate(no_orders = n_distinct(order_id)) %>%
  select("user_id","order_id","no_orders") %>% arrange(desc(no_orders))


#There is only one order per user in training set but multiple orders in test set

#take out the count of all user-items pair and see in how many orders for the user are they occuring
#incorrect
ProductsInOrderPercentage = Order_Items_Train_Data %>% inner_join(orders) %>% 
  group_by(user_id, product_id) %>% summarize(count = n()) #%>%
  # inner_join(nOrders,by="user_id") %>% mutate(Percentage = count/no_orders) %>% 
  # arrange(desc(Percentage)) %>% select(user_id, product_id,order_id,Percentage) 

#correct
#the training data set does not have multiple order ids hence it doesn't work for them
ProductsInOrderPercentage1 = order_item_data %>% inner_join(orders) %>% 
  group_by(user_id, product_id) %>% mutate(count = n()) %>%
  select(user_id, product_id,order_id,order_number,order_dow,count)%>% 
  arrange(user_id,product_id,order_id) %>% arrange(desc(count))
# inner_join(nOrders,by="user_id") %>% mutate(Percentage = count/no_orders) %>% 
# arrange(desc(Percentage)) %>% select(user_id, product_id,order_id,Percentage) 

ProductsInOrderPercentage2 = order_item_data %>% inner_join(orders) %>% 
  group_by(user_id, product_id) %>% mutate(nOfProds = n()) %>%
  inner_join(nOrders,by="user_id") %>% mutate(Percentage = nOfProds/no_orders) %>% 
  select(user_id, product_id,order_id.y,Percentage,nOfProds,no_orders)%>% arrange(user_id,product_id,order_id.y) %>%
  arrange(desc(Percentage)) 

#Now check the items which are occuring in almost every transaction of the user
EvaluatePercentage = ProductsInOrderPercentage2 %>% filter(Percentage >= 1) %>%
  arrange(desc(nOfProds))
head(EvaluatePercentage,10)


###################################


#Logistic regression
trainOrdersDays = orders %>% filter(eval_set == "train") %>% select(order_id, user_id, days_since_prior_order)
head(trainOrdersDays,10)

trainOrdersItems = Order_Items_Train_Data %>% filter(reordered == 1)%>% 
  group_by(order_id) %>% mutate(prods = paste0(product_id, collapse = " "))  %>%
  select(order_id, prods) %>% unique()
head(trainOrdersItems,10)

#please run this function and calling variable together
word.in.sentence = function(word, sentence){
  word %in% strsplit(sentence, split = " ")[[1]]
}
ItemsInOrders = trainOrdersDays %>% inner_join(ProductsInOrderPercentage) %>% inner_join(trainOrdersItems) %>% group_by(order_id) %>% 
  mutate(Exists = word.in.sentence(product_id, prods)) %>% data.frame()
ItemsInOrders

# let's train a logistic regression model with days_since_prior_order and Percentage as the regressors.
LinearRelation = glm(Exists ~ Percentage + days_since_prior_order, data = ItemsInOrders, family = binomial)
summary(LinearRelation)

#Logistic regression
priorOrdersDays = orders %>% filter(eval_set == "prior") %>% select(order_id, user_id, days_since_prior_order)
head(priorOrdersDays,10)
priorOrdersDays[priorOrdersDays$user_id==17155,]


priorOrdersItems = order_item_data %>% filter(reordered == 1)%>% 
  group_by(order_id) %>% mutate(prods = paste0(product_id, collapse = " "))  %>%
  select(order_id, prods) %>% unique()
View(priorOrdersItems)
head(priorOrdersItems,10)

#this code will give true for products occuring in the corresponding product list of user
#please run this function and calling variable together
word.in.sentence = function(word, sentence){
  word %in% strsplit(sentence, split = " ")[[1]]
}
ItemsInOrders = priorOrdersDays %>% inner_join(ProductsInOrderPercentage2) %>% inner_join(priorOrdersItems) %>% group_by(order_id) %>% 
  mutate(Exists = word.in.sentence(product_id, prods)) %>% data.frame()
View(ItemsInOrders[ItemsInOrders$user_id==17155 & ItemsInOrders$product_id==1158,])
head(ItemsInOrders,10)


# let's train a logistic regression model with days_since_prior_order and Percentage as the regressors.
LinearRelation = glm(Exists ~ Percentage + days_since_prior_order, data = ItemsInOrders, family = binomial)
summary(LinearRelation)


#Now, this model outputs an updated "probability" of a certain item 
#being in an order which is an increasing function of the fraction of 
#prior orders and a decreasing function of the time since the previous 
#order was made - the longer a user waits between orders, the less 
#likely it is that they will buy something! 














