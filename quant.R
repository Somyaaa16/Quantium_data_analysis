
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(lubridate)
library("tidyr")
library(anytime)




trans<- read_excel("E:/RMIT/qantinum/QVI_transaction_data.xlsx")
purchase<- read_csv("E:/RMIT/qantinum/QVI_purchase_behaviour.csv")

## cleaning and preparing transaction dataset
colnames(trans)
nrow(trans)
head(trans)
str(trans)
summary(trans)

unique(trans$PROD_NAME)

is.na(trans$DATE) %>% table()
is.na(trans$STORE_NBR) %>% table()
is.na(trans$LYLTY_CARD_NBR) %>% table()
is.na(trans$TXN_ID) %>% table()
is.na(trans$PROD_NBR) %>% table()
is.na(trans$PROD_NAME) %>% table()
is.na(trans$PROD_QTY) %>% table()
is.na(trans$TOT_SALES) %>% table()

trans<-trans%>% filter(PROD_QTY<50 & TOT_SALES<100)
summary(trans)
nrow(trans)

unique(trans$PROD_NAME)
trans1<-trans %>% separate(PROD_NAME,into=c("Brand name","type"),sep=" ")
head(trans1)
trans<-trans%>% left_join(trans1)
head(trans)

class(trans$DATE)
typeof(trans$DATE)

trans$date<-as.Date(trans$DATE, origin="1900-01-01")
head(trans)

a<-trans %>% separate(PROD_NAME,into=c(NA,NA,"Type","extra","extra2","weight"),extra = "merge", fill = "left")
a

trans<-trans%>% left_join(a)
trans
trans<- select (trans,-c(extra,extra2,DATE,Type))
trans
trans <- trans[, c(10,1,2,3,4,5,8,9,11,6,7)]
trans
unique(trans$`Brand name`)
unique(trans$type)
unique(trans$weight)


## cleaning and preparing and Analyzing purchase dataset

colnames(purchase)
nrow(purchase)
head(purchase)
str(purchase)
summary(purchase)

unique(purchase$PREMIUM_CUSTOMER)
unique(purchase$LIFESTAGE)

is.na(purchase$LYLTY_CARD_NBR) %>% table()
is.na(purchase$LIFESTAGE) %>% table()
is.na(purchase$PREMIUM_CUSTOMER) %>% table()

data<-trans%>% left_join(purchase)
data
str(data)

# Analysis of data
head(data)
unique(data$date)
class(data$date)
typeof(data$date)

month<-months(data$date)

class(month)
unique(month)
data<-data%>% cbind(month)
head(data)

unique(data$type)
summary(data)



#who purchases more
c<-table(data$LIFESTAGE)
barplot(c,ylim=c(0,60000),col=c("blue"),xlab="Lifestage of customer",ylab="Frequency",main="Purchase rate wrt lifestage of customer")

#which brand has highest qty
c1<-table(data$PROD_QTY,data$`Brand name`)
barplot(c1,col=blues9,ylim=c(0,50000), legend = TRUE,ylab="Quantity",xlab="Brand name",main="Brand quantity purchase rate")
abline(h = seq(5000,50000,5000), lty = "dashed", col = "gray30")


#which brand have highest sales
c2<-table(data$TOT_SALES,data$`Brand name`)
barplot(c2,col=blues9,ylim=c(0,50000),ylab="Sales",xlab="Brand name",main="Total sales of brand")
abline(h = seq(5000,50000,5000), lty = "dashed", col = "gray30")



cr<-table(data$PREMIUM_CUSTOMER,data$LIFESTAGE)
barplot(cr,col=blues9,ylim=c(0,60000), legend = TRUE,ylab="No. of customers",xlab="Lifestage",main="Customers Lifestage")
abline(h = seq(5000,60000,5000), lty = "dashed", col = "gray30")



#which month higest sale
boxplot(TOT_SALES~month,data=data,col=c("blue"),ylab="Sale",xlab="Month",main="Total sale per month")

boxplot(TOT_SALES~LIFESTAGE,data=data,col=c("blue"),ylab="Total Sale",xlab="Lifestage",main="Sale wrt Lifestage of customer")

boxplot(PREMIUM_CUSTOMER~type,data=data,col=c("blue"),ylab="Total Sale",xlab="Type of customer",main="Sale of Customer")


#which customer buys more
x1<-table(data$PREMIUM_CUSTOMER)
barplot(x1,ylim=c(0,120000), col = "blue",ylab="Frequency",xlab="Type of customer",main="Customer buying rate")
unique(data$TOT_SALES)

#how much qty most purchased
v<-table(data$weight)
barplot(v,ylim=c(0,60000),col=c("lightblue"),ylab="Frequency",xlab="Product weight",main="Product quantity preffered")

#top brands
v1<-table(data$`Brand name`)
barplot(v1,ylim=c(0,50000),col=c("blue"),ylab="Brand",xlab="Frequencyr",main="Brand preference")

#which type preffered most
v2<-table(data$type)
barplot(v2,ylim=c(0,26000),col=c("blue"),ylab="Type",xlab="Frequency",main="Snack type preference")


ggplot(data,aes(x = PROD_QTY,fill = LIFESTAGE)) +
  geom_density(alpha = 0.4) +
  labs(title = "product quantity by lifestage",y="Frequency",x="Product quantity")
unique(data$type)






