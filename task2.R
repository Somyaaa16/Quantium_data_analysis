
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(lubridate)
library("tidyr")
library(anytime)



data <- read_csv("C:/Users/SOMYA SINGHAL/Downloads/QVI_data.csv")
head(data)
colnames(data)
nrow(data)
str(data)

# 1 store:
store77<-data%>% filter(STORE_NBR==77 & TOT_SALES) %>% group_by(STORE_NBR)
head(store77)

sales77<-store77[,8]
head(sales77)

#1 total sales revenue
sales77%>% table() %>% sum()

#2 total no. of customer
cus77<-store77[,1]
cus77%>% unique()%>% table() %>% sum()

#3 average number of transactions per customer
df<-data.frame(store77)
df<-select(df,c(1,4,8))


avg_trans<-df%>% select(LYLTY_CARD_NBR)%>% table()
head(avg_trans)
mean(avg_trans)

# 2 store:
store86<-data%>% filter(STORE_NBR==86 & TOT_SALES) %>% group_by(STORE_NBR)
head(store86)
sales86<-store86[,8]

#total sale revenue
sales86%>% table() %>% sum()

#2 total no. of customer
cus86<-store86[,1]
cus86%>% unique()%>% table()%>% sum()

#3 average number of transactions per customer
trans86<-store86$LYLTY_CARD_NBR %>% table()
head(trans86) 
mean(trans86) 

# 3 store:
store88<-data%>% filter(STORE_NBR==88 & TOT_SALES) %>% group_by(STORE_NBR)
head(store88)
sales88<-store88[,8]

#total sale revenue
sales88%>% table() %>% sum()

#2 total no. of customer
cus88<-store88[,1]
cus88%>% unique()%>% table()%>% sum()

#3 average number of transactions per customer
trans88<-store88$LYLTY_CARD_NBR %>% table()
head(trans88) 
mean(trans88)

st<-data$STORE_NBR %>% table()
View(st)
class(st)

#########################################################################

d<-data.frame(data)
month<-months(d$DATE)
d<-d%>% cbind(month)
head(d)
y<-year(d$DATE)
head(y)
d<-d%>% cbind(y)
head(d)
d<-unite(d, "Time period", month, y, sep = " ")
head(d)


d1<-d%>% filter(STORE_NBR==77 | STORE_NBR==86 | STORE_NBR==88) 
d1<-d1%>% filter(`Time period`== "February 2019" | `Time period`== "March 2019" | `Time period`== "April 2019")
View(d1)
rev<-d1 %>% group_by(STORE_NBR,`Time period`) %>% summarise(sale=mean(TOT_SALES))
cust<-d1%>% group_by(STORE_NBR,`Time period`) %>% summarise(no=n_distinct(LYLTY_CARD_NBR))
trans<-d1%>% group_by(STORE_NBR,`Time period`) %>% summarise(no=mean(n_distinct(TXN_ID)))

###########control:

d2<-d%>% filter(STORE_NBR !=77 | STORE_NBR!=86 | STORE_NBR!=88) 
d2<-d2%>% filter(`Time period`> "July 2018" | `Time period`< "June 2019")
View(d2)
m_reven<-d2 %>% group_by(STORE_NBR,`Time period`) %>% summarise(sale=mean(TOT_SALES))
m_cust<-d2%>% group_by(STORE_NBR,`Time period`) %>% summarise(no=n_distinct(LYLTY_CARD_NBR))
m_trans<-d2%>% group_by(STORE_NBR,`Time period`) %>% summarise(no=mean(n_distinct(TXN_ID)))

com1<-left_join(rev,m_reven,by="sale")
com2<-left_join(cust,m_cust,by="no")
com3<-left_join(trans,m_trans,by="no")
distinct(com2$STORE_NBR.y)

a<-com2 %>% distinct(STORE_NBR.y)
b<-com3 %>% distinct(STORE_NBR.y)
w<-intersect(a,b)
View(w)


#Once you have selected your control stores, compare each trial and control pair during the trial period.
#You want to test if total sales are significantly different in the trial period a


comp<-d%>% filter(`Time period`== "February 2019" | `Time period`== "March 2019" | `Time period`== "April 2019")

#control  stores
comp<-comp%>% filter(STORE_NBR==2 |STORE_NBR==2 |STORE_NBR==2 |STORE_NBR==2 |STORE_NBR==2 |STORE_NBR==2 |STORE_NBR==2 |
                       STORE_NBR==12 |STORE_NBR==77 |STORE_NBR==233 |STORE_NBR==240 |STORE_NBR==53 |STORE_NBR==66 |STORE_NBR==246 |
                       STORE_NBR==18 |STORE_NBR==103 |STORE_NBR==220 |STORE_NBR==242 |STORE_NBR==27 |STORE_NBR==74 |STORE_NBR==228 |
                       STORE_NBR==25 |STORE_NBR==111 |STORE_NBR==205 |STORE_NBR==251 |STORE_NBR==21 |STORE_NBR==89 |STORE_NBR==200 |
                       STORE_NBR==37 |STORE_NBR==126 |STORE_NBR==197 |STORE_NBR==253 |STORE_NBR==20 |STORE_NBR==90 |STORE_NBR==185 |
                       STORE_NBR==46 |STORE_NBR==131 |STORE_NBR==176 |STORE_NBR==264 |STORE_NBR==16 |STORE_NBR==142 |STORE_NBR==182 |
                       STORE_NBR==50 |STORE_NBR==143 |STORE_NBR==170 |STORE_NBR==268 |STORE_NBR==9 |STORE_NBR==149 |STORE_NBR==171 |
                       STORE_NBR==54 |STORE_NBR==150 |STORE_NBR==163 |STORE_NBR==272 |STORE_NBR==1 |STORE_NBR==167 |STORE_NBR==169 |
                       STORE_NBR==249 |STORE_NBR==33 |STORE_NBR==55 |STORE_NBR==216 |STORE_NBR==223 |STORE_NBR==178 |STORE_NBR==181 |
                       STORE_NBR==256 |STORE_NBR==26 |STORE_NBR==67 |STORE_NBR==194 |STORE_NBR==250 |STORE_NBR==166 |STORE_NBR==191 |
                       STORE_NBR==22 |STORE_NBR==4 |STORE_NBR==69 |STORE_NBR==184 |STORE_NBR==257 |STORE_NBR==156 |STORE_NBR==217 |
                       STORE_NBR==34 |STORE_NBR==3 |STORE_NBR==72 |STORE_NBR==164 |STORE_NBR==62 |STORE_NBR==154 |STORE_NBR==222 |
                       STORE_NBR==38 |STORE_NBR==260 |STORE_NBR==66 |STORE_NBR==157 |STORE_NBR==80 |STORE_NBR==137 |STORE_NBR==236 |
                       STORE_NBR==64 |STORE_NBR==234 |STORE_NBR==97 |STORE_NBR==152 |STORE_NBR==93 |STORE_NBR==125 |STORE_NBR==269 |
                       STORE_NBR==73 |STORE_NBR==214 |STORE_NBR==113 |STORE_NBR==138 |STORE_NBR==95 |STORE_NBR==123 |STORE_NBR==63 |
                       STORE_NBR==108 |STORE_NBR==174 |STORE_NBR==114 |STORE_NBR==133 |STORE_NBR==100 |STORE_NBR==112 |STORE_NBR==210 |
                       STORE_NBR==227 |STORE_NBR==230 |STORE_NBR==40 |STORE_NBR==88 |STORE_NBR==237 |STORE_NBR==58 |STORE_NBR==199 |STORE_NBR==238)

n<-comp %>% group_by(`Time period`) 

View(n)









