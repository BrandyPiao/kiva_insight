FINAL CODE

library(ggplot2)
library(googleVis)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(plotly)

################# PART ONE ###
#keep only variables we want to look at
dataset0<-lender_loans[,c(1,4,5,8,11,13,19,21,22,23,24,25,27,29,30,31,37)]

#get rid of duplicated rows
which(duplicated(dataset0))
dataset1<- dataset0 %>% distinct()

#generate levels of status column
levels(dataset1$status)

#get rid of all lenders with loan status of in_repayment
dataset2<-subset(dataset1,status!="in_repayment")


##################### PART TWO ###
dataset3<-ddply(.data=dataset2, .variables = "country_code", summarise, TOTALLOANGIVEN=sum(loan_amount, na.rm =T), AVGLOANGIVEN=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T), ID="LENDER")

colnames(dataset3)[1] <- "COUNTRY"

#top lending countries
dataset3a<-arrange(dataset3, desc(TOTALLOANGIVEN))
dataset3a<-head(dataset3a)
dataset3a<-dataset3a[-c(3),]
dataset3a$Rank<-c(1:5)

dataset4<-ddply(.data=dataset2, .variables = "location.country_code", summarise, TOTALLOANRECEIVED=sum(loan_amount, na.rm =T), AVGRECEIVED=mean(loan_amount, na.rm=T), TOTALREPAID=sum(paid_amount, na.rm = T), AVGREPAID=mean(paid_amount, na.rm = T), ID="RECEIVER")

colnames(dataset4)[1] <- "COUNTRY"


####################### PART THREE ###
#top borrowing countries
dataset4a<-arrange(dataset4, desc(TOTALLOANRECEIVED)) 
dataset4a<-head(dataset4a)
dataset4a <- dataset4a[-nrow(dataset4a),]
dataset4a$Rank<-c(1:5)

dataset3a4a<-merge(dataset3a, dataset4a, by="Rank")
dataset3a4a<-dataset3a4a[,c(1,2,3,8,9)]
dataset3a4ai<-dataset3a4a[,c(1,3,5)]

#scale data
dataset3a4ai[,2]=dataset3a4ai[,2]/1000
dataset3a4ai[,3]=dataset3a4ai[,3]/1000

dataset3a4ai<-melt(dataset3a4ai, id=c("Rank"))

#side by side boxplot of highest lending and highest borrowing countries
ggplot(dataset3a4ai,aes(Rank,value,fill=variable))+
  geom_bar(stat="identity",position="dodge")


######################### PART FOUR ###
#find sectors where money has been lent
dataset5<-ddply(.data=dataset2, .variables = "sector", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))

#top sectors invested in
dataset5a<-arrange(dataset5, desc(TOTALINVESTED))
dataset5a<-head(dataset5a)
dataset5a<-dataset5a[,c(1,2)]
dataset5a[,2]=dataset5a[,2]/1000

#interactive visual showing top sectors invested in
plot_ly(
  x = dataset5a[,1],
  y = dataset5a[,2],
  name = "INVESTED SECTORS",
  type = "bar"
)

######################## PART FIVE ###
#create subsets for top 5 lending countries
dataset6a<- subset(dataset2, country_code=="US") 
dataset6b<- subset(dataset2, country_code=="CA")
dataset6d<- subset(dataset2, country_code=="GB")
dataset6e<- subset(dataset2, country_code=="DE")
dataset6f<- subset(dataset2, country_code=="AU")

#find which occupations lend the highest in the top 5 lending countries
dataset6aa<-ddply(.data=dataset6a, .variables = "occupation", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6aa<-arrange(dataset6aa, desc(TOTALINVESTED))
dataset6aa<-head(dataset6aa)
dataset6aa$Rank<-c(1:6)
dataset6aa$Country<-"US"

dataset6bb<-ddply(.data=dataset6b, .variables = "occupation", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6bb<-arrange(dataset6bb, desc(TOTALINVESTED))
dataset6bb<-head(dataset6bb)
dataset6bb$Rank<-c(1:6)
dataset6bb$Country<-"CA"

dataset6dd<-ddply(.data=dataset6d, .variables = "occupation", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6dd<-arrange(dataset6dd, desc(TOTALINVESTED))
dataset6dd<-head(dataset6dd)
dataset6dd$Rank<-c(1:6)
dataset6dd$Country<-"GB"

dataset6ee<-ddply(.data=dataset6e, .variables = "occupation", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6ee<-arrange(dataset6ee, desc(TOTALINVESTED))
dataset6ee<-head(dataset6ee)
dataset6ee$Rank<-c(1:6)
dataset6ee$Country<-"DE"




########################## PART SIX ###
dataset6ff<-ddply(.data=dataset6f, .variables = "occupation", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6ff<-arrange(dataset6ff, desc(TOTALINVESTED))
dataset6ff<-head(dataset6ff)
dataset6ff$Rank<-c(1:6)
dataset6ff$Country<-"AU"

#merge above data
dataset6<-rbind(dataset6aa, dataset6bb, dataset6dd, dataset6ee, dataset6ff, by="Rank")
dataset6<-dataset6[,c(1,2,6,7)]

#clean data
dataset6 <- dataset6[-nrow(dataset6),]
dataset6<-dataset6[-c(1,7,13,19,25), ]

#scale data
dataset6[,2]=dataset6[,2]/1000 
dataset6<-ddply(.data=dataset6, .variables = "occupation")

#visual showing top 6 occupations in top 5 lending countries
qplot(Country, TOTALINVESTED, geom=c("point", "smooth"), data=dataset6,colour=occupation)

#find top 6 sectors the top 5 lending countries have invested in 
dataset6aaa<-ddply(.data=dataset6a, .variables = "sector", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6aaa<-arrange(dataset6aaa, desc(TOTALINVESTED))
dataset6aaa<-head(dataset6aaa)
dataset6aaa$Rank<-c(1:6)
dataset6aaa$Country<-"US"

dataset6bbb<-ddply(.data=dataset6b, .variables = "sector", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6bbb<-arrange(dataset6bbb, desc(TOTALINVESTED))
dataset6bbb<-head(dataset6bbb)
dataset6bbb$Rank<-c(1:6)
dataset6bbb$Country<-"CA"

dataset6ddd<-ddply(.data=dataset6d, .variables = "sector", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6ddd<-arrange(dataset6ddd, desc(TOTALINVESTED))
dataset6ddd<-head(dataset6ddd)
dataset6ddd$Rank<-c(1:6)
dataset6ddd$Country<-"GB"

dataset6eee<-ddply(.data=dataset6e, .variables = "sector", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6eee<-arrange(dataset6eee, desc(TOTALINVESTED))
dataset6eee<-head(dataset6eee)
dataset6eee$Rank<-c(1:6)
dataset6eee$Country<-"DE"


dataset6fff<-ddply(.data=dataset6f, .variables = "sector", summarise, TOTALINVESTED=sum(loan_amount, na.rm =T), AVGINVESTED=mean(loan_amount, na.rm=T), TOTALRECEIVEDBACK=sum(paid_amount, na.rm = T), AVGRECEIVEDBACK=mean(paid_amount, na.rm = T))
dataset6fff<-arrange(dataset6fff, desc(TOTALINVESTED))
dataset6fff<-head(dataset6fff)
dataset6fff$Rank<-c(1:6)
dataset6fff$Country<-"AU"

#merge above data
dataset7<-rbind(dataset6aaa, dataset6bbb, dataset6ddd, dataset6eee, dataset6fff, by="Rank")
dataset7<-dataset7[,c(1,2,6,7)]

#clean data
dataset7 <- dataset7[-nrow(dataset7),]

#scale data
dataset7[,2]=dataset7[,2]/1000 
dataset7<-ddply(.data=dataset7, .variables = "sector")

#visual showing top 6 sectors top 5 lending countries have invested in
qplot(Country, TOTALINVESTED, geom=c("point", "smooth"), data=dataset7,colour=sector)





############################### PART SEVEN ###
#subset of defaulters
dataset8<-subset(dataset2, status=="defaulted")

#find top 6 defaulters by default count
dataset9<-ddply(.data=dataset8, .variables = "location.country_code", summarise, DEFAULTCOUNT=length(borrower_count))
dataset9<-arrange(dataset9, desc(DEFAULTCOUNT))
dataset9<-head(dataset9)

#find data of top 6 defaulters
dataset9a<-subset(dataset8, location.country_code=="KE")
dataset9b<-subset(dataset8, location.country_code=="TG")
dataset9c<-subset(dataset8, location.country_code=="EC")
dataset9d<-subset(dataset8, location.country_code=="AF")
dataset9e<-subset(dataset8, location.country_code=="TZ")
dataset9f<-subset(dataset8, location.country_code=="DO")

#find highest defaulting sectors in each of top 6 defaulting countries
dataset9aa<-ddply(.data=dataset9a, .variables = "sector", summarise, DEFAULTCOUNT=length(borrower_count))
dataset9aa<-arrange(dataset9aa, desc(DEFAULTCOUNT))
dataset9aa<-head(dataset9aa)
dataset9aa$Rank<-c(1:6)
dataset9aa$Country<-"KE"

dataset9bb<-ddply(.data=dataset9b, .variables = "sector", summarise, DEFAULTCOUNT=length(borrower_count))
dataset9bb<-arrange(dataset9bb, desc(DEFAULTCOUNT))
dataset9bb<-head(dataset9bb)
dataset9bb$Rank<-c(1:6)
dataset9bb$Country<-"TG"

dataset9cc<-ddply(.data=dataset9c, .variables = "sector", summarise, DEFAULTCOUNT=length(borrower_count))
dataset9cc<-arrange(dataset9cc, desc(DEFAULTCOUNT))
dataset9cc<-head(dataset9cc)
dataset9cc$Rank<-c(1:6)
dataset9cc$Country<-"EC"

dataset9dd<-ddply(.data=dataset9d, .variables = "sector", summarise, DEFAULTCOUNT=length(borrower_count))
dataset9dd<-arrange(dataset9dd, desc(DEFAULTCOUNT))
dataset9dd<-head(dataset9dd)
dataset9dd$Rank<-c(1:6)
dataset9dd$Country<-"AF"

dataset9ee<-ddply(.data=dataset9e, .variables = "sector", summarise, DEFAULTCOUNT=length(borrower_count))
dataset9ee<-arrange(dataset9ee, desc(DEFAULTCOUNT))
dataset9ee<-head(dataset9ee)
dataset9ee$Rank<-c(1:6)
dataset9ee$Country<-"TZ"

dataset9ff<-ddply(.data=dataset9f, .variables = "sector", summarise, DEFAULTCOUNT=length(borrower_count))
dataset9ff<-arrange(dataset9ff, desc(DEFAULTCOUNT))
dataset9ff<-head(dataset9ff)
dataset9ff$Rank<-c(1:6)
dataset9ff$Country<-"DO"

#merge above data
dataset10<-rbind(dataset9aa, dataset9bb, dataset9cc, dataset9dd, dataset9ee, dataset9ff, by=2)

#clean data
dataset10 <- dataset10[-nrow(dataset10),]

#visual showing top 6 defaulting sectors in top 6 defaulting countries
qplot(Country, DEFAULTCOUNT, geom=c("point", "smooth"), data=dataset10,colour=sector)


