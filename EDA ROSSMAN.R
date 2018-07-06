train_data<- read.csv(file="train.csv")
store_data<-read.csv(file = "store.csv")

test_data <- read.csv(file="test.csv")

summary(train_data)

unique(is.na(train_data))

unique(train_data$Open)
unique(train_data$Promo)
unique(train_data$StateHoliday)
unique(train_data$SchoolHoliday)

train_data["Open"] <- train_data["Open"]==1
train_data["Promo"] <- train_data["Promo"]==1
train_data["SchoolHoliday"]<- train_data["SchoolHoliday"]==1


test_data["Promo"] <- test_data["Promo"]==1
test_data["SchoolHoliday"]<- test_data["SchoolHoliday"]==1
new_data<-subset(test_data, is.na(test_data$Open))

obs_Open<-sample(na.omit(test_data$Open), 1)
new_data$Open<-obs_Open
test_data$Open[is.na(test_data$Open)] <- new_data$Open

test_data$Open<- test_data$Open==1


test_data$Open[is.na(test_data$Open)] 

train_data$DateDay<- as.numeric(strftime(train_data$Date,format="%d"))
train_data$DateMonth<- as.numeric(strftime(train_data$Date,format="%m"))
train_data$DateYear<- as.numeric(strftime(train_data$Date,format="%y"))
train_data$DateWeek<- as.numeric(strftime(train_data$Date,format="%w"))


train_data <- subset(train_data,select=-c(Customers,Date))

test_data$DateDay<- as.numeric(strftime(test_data$Date,format="%d"))
test_data$DateMonth<- as.numeric(strftime(test_data$Date,format="%m"))
test_data$DateYear<- as.numeric(strftime(test_data$Date,format="%y"))
test_data$DateWeek<- as.numeric(strftime(test_data$Date,format="%w"))
#Dropping Date column
test_data <- subset(test_data,select=-c(Date))

#Dropping Id column as It is redundant here
test_data <- subset(test_data,select=-c(Id))

write.csv(store_data,file="store_data_clean.csv")
write.csv(train_data,file="train_data_clean.csv")
write.csv(test_data,file="test_data_clean.csv")

#To check which day is giving the maximum sales
agg<-aggregate(train_data$Sales, by=list(train_data$DayOfWeek), FUN=mean)
barplot(agg$x, main="sales vs day of week")

#histogram of the sales 
hist(train_data$Sales,xlab="Sales Amount")

#To check which month is giving the maximum sales
agg2<-aggregate(train_data$Sales, by=list(train_data$DateMonth), FUN=mean)
plot(agg2$x, main="sales vs Month")

#Aggregate data of sales of each year and combine it with the store data 
agg_storeSales<-aggregate(train_data$Sales, by=list(train_data$Store), FUN=mean)
colnames(agg_storeSales)<-c("Store","Sales")
merge_salesstore<-merge(x=agg_storeSales,y=store_data,by = "Store", all = TRUE)
write.csv(merge_salesstore,file="merge_salesstore.csv")

#How store type is affecting the sales
agg_type<-aggregate(merge_salesstore$Sales, by=list(merge_salesstore$StoreType), FUN=mean)
plot(agg_type$x, main="type vs sales")

#How Assortment is affecting the sales
agg_assort<-aggregate(merge_salesstore$Sales, by=list(merge_salesstore$Assortment), FUN=mean)
plot(agg_assort$x, main="Assortment vs sales")


#How State holiday is affecting the sales
agg_stateholi<-aggregate(train_data$Sales, by=list(train_data$StateHoliday), FUN=mean)
plot(agg_stateholi$x, main="state holiday vs sales")


merge_salesstore<-merge_salesstore[complete.cases(merge_salesstore[,"CompetitionDistance"]),]
summary(merge_salesstore)

#Aggregate the sales by same distance of competititon stores
compdis<-aggregate(merge_salesstore$Sales,by= list(merge_salesstore$CompetitionDistance),FUN=mean)
colnames(compdis)<-c("Distance to competition","Mean Sales")
plot(x=compdis$`Distance to competition`,y=compdis$`Mean Sales`)

#Create range of the distances and aggregate them
generalcompetitiondistance<-aggregate(compdis$`Mean Sales`, list(cut(compdis$`Distance to competition`, breaks=c(0,5000,10000,15000,20000,25000,30000))), mean)

plot(x=generalcompetitiondistance$Group.1,y=generalcompetitiondistance$x)


with(merge_salesstore, symbols(x=merge_salesstore$CompetitionDistance, y=merge_salesstore$Sales, circles=merge_salesstore$Promo2, inches=1/15,
                  ann=F, bg="steelblue2", fg=NULL))
title(main="main title", sub="sub-title", 
      xlab="x-axis label", ylab="y-axis label")
legend("topright",title = "Promo",c("0","1"),pch=c(20,19), col ="steelblue2")

t.test(train_data[train_data$Promo,]$Sales,train_data[!train_data$Promo,]$Sales)
#As probability us low, we can justify that promo is highly affecting the sales.


