
#***********************************************************************
#*************************                          ********************
#*                                  FORBES 2000                       **
#*                           GLOBAL LIST-Version 2016                 **
#*                       Statistical Review BY "R"                    **
#*                                                                    **
#*                                                                    **
#*                             R Final Project                        **
#*                                                                    **
#*                                Spring 2018                         **
#*                                                                    **
#*************************                          ********************
#***********************************************************************

getwd()#find out working directory
#C:\Users\reza\Documents


#***************  importing csv file  from workin  *********************

Forbes2000=read.csv("fg2016.csv", header=TRUE,sep="," )

#***************    Data set   initial review        *******************

head(Forbes2000)#getting a total overview about data
h2017

str(Forbes2000)#look at description of data structure

class(Forbes2000)#class of data set

dim(Forbes2000)#dimension of dataset 

nrow(Forbes2000)#number of rows

ncol(Forbes2000)#number of columns

names(Forbes2000)#variable names

class(Forbes2000[, "Rank"])#The values of single variables

length(Forbes2000[, "Rank"])#length of ranking vector

Forbes2000[, "Company"][1]#world's largest company

class(Forbes2000[, "Sector"])#finding the class of categorical 

nlevels(Forbes2000[, "Sector"])#extract business Sectors

levels(Forbes2000[, "Sector"])#extract business Sectors 

table(Forbes2000[, "Sector"])#numbers of comanies are in each Sector 

#**************   Dealing with missing value     **********************  


attach(Forbes2000)
class(Sector)#check Sector's class after detaching

is.na(Sector)

newForbes2000=na.omit(Forbes2000)#omit Na from data set

dim(newForbes2000)

#check if is there any missing value
Forbes2000[!complete.cases(Forbes2000),]

#check if is there any missing value
complete.cases(Forbes2000)

#change class of Sector from Factor to Character
Forbes2000$Sector=as.character(Forbes2000$Sector)

#check the Sector class changing
is.character(Forbes2000$Sector)

#add unknown level to Sector instead of blank space
Forbes2000$Sector[Forbes2000$Sector==""|Forbes2000$Sector==" "]="unknown"

#changing back Sector class to Factor
Forbes2000$Sector=as.factor(Forbes2000$Sector)

#check new Sector class
is.factor(Forbes2000$Sector)

levels(Forbes2000$Sector)

str(Forbes2000)

#*********************  some statistical data manipulation   **********

#sort according to Sales
order_sales<-order(Forbes2000$Sales,decreasing = TRUE)

#3 biggest companies according to their sales
Forbes2000[order_sales[1:3] ,c("Company","Sales", "Profits", "Assets")]


attach(Forbes2000)

median(Forbes2000[, "Sales"])#calculating median of sales

mean(Forbes2000[,"Sales"])#calculating mean of sales

range(Forbes2000[, "Sales"])#calculating range of sales


summary(Forbes2000[, "Sales"])#5 number for Sales

companies <- Forbes2000[, "Company"]

companies[1:3]==companies[-(4:2000)]#top three companies


Forbes2000[1:3, c("Company", "Sales", "Profits", "Assets")]


#extracting the largest companies
#with respect to an alternative ordering
order_sales <- order(Forbes2000$Sales,decreasing = TRUE)

companies[order_sales[1:3]]

#The companies with assets of more than 1000 billion US dollars are
Forbes2000[Forbes2000$Assets > 1000, c("Company", "Sales","Profits", "Assets")]

#number of companies with assets more than 1000 B $
table(Forbes2000$Assets > 1000)

#subset of the Forbes 2000 list consisting of all companies based in
#the United Kingdom by
UKcomp <- subset(Forbes2000, Country == "United Kingdom")

dim(UKcomp)#number of companies based in the UK

summary(Forbes2000)

lapply(Forbes2000, summary)

#compare the profits in each of the 27 categories
#we first compute the median profit for each Sector from
mprofits <- tapply(Forbes2000$Profits, Forbes2000$Sector,median, na.rm = TRUE)

mprofits

#three categories with highest median profit
sort(mprofits,decreasing = TRUE)[1:3]

rev(sort(mprofits))[1:3]

#***********************     graphical analysis **********************
head(Forbes2000)


counts=table(Forbes2000$Country)
counts
barplot(counts,main = "Company Distribution",col="blue",xlab = "Countries")


layout(matrix(1:2, nrow = 2))#determining the output layout in plots area

hist(Forbes2000$Market.Value)#histogram of mareket value

hist(log(Forbes2000$Market.Value))#changing scales to logarithmic scale


#boxplot(log(data$Market.Value) ~ data$Country)

attach(Forbes2000)

plot(Market.Value ~ Sales, data = Forbes2000,pch = 16)

#changing scales to logarithmic scale
plot(log(Market.Value) ~ log(Sales), data = Forbes2000,pch =16)

data = subset(Forbes2000,Country %in% c("United Kingdom","Germany", "India"))
data



#***************************     The End       ****************************#
#**************************************************************************#