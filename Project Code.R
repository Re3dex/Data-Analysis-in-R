#STAT 206 Final Project
#Name: Ammar Khalid
#Roll No.: 241547086


### Creating the complete Dataset ### 
#1. Installed readxl and Rcpp to view xlsx files
#2. Opening the xlsx files
#Had to rename the column names as some had different capitalizations and rbind did not accept that.
library(readxl)
Counties_Data_Set_1<-read_excel("Counties Data Set 1.xlsx")
Counties_Data_Set_2<-read_excel("Counties Data Set 2.xlsx")
Counties_Data_Set_3<-read_excel("Counties Data Set 1.xlsx")
#3. Binding all the data into one variable
original_data<-rbind(Counties_Data_Set_1,Counties_Data_Set_2,Counties_Data_Set_3)
mydata<-rbind(Counties_Data_Set_1,Counties_Data_Set_2,Counties_Data_Set_3)
attach(mydata)
### Calculating percentage of missing data for each column and replacing the NAs ###
(colMeans(is.na(mydata)))*100 #Prints out the percentage of NAs in each column

#The percentage missing data is relatively low, just under 2%.
#The Best way to deal with this missing Data would be to replace its NAs with average.
mydata$Age75[is.na(Age75)]=mean(Age75,na.rm=TRUE)
mydata$Crime[is.na(Crime)]=mean(Crime,na.rm=TRUE)
mydata$Income[is.na(Income)]=mean(Income,na.rm=TRUE)
mydata$Democrat[is.na(Democrat)]=mean(Democrat,na.rm=TRUE)
mydata$Republican[is.na(Republican)]=mean(Republican,na.rm=TRUE)
mydata$Turnout[is.na(Turnout)]=mean(Turnout,na.rm=TRUE)

(colMeans(is.na(mydata)))*100 #Prints out the percentage of NAs in each column

### Checking and Removing the Outliers in the data ###

#I will be using a boxplot to check for outliers and any clear outliers will be replaced with the average of the column to avoid other data from being skued. 
#Non-Apparent outliers will be kept in.

#Pop
boxplot(mydata$Pop,horizontal=TRUE)
#Outliers exist above 3x10^6.
mean1<-mean(Pop)
mydata$Pop[mydata$Pop>3000000]<-mean1


#Pop.Density
boxplot(mydata$Pop.Density,horizontal=TRUE)
#Outlier values seem to exist above 15000.
mean2<-mean(Pop.Density)
mydata$Pop.Density[mydata$Pop.Density>15000]<-mean2

#Pop.Change
boxplot(mydata$Pop.Change,horizontal=TRUE)
#Outliers exist above 150
mean3<-mean(mydata$Pop.Change)
mydata$Pop.Change[mydata$Pop.Change>150]<-mean3


#Farm
boxplot(mydata$Farm,horizontal=TRUE)
#Outlier values exist above 60.
mean4<-mean(mydata$Farm)
mydata$Farm[mydata$Farm>60]<-mean4

#Age65-74
boxplot(mydata$`Age65-74`,horizontal=TRUE)
#No Apparent outliers.

#Age75
boxplot(mydata$Age75,horizontal=TRUE)
#Outliers exist below 1.
mean5<-mean(mydata$Age75)
mydata$Age75[mydata$Age75<1]<-mean5

#Crime
boxplot(mydata$Crime,horizontal=TRUE)
#Outliers exist above 15000.
mean6<-mean(mydata$Crime)
mydata$Crime[mydata$Crime>15000]<-mean6

#College
boxplot(College,horizontal=TRUE)
#Outliers not apparent.

#Income
boxplot(mydata$Income,horizontal=TRUE)
#Outliers exist below 15000
mean7<-mean(mydata$Income)
mydata$Income[mydata$Income<15000]<-mean7


#Democrat
boxplot(mydata$Democrat,horizontal=TRUE)
#Outliers are below 20 and above 80
mean8<-mean(mydata$Democrat)
mydata$Democrat[mydata$Democrat<20]<-mean8
mydata$Democrat[mydata$Democrat>80]<-mean8

#Republican
boxplot(Republican,horizontal=TRUE)
#No Apparent Outliers

#White
boxplot(White,horizontal=TRUE)
#No Apparent Outliers

#Black
boxplot(Black,horizontal=TRUE)
#No apparent outliers

#Turnout
boxplot(mydata$Turnout,horizontal=TRUE)
#Outliers are below 20 and above 80.
mean9<-mean(mydata$Turnout)
mydata$Turnout[mydata$Turnout<20]<-mean9
mydata$Turnout[mydata$Turnout>80]<-mean9

### Calculating the Summary ###
#Minimum
my_min<-function(x){
  a<-x[1]
  for (i in x){
  if (i<a){a<-i}}
return(a)
}

#Maximum
my_max<-function(x){
  a<-x[1]
  for (i in x){
    if (i>a){a<-i}}
  return(a)
}

#Mean
my_mean<-function(x){
  total<-0
  for (i in x){total<-total+i}
  l<-length(x)
  meen<-total/l
  return(meen)}

#Median
my_median<-function(x){
  sorted<-sort(x)
  l<-length(x)
  med_l<-l/2
  if (med_l%%1!=0){
    a<-sorted[ceiling(med_l)]
    return(a)
  }else{
    a<-sorted[med_l]
    b<-sorted[med_l+1]
    c<-(a+b)/2
    return(c)
  }
} 

#Standard Deviation
my_sd<-function(x){
  a<-sqrt(sum((x-mean(x))^2/(length(x)-1)))
  return(a)
}


my_summary<-function(x){
  min<-my_min(x)
  max<-my_max(x)
  mean<-my_mean(x)
  median<-my_median(x)
  sd<-my_sd(x)
  result<-list(min,max,mean,median,sd)
  return(result)
}

### Performing the Data analysis ###

`Population Density`<-as.numeric(my_summary(mydata$Pop.Density))
Population<-as.numeric(my_summary(mydata$Pop))
`Population Change`<-as.numeric(my_summary(mydata$Pop.Change))
`Age 65-74`<-as.numeric(my_summary(mydata$`Age65-74`))
`Age 75`<-as.numeric(my_summary(mydata$Age75))
Crime<-as.numeric(my_summary(mydata$Crime))
College<-as.numeric(my_summary(mydata$College))
Income<-as.numeric(my_summary(mydata$Income))
Farm<-as.numeric(my_summary(mydata$Farm))
Democrat<-as.numeric(my_summary(mydata$Democrat))
Republican<-as.numeric(my_summary(mydata$Republican))
White<-as.numeric(my_summary(mydata$White))
Black<-as.numeric(my_summary(mydata$Black))
Turnout<-as.numeric(my_summary(mydata$Turnout))

library(data.table)

df<-data.frame(`Population Density`,Population,`Population Change`,`Age 65-74`,`Age 75`,Crime,College,Income,Farm,Democrat,Republican,White,Black,Turnout)
rownames(df) <- c("Min","Max","Mean","Median","SD")
df_table<-setDT(df)

### Barplots ###

my_summary2<-function(x){
  mean<-my_mean(x)
  sd<-my_sd(x)
  result<-list(mean,sd)
  return(result)
}
Pop.D<-as.numeric(my_summary2(mydata$Pop.Density))
Pop.C<-as.numeric(my_summary2(mydata$Pop.Change))
Democrats<-as.numeric(my_summary2(mydata$Democrat))
Republicans<-as.numeric(my_summary2(mydata$Republican))
White1<-as.numeric(my_summary2(mydata$White))
Black1<-as.numeric(my_summary2(mydata$Black))
Turnout1<-as.numeric(my_summary2(mydata$Turnout))

df2<-data.frame(Pop.D,Pop.C,Democrats,Republicans,White1,Black1,Turnout1)
rownames(df2)<-c('AVG','SD')

library(data.table)
df2_table<-setDT(df2)


# Grouped Bar Plot
barplot(t(as.matrix(df2)),beside=TRUE,legend=TRUE)
library(ggplot2)
# Option 1
ggplot(mydata, aes(x = State, y = White)) + 
  geom_bar(stat = "summary", fun = "mean")

ggplot(mydata, aes(x = State, y = Black)) + 
  geom_bar(stat = "summary", fun = "mean")

