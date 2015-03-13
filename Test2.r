# Title:  Second Part of the Worst Project
# Class:  The Worst Class on Coursera (Coursera.org)
# Name:   Philip Coyne


# Set working directory

setwd("E:/Users/Philip Coyne/Documents")

# Load packages in order to generate pdf documentation afterwards
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("RepReseOne.Rmd")
knit2html("RepReseOne.Rmd", "RepReseOne.html")
#markdownToHTML('RepReseOne.md', 'RepReseOne.html', options=c("use_xhml"))
system("pandoc -s RepReseOne.html -o RepReseOne.pdf")


#Part 1: Read in data file
stepData<-read.csv("activity.csv")

#Part 2: Calculate mean number of steps taken per day

##Determine number of unique dates
test<-unique(stepData$date)
length(test)


##Utilize a for loop to determine the number of steps per date

meanMedianStepData<-data.frame(matrix(ncol=4,nrow=length(test)))
meanMedianStepData[,1]<-test
meanMedianStepData[,1]<-as.Date(meanMedianStepData[,1])
for (i in 1:length(test)){

  data<-subset(stepData,test[i]==stepData$date)
  meanMedianStepData[i,2]<-mean(data$steps)
  meanMedianStepData[i,3]<-median(data$steps)
  meanMedianStepData[i,4]<-sum(data$steps)
}

colnames(meanMedianStepData)<-c("Date","Mean","Median","Total")


## Create histogram of date versus total number of steps
hist(meanMedianStepData$Total)

##Calculate and report the mean and median of the Total portion
##of meanMedianStepData
totalStepAvg<-mean(meanMedianStepData$Total,na.rm=TRUE)
totalStepMedian<-median(meanMedianStepData$Total,na.rm=TRUE)

#Part 3: What is the average activity pattern?

newTest<-unique(stepData$interval)

intervalStepData<-data.frame(matrix(ncol=2,nrow=length(newTest)))

intervalStepData[,1]<-newTest

for (i in 1:length(newTest)){
  
  data<-subset(stepData,newTest[i]==stepData$interval)
  intervalStepData[i,2]<-mean(data$steps,na.rm=TRUE)
  
}

##Determine maximum number of steps


plot(intervalStepData[,1],intervalStepData[,2],type="l")







#Part 4: Inputing missing values

##Determine the location of missing values and find how many
##are out there
indexMissing<-is.na(stepData$steps)
sum(indexMissing)

test<-unique(stepData$date)
length(test)

experimentalStepData<-data.frame(matrix(ncol=3,nrow=0))
colnames(experimentalStepData)<-c("steps","date","interval")

for(i in 1:length(test)){
  
  data<-subset(stepData,test[i]==stepData$date)
  conditionalMean<-mean(data$steps)
  doop<-is.na(data$steps)
  sum(doop)
  
  ##If the entire day is NA, set all steps to 0
  if(is.na(conditionalMean)==TRUE){
    data$steps<-0
  }
  
  
  experimentalStepData<-rbind(experimentalStepData,data)
  ##If not, find all NAs and substitute them with 
  ##conditional mean.  NOTE: IT WAS DETERMINED THAT THIS 
  ##DOES NOT OCCUR.  The only time NA is detected is when 
  ##an entire day is omitted
    
}

##Show that the new data set has the same amount of entries
##to the original data set
dim(experimentalStepData)
dim(stepData)

##Calculate the total number of steps taken each day.  Report
##mean and median.  

experimentalMeanStepData<-data.frame(matrix(ncol=2,nrow=length(test)))
experimentalMeanStepData[,1]<-test
experimentalMeanStepData[,1]<-as.Date(experimentalMeanStepData[,1])



for (i in 1:length(test)){
  
  data<-subset(experimentalStepData,test[i]==experimentalStepData$date)
  experimentalMeanStepData[i,2]<-sum(data$steps)
}

colnames(experimentalMeanStepData)<-c("Date","Total")


##Create histogram of the total number of steps. Compare to 
##the previous histogram in the first part of the assignment
hist(experimentalMeanStepData$Total)
hist(meanMedianStepData$Total)

mean(experimentalMeanStepData$Total)
median(experimentalMeanStepData$Total)
#Part 5: Are there differences in activity patterns between
#weekday and weekends?

#Use weekdays() function to create factor variable

newESD<-experimentalStepData
newESD$date<-as.Date(newESD$date)
newESDColumn<-weekdays(newESD$date)
newESD["Weekday"]<-newESDColumn



newTest<-unique(newESD$interval)

newIntESD<-data.frame(matrix(ncol=2,nrow=length(newTest)))

newIntESD[,1]<-newTest

newWeekDayIntESD<-newIntESD
newWeekEndIntESD<-newIntESD 

##Sort Weekday and Weekend dates

weekdayIntESD<-data.frame(matrix(ncol=4,nrow=0))
weekendIntESD<-data.frame(matrix(ncol=4,nrow=0))

colnames(weekdayIntESD)<-c("steps","date","interval","Weekday")
colnames(weekendIntESD)<-c("steps","date","interval","Weekday")

sunday<-subset(newESD,newESD$Weekday=="Sunday")  
saturday<-subset(newESD,newESD$Weekday=="Saturday")  
monday<-subset(newESD,newESD$Weekday=="Monday")  
tuesday<-subset(newESD,newESD$Weekday=="Tuesday")  
wednesday<-subset(newESD,newESD$Weekday=="Wednesday")  
thursday<-subset(newESD,newESD$Weekday=="Thursday")  
friday<-subset(newESD,newESD$Weekday=="Friday")  

weekdayIntESD<-rbind(monday,tuesday,wednesday,thursday,friday)
weekendIntESD<-rbind(sunday,saturday)


##Weekday Intervals 
for (i in 1:length(newTest)){
  
  data<-subset(weekdayIntESD,newTest[i]==weekdayIntESD$interval)
  newWeekDayIntESD[i,2]<-mean(data$steps)
  
}


##Weekend Intervals

for (i in 1:length(newTest)){
  
  data<-subset(weekendIntESD,newTest[i]==weekendIntESD$interval)
  newWeekEndIntESD[i,2]<-mean(data$steps)
  
}



##Display panel plot

par(mfrow=c(2,1))

plot(newWeekEndIntESD[,1],newWeekEndIntESD[,2],type="l")

plot(newWeekDayIntESD[,1],newWeekDayIntESD[,2],type="l")





