library(data.table)
library(reshape2)
library(ggplot2)
library(bit64)
library(dplyr)
library(randomForest)
library(miscTools)


#setwd("D:/junior/stat184/final_project/american-time-use-survey")
#data <- fread("atusact.csv")
#codes <- fread('codes.csv')
#idinfo <- fread("atuscps.csv")

# column teio1ocd has the occupation codes
# rest is found https://www.bls.gov/tus/census10ocodes.pdf and https://www.bls.gov/tus/atusintcodebk12.pdf
#jobinfo <- fread('atusresp.csv')

# Test jeffery data on model to predict sleep hours
testjeffery <- function(test, jeffery, model)
{
  levels(jeffery$gereg) <- levels(test$gereg)
  levels(jeffery$gestfips) <- levels(test$gestfips)
  levels(jeffery$hehousut) <- levels(test$hehousut)
  levels(jeffery$wokeupearly) <- levels(test$wokeupearly)

  jeffery_sleep <- predict(model, jeffery)
  return(jeffery_sleep)
}


# random forest model train and test
randomrestmodel <- function(train, test)
{
  cols <- names(train)[1:7]
  traincols = c("gereg", "gestfips", "hehousut", "prtage", "wokeupearly", "average_end_hour")
  clf <- randomForest(tuactdur24 ~ ., data=train[,..cols], ntree=20)
  r2 <- rSquared(test$tuactdur24, test$tuactdur24 - predict(clf, test[,..cols]))
  mse <- mean((test$tuactdur24 - predict(clf, test[,..cols]))^2)
  print(paste("r2 is:", r2, "mse is:", mse))
  
  print("graphing some data about the random forest")
  c <- ggplot(aes(x=actual, y=pred),
              data=data.frame(actual=test$tuactdur24, pred=predict(clf, test[,..cols])))
  c <- c + geom_point() +
    geom_abline(color="red") +
    ggtitle("RandomForest Regression analysis") + theme(plot.title = element_text(hjust = 0.5))
  c <- c + labs(caption = paste("INFO: This is the graph to show our results of actual vs predicted."))
  
  ggsave("randomforest.pdf", plot = c)
  plot(c)
  return(clf)
}


# helper function to create new variable
getendtime <- function(string)
{
  time <- substr(string, start = 1, stop = 2)
  time <- strtoi(time, base = 0L)
  return(time)
}

# taking in only the data we need for machine learning. We will also create some of our own data columns
datacleaning <- function(testdata, idinfo)
{
  x <- testdata[which(testdata$activity == "Sleeping")]
  #setnames(idinfo, c("name"), c("trcodep", "activity"))
  x<-merge(x,idinfo[,c("tucaseid", "gereg", "gestfips", "hehousut", "prtage")],all.x=T)
  x <- select(x, tucaseid, gereg, gestfips, hehousut, prtage, tustarttim, tuactdur24, tustoptime)
  x_mean <- dcast(x, tucaseid~., mean, na.rm = T, value.var = "tuactdur24")
  x$endhour = getendtime(x$tustoptime)
  x_endhourmean <- dcast(x, tucaseid~., mean, na.rm = T, value.var = "endhour")
  x <- merge(x, x_mean[,c("tucaseid", ".")], all.x = T )
  setnames(x, c("."), c("Average_Time"))
  x <- merge(x, x_endhourmean[,c("tucaseid", ".")], all.x = T )
  setnames(x, c("."), c("average_end_hour"))
  
  
  print("Create new boolean column (3rd one) if they woke up before 7am on average")
  
  x <- x[!duplicated(x$tucaseid),]
  x$wokeupearly = x$average_end_hour < 7.1
  x <- x[complete.cases(x), ]
  
  x <- select(x, gereg, gestfips, hehousut, prtage, tuactdur24, wokeupearly, average_end_hour)
  x$gereg = factor(x$gereg)
  x$gestfips = factor(x$gestfips)
  x$hehousut = factor(x$hehousut)
  x$wokeupearly = factor(x$wokeupearly)
  
  return (x)
}


#preparing data for first graph
datacleaning1 <- function(testdata){
  totaltime <- dcast(testdata, activity~., sum, na.rm = T, value.var="tuactdur24")
  setnames(totaltime, c("."), c("Total_Time"))
  totaltime <- totaltime[order(-totaltime$Total_Time),]
  return(totaltime)
}


# Formats the data to get the most popular activities for each job id 
datacleaningjobid <- function(testdata, jobidnumber){
  x <- testdata[which(testdata$jobid == jobidnumber)]
  numunique <- length(unique(x$tucaseid))
  rows <- nrow(x)
  print(paste("Number of data rows for", jobidnumber, "is:", rows))
  x <- dcast(x, activity~., sum, na.rm = T, value.var="tuactdur24")
  setnames(x, c("."), c("Total_Time"))
  x <- x[order(-x$Total_Time),]
  
  print("Calculating a new column variable, number of hours per person on average")
  x$hoursperperson = x$Total_Time / (numunique * 60)
  return (x)
}

# Formats the data to get the most popular activities for each industry
datacleaningindustry<- function(testdata, industryidcode)
{
  x <- testdata[which(testdata$industryid == industryidcode)]
  numunique <- length(unique(x$tucaseid))
  rows <- nrow(x)
  print(paste("Number of data rows for", industryidcode , "is:", rows))
  x <- dcast(x, activity~., sum, na.rm = T, value.var="tuactdur24")
  setnames(x, c("."), c("Total_Time"))
  x <- x[order(-x$Total_Time),]
  
  print("Calculating a new column variable, number of hours per person on average")
  x$hoursperperson = x$Total_Time / (numunique * 60)
  return (x)
}


# merge the occupations to the data dataframe
mergeoccupation<-function(datacopy, codescopy){
  setnames(codescopy, c("code", "name"), c("trcodep", "activity"))
  datacopy<-merge(datacopy,codescopy[,c("trcodep", "activity")],all.x=T)
  return(datacopy)
}

# merge the job to the data dataframe
# merge industry data to the dataframe
mergejob <- function(testdata, jobinfocopy){
  setnames(jobinfocopy, c("teio1ocd", "teio1icd"), c("jobid", "industryid"))
  testdata<-merge(x = testdata, y = jobinfocopy[,c("tucaseid", "jobid", "industryid")], 
                  by.x = 'tucaseid', by.y = 'tucaseid',all = T)
  
  return(testdata)
}


# first graph function, graphs total time spend on activities
graphtotaltime <- function(totaltime)
{
  c <- ggplot(head(totaltime, 10),aes(x=activity,y=Total_Time))+geom_col()+
    ggtitle("Total Time spend on Activities in Minutes (Top 10)")+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Activity")+ylab("Time in minutes across all respondents")
  c <- c + scale_x_discrete(labels = c("Eating and drinking" = "Food", 
                                       "Socializing and communicating with others" = "Socializing", 
                                       "Television and movies (not religious)" = "TV",
                                       "Washing, dressing and grooming oneself" = "Showering", 
                                       "Food and drink preparation" = "Cooking", "Interior cleaning" = "Cleaning", 
                                       "Reading for personal interest" = "Reading", 
                                       "Relaxing, thinking" = "Relaxing"))
  #c + scale_x_discrete(labels = abbreviate)
  c <- c + labs(caption = "INFO: The top 10 time spend on activites for all the respondents. Sleeping is the most by far")
  print("Plotting first graph...")
  plot(c)
  ggsave("graph1.pdf", plot = c)
  print(c)
  
}

# graphing total time spent on activities depending on job
graphjob <- function(jobtimedata, jobstitle)
{
  
  c <- ggplot(head(jobtimedata, 10),aes(x=activity,y=hoursperperson))+geom_col()+
    ggtitle(paste("Total Time spent on Activities in Hours (Top 10) for", jobstitle))+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Activity")+ylab("Time in hours across all respondents")
  c <- c + scale_x_discrete(labels = c("Eating and drinking" = "Food", 
                                       "Socializing and communicating with others" = "Socializing", 
                                       "Television and movies (not religious)" = "TV",
                                       "Washing, dressing and grooming oneself" = "Showering", 
                                       "Food and drink preparation" = "Cooking", "Interior cleaning" = "Cleaning", 
                                       "Reading for personal interest" = "Reading", 
                                       "Relaxing, thinking" = "Relaxing", 
                                       "Computer use for leisure (exc. Games)" = "Gaming", 
                                       "Shopping, except groceries, food and gas" = "Shopping", 
                                       "Travel related to working" = "Work-related Travel"
                                       ))
  #c + scale_x_discrete(labels = abbreviate)
  c <- c + labs(caption = paste("INFO: The top 10 time spend on activites for", jobstitle,"Numbers may look weird due to weekends."))
  print(paste("plotting second graph about", jobstitle))
  ggsave(paste(jobstitle, ".pdf", sep = ""), plot = c)
  plot(c)
}

graphindustry <- function(industrydata, industryname)
{
  c <- ggplot(head(industrydata, 10),aes(x=activity,y=hoursperperson))+geom_col()+
    ggtitle(paste("Total Time spent on Activities in Hours (Top 10) in industry", industryname))+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Activity")+ylab("Time in hours across all respondents")
  c <- c + scale_x_discrete(labels = c("Eating and drinking" = "Food", 
                                       "Socializing and communicating with others" = "Socializing", 
                                       "Television and movies (not religious)" = "TV",
                                       "Washing, dressing and grooming oneself" = "Showering", 
                                       "Food and drink preparation" = "Cooking", "Interior cleaning" = "Cleaning", 
                                       "Reading for personal interest" = "Reading", 
                                       "Relaxing, thinking" = "Relaxing", 
                                       "Computer use for leisure (exc. Games)" = "Gaming", 
                                       "Shopping, except groceries, food and gas" = "Shopping", 
                                       "Travel related to working" = "Work-related Travel"
  ))
  #c + scale_x_discrete(labels = abbreviate)
  c <- c + labs(caption = paste("INFO: The top 10 time spend on activites for", industryname,"Numbers may look weird due to weekends."))
  print(paste("plotting second graph about", industryname))
  ggsave(paste(industryname, ".pdf", sep = ""), plot = c)
  plot(c)
}

main<-function(){
  # data from kaggle https://www.kaggle.com/bls/american-time-use-survey
  setwd("D:/junior/stat184/final_project/american-time-use-survey")
  print("Reading data from working directory")
  data <- fread("atusact.csv")
  codes <- fread('codes.csv')
  idinfo <- fread("atuscps.csv")
  # column teio1ocd has the occupation codes
  # rest is found https://www.bls.gov/tus/census10ocodes.pdf and https://www.bls.gov/tus/atusintcodebk12.pdf
  jobinfo <- fread('atusresp.csv')
  
  # creating copies
  datacopy <- data
  codescopy <- codes
  idinfocopy <- idinfo
  jobinfocopy <- jobinfo
  
  print("merging the data")
  testdata = mergeoccupation(datacopy, codescopy)
  
  print("preparing data for graph")
  totaltime = datacleaning1(testdata)
  
  print("Graphing total time")
  graphtotaltime(totaltime)
  
  print("merging job data")
  testdata = mergejob(testdata, jobinfocopy)
  
  print("--------------------------")
  print("data preprocessing for next graph (job = software development)")
  softwaredevelopment = datacleaningjobid(testdata, 1020)
  print("graph average time spent for software developers")
  graphjob(softwaredevelopment, "Software Developers")
  print("ANALYSIS:")
  print("Software developers have relatively the same top 10")
  print("as everyone else but computer gaming enters the top 10")
  
  print("-------------------------")
  print("data preprocessing for next graph (job = lawyers)")
  lawyers = datacleaningjobid(testdata, 2100)
  print("graph average time spent for lawyers")
  graphjob(lawyers, "Lawyers")
  print("ANALYSIS")
  print("Graph averages for sleeping and work are again similiar to Software developers")
  print("Computer Gaming is gone but travel related to work has shown up in the top 10")

  # next do by industry graphs, make another variable while doing that and then do random forest
  # then we are done. 
  

  print("-------------------------")
  print("data preprocessing for next graph (industry = Air Transportation")
  air = datacleaningindustry(testdata, 6070)
  print("graph average time spent for people in air transportation")
  graphindustry(air, "Air Transportation")
  print("Analysis")
  print("Nothing new here, Everything looks comparable to the averages for Software developers")
  print("and Lawyers, We will check it with another industry maybe to see changes.")
  
  print("-------------------------")
  print("data preprocessing for next graph (industry = Banking")
  banking = datacleaningindustry(testdata, 6870)
  print("graph average time spent for people in banking")
  graphindustry(banking, "Banking")
  print("Analysis")
  print("Surprising that Air Transportation people sleep more on average than Bankers")
  print("But the top 10 activities done looks the same for bankers and Air transportation")
  print("FINAL ANALYSIS")
  print("People from all industries and jobs seem to spend roughly the same amount of time on")
  print("their top 10 activities. Sleeping is the most and then Work. The only deviation was that")
  print("Software developers spend mroe time playing video games/computer games")
  
  
  print("-------------------------")
  print("-------------------------")
  print("FINAL PART")
  
  print("Random Forest Regressor for predicting sleep based on various factors")
  print("Data preprocessing for random forest")
  MLData = datacleaning(testdata, idinfo)
  print("Done preprocessing ml data, we will use 6 column variables to predict average time slept")
  print("Split to train test samples with 30/70 split")
  smp_size <- floor(0.70 * nrow(MLData[1:10000,]))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(MLData)), size = smp_size)
  train <- MLData[train_ind, ]
  test <- MLData[-train_ind, ]
  
  print("-------------------------")
  print("Train random forest model.")
  model = randomrestmodel(train, test)
  print("It isn't too accurate but I only used 10000 rows instead of the 100000+ due to trianing speed")
  print("If we used more data it may become a lot better, but it also didn't perform too amazing due to ")
  print("The random/high variance nature of the data and we didn't use too many features")
  print("Next I will just do a fun prediction on my sleep hours in the future")
  
  jeffery <- data.frame("gereg" = "1", "gestfips" = "42", "hehousut" = "1", "prtage" = as.integer(22), "wokeupearly" = "FALSE", "average_end_hour" = 8.0)
  jeffery_sleep <- testjeffery(test, jeffery, model)
  print("Jeffery dataset is: ")
  print(jeffery)
  print(paste("Jeffery predicted hours of sleep is:", jeffery_sleep))
  print("Thats not very good.")
  
  print("Finally all the graphs should be plotted or have been saved in the working directory")
}

main()

