library(data.table)
library(reshape2)
library(ggplot2)
library(bit64)

setwd("D:/junior/stat184/final_project/american-time-use-survey")
data <- fread("atusact.csv")
codes <- fread('codes.csv')
idinfo <- fread("atuscps.csv")

# column teio1ocd has the occupation codes
# rest is found https://www.bls.gov/tus/census10ocodes.pdf and https://www.bls.gov/tus/atusintcodebk12.pdf
jobinfo <- fread('atusresp.csv')


#preparing data for first graph
datacleaning1 <- function(testdata){
  totaltime <- dcast(testdata, activity~., sum, na.rm = T, value.var="tuactdur24")
  setnames(totaltime, c("."), c("Total_Time"))
  totaltime <- totaltime[order(-totaltime$Total_Time),]
  return(totaltime)
}

# TODO
# Remove -1 users
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
  plot(c)
}



main<-function(){
  # data from kaggle https://www.kaggle.com/bls/american-time-use-survey
  setwd("D:/junior/stat184/final_project/american-time-use-survey")
  #data <- fread("atusact.csv")
  codes <- fread('codes.csv')
  #idinfo <- fread("atuscps.csv")
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
  print("")
  print("ANALYSIS: Software developers have relatively the same top 10")
  print("as everyone else but computer gaming enters the top 10")
  
  print("-------------------------")
  print("data preprocessing for next graph (job = lawyers)")
  lawyers = datacleaningjobid(testdata, 2100)
  print("graph average time spend for lawyers")
  graphjob(lawyers, "Lawyers")
  print("")
  print("ANALYSIS: Graph averages for sleeping and work are again similiar to Software developers")
  print("Computer Gaming is gone but travel related to work has shown up in the top 10")

  # next do by industry graphs, make another variable while doing that and then do random forest
  # then we are done. 
  
  return(testdata)
}



