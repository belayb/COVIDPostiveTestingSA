#' This script loads data from git.hub -- repo 
#' We want to model the postive testing rate 
#' We will use a binomial model or beta-binomial model 
#' to capture the trend we will use smoothing or ar1 or rw1 model
#' Model fitting will be based on stan  

#'---------------Load packages 
library(ggplot2)
library(visreg)
library(brms)
#'library(visibly)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(patchwork)
library(lubridate)

#'------------- Load data 
tests_sa <- read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv",header=TRUE)
cases_sa <- read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv",header=TRUE)
head(tests_sa)
head(cases_sa)
#'------------- Prepare the two data sets to merge  
tests_sa<-tests_sa%>%mutate(date2=lubridate::ymd(YYYYMMDD))
cases_sa<-cases_sa%>%mutate(date2=lubridate::ymd(YYYYMMDD))

variable_keep_test<-c("date2","cumulative_tests")
variable_keep_cases<-c("date2","total")

sa_cov_dat<-left_join(cases_sa[,variable_keep_cases], tests_sa[,variable_keep_test], by="date2")
head(sa_cov_dat)
dim(sa_cov_dat[is.na(sa_cov_dat$cumulative_tests),])# There are unknown number of tests in the first 4 days 05,08,09,10-03-2020 
# sa_cov_dat[18:35,] also unknown number of tests on 25-03, and 07-04 
sa_cov_dat<-sa_cov_dat[sa_cov_dat$date2>'2020-03-09'&!is.na(sa_cov_dat$cumulative_tests),]


# You need to start from 2020-03-12 in order to properly calaculate daily cases and tests
# Compute daily cases and tests 
sa_cov_dat<-sa_cov_dat%>%mutate(Dialy_cases=-1*(lag(total)-total), Daily_tests=-1*(lag(cumulative_tests)-cumulative_tests))
sa_cov_dat<-sa_cov_dat[-1, ]
#Plot of observed data- cummulative cases, daily cases, cummulative tests 

p1<-ggplot(sa_cov_dat, aes(x=date2, y=Dialy_cases,group=1))+geom_point()+xlab("Date")+ylab("Daily cases")+theme_bw()

p2<-ggplot(sa_cov_dat, aes(x=date2, y=total,group=1))+geom_line(size = 1.5)+xlab("Date")+ylab("Cummulative cases")+theme_bw()


p3<-ggplot(sa_cov_dat, aes(x=date2, y=cumulative_tests,group=1))+geom_line(size = 1.5)+xlab("Date")+ylab("Cummulative tests")+theme_bw()

p1_3<-p1/p2/p3


# crate a new process time column from date2

sa_cov_dat<-sa_cov_dat%>%mutate(time=cumsum(c(0,diff.Date(date2))))





