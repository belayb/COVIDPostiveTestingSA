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


#----------------- crate a new process time column from date2

sa_cov_dat<-sa_cov_dat%>%mutate(time=cumsum(c(0,diff.Date(date2))))
head(sa_cov_dat)

fit1<- brm(bf(Dialy_cases|trials(Daily_tests) ~ s(time)),
                data = sa_cov_dat, family = binomial(), cores = 4, seed = 17,
                iter = 4000, warmup = 1000, thin = 10, refresh = 0,
                control = list(adapt_delta = 0.99))


#----------------- get the profile plot - liner predictor 
len = 500
x1_new <- seq(min(sa_cov_dat$time), max(sa_cov_dat$time), length.out = len)
newdata<-data.frame(x1_new, rep(5, 100))
names(newdata)<-c('time','Daily_tests')

predictions <- brms::posterior_linpred(fit1, newdata = newdata, summary = FALSE)
means <- apply(predictions, MARGIN = 2,mean)
lower <- apply(predictions, MARGIN = 2,quantile, prob = 0.055)
upper <- apply(predictions, MARGIN = 2,quantile, prob = 0.945)

profile_plot<-data.frame(means) %>%
  cbind(lower) %>%
  cbind(upper) %>%
  cbind(newdata) %>%
  ggplot(.,aes(time, means)) + geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+theme_bw()

# --------------- get the profile plot on the response scale 

#---------------- get the first derivative 
len = 500
epsilon <- 1e-6
x1_new <- seq(min(sa_cov_dat$time), max(sa_cov_dat$time), length.out = len)
first<-data.frame(x1_new, rep(1, len))
names(first)<-c('time','Daily_tests')

second <- first %>%
  mutate(time = time + epsilon)

## get predictions
first_preds <-
  posterior_linpred(fit1,
                    newdata = first,
                    summary = FALSE,)
second_preds <-
  posterior_linpred(fit1,
                    newdata = second,
                    summary = FALSE)


## Calcualte the differennce and divide by epsilon - this is analgous to the dx/dt 
diff <- (second_preds - first_preds) / epsilon


## using the posterior samples, we calculate the mean and lower and upper quantiles
mean_finite_diff  <- apply(diff, MARGIN = 2, FUN = mean)
lower_finite_diff <- apply(diff, MARGIN = 2, FUN = quantile,prob = 0.025)
upper_finite_diff <- apply(diff, MARGIN = 2, FUN = quantile,prob = 0.975)

derivative_plot<-data.frame(mean_finite_diff) %>%
  cbind(lower_finite_diff) %>%
  cbind(upper_finite_diff) %>%
  cbind(first) %>%
  ggplot(.,aes(time, mean_finite_diff)) + geom_line()  +
  geom_ribbon(aes(ymin = lower_finite_diff, ymax = upper_finite_diff), alpha = 0.1) + 
  labs(y = "First Derivative", title = "Estimated First Derivatives")+theme_bw()

postive_rate_plot<-ggplot(sa_cov_dat, aes(x=date2, y=total/Daily_tests,group=1))+geom_point()+xlab("Date")+ylab("Postive test")+theme_bw()

postive_rate_plot/profile_plot/derivative_plot
