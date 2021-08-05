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

p4<-ggplot(sa_cov_dat, aes(x=date2, y=Daily_tests,group=1))+geom_point()+xlab("Date")+ylab("Daily tests")+theme_bw()

p1/p4
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
  labs(y = "First Derivative", title = "Estimated First Derivatives")+theme_classic()

postive_rate_plot<-ggplot(sa_cov_dat, aes(x=date2, y= Dialy_cases/Daily_tests,group=1))+geom_point()+xlab("Date")+ylab("Postive test")+theme_classic()

postive_rate_plot/profile_plot/derivative_plot# work on getting dates right 


# Use another model for the trend - random walk for example 

library(INLA)

sa_cov_dat<-sa_cov_dat[sa_cov_dat$date2>'2020-03-09'&!is.na(sa_cov_dat$cumulative_tests),]
sa_cov_dat<-sa_cov_dat[sa_cov_dat$date2<'2021-02-28',]#temp

U <- 0.5#1

hyper.prec <- list(theta = list(prior = "pc.prec", param = c(U, 0.75)))#0.01

formula_rw2<-Dialy_cases ~1 + f(time, model = "rw2", scale.model = TRUE, 
        hyper = hyper.prec)

bin.rw2 <- inla(formula_rw2,                   
               data = sa_cov_dat,
               family = "betabinomial",
               Ntrials=Daily_tests,
               control.family=list(link='logit'),
               control.predictor = list(compute = TRUE),
               control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))
summary(bin.rw2)

newdata_rw2<-cbind(sa_cov_dat[,c("date2","Dialy_cases","Daily_tests","time")],bin.rw2$summary.fitted.values)
newdata_rw2 <- reshape:::rename(newdata_rw2, c("0.025quant"="lower", "0.975quant"="upper"))

newdata_rw2<-newdata_rw2%>%
  mutate(Postive_testing=Dialy_cases/Daily_tests,
         mean_prop=mean/Daily_tests,
         mean_L=lower/Daily_tests,
         mean_U=upper/Daily_tests)

p_rw2_inc<-ggplot(newdata_rw2, aes(y=mean, x=date2)) +
  geom_blank()+
  geom_point(aes(y=Dialy_cases, x=date2)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='blue', alpha=0.6) +
  geom_line(aes(y=mean, x=date2)) +xlab("Date")+ylab("Observed and fitted cases")+
  labs(title = "A" )+
  theme_bw()

p_rw2_prop<-ggplot(newdata_rw2, aes(y=mean, x=date2)) +
  geom_blank()+
  geom_point(aes(y=I(Dialy_cases/Daily_tests), x=date2)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='blue', alpha=0.6) +
  geom_line(aes(y=mean, x=date2)) +xlab("Date")+ylab("Positive testing rate")+
  geom_hline(yintercept = 0.05, linetype = "dashed", color="red",size=1)+
  labs(title = "A")+
  theme_bw()


newdata_rw2$deriv<-D1ss(x=newdata_rw2$time,y=newdata_rw2$mean, spar.off=0.0)
newdata_rw2$L_deriv<-D1ss(x=newdata_rw2$time,y=newdata_rw2$lower, spar.off=0.0)
newdata_rw2$U_deriv<-D1ss(x=newdata_rw2$time,y=newdata_rw2$upper, spar.off=0.0)


p_rw2_deriv_new<-ggplot(newdata_rw2, aes(y=deriv, x=date2)) +
  geom_blank()+
  geom_ribbon(aes(ymin=L_deriv, ymax=U_deriv), fill='blue', alpha=0.6) +
  geom_line(aes(y=deriv, x=date2)) +xlab("Date")+ylab("Derivative")+
  geom_hline(yintercept = 0, linetype = "dashed", color="red",size=1)+
  geom_vline(xintercept = as.numeric(newdata_rw2$date2[310]), linetype = "twodash", color="pink4",size=1)+
  geom_vline(xintercept = as.numeric(newdata_rw2$date2[230]), linetype = "twodash", color="pink4",size=1)+
  geom_vline(xintercept = as.numeric(newdata_rw2$date2[153]), linetype = "twodash", color="pink4",size=1)+
  labs(title = "B")+
  theme_bw()

ppnew<-p_rw2_prop/p_rw2_deriv_new
ggsave("C:/Users/user/Dropbox (The University of Manchester)/Thesis_Belay/Fig_Fold/testingplot.png",ppnew)
