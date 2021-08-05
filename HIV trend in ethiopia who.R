library(ggplot2)

p1<-ggplot(hiv_data_unaids[hiv_data_unaids$Outcome=="New_hiv",], aes(x=as.factor(Year),y=Estimate, group=1))+geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.2,
                position=position_dodge(0.1)) +ylab("New HIV infections")+xlab("Year") +
  theme_classic()

p2<-ggplot(hiv_data_unaids[hiv_data_unaids$Outcome=="incidence",], aes(x=as.factor(Year),y=Estimate, group=1))+geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.2,
                position=position_dodge(0.1))+ylab("HIV incidence per 1000 population")+xlab("Year") + theme_classic()

p3<-ggplot(hiv_data_unaids[hiv_data_unaids$Outcome=="prevalence",], aes(x=as.factor(Year),y=Estimate, group=1))+geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.2,
                position=position_dodge(0.1))+ylab("People living with HIV")+xlab("Year") + theme_classic()

p4<-ggplot(hiv_data_unaids[hiv_data_unaids$Outcome=="death",], aes(x=as.factor(Year),y=Estimate, group=1))+geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.2,
                position=position_dodge(0.1))+ylab("AIDS-related deaths")+xlab("Year") + theme_classic()

library(patchwork)
pp<-p1+p2+p3+p4

ggsave("C:/Users/user/Dropbox (The University of Manchester)/Thesis_Belay/Fig_Fold/hiveth.png",pp)
