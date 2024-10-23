# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(lme4)
library(broom.mixed)

#read in metals and speciated As data
metalsR1R6_q<- read.csv("../results/metalsR1R6_q.csv")
As_IOR1R6_q<- read.csv("../results/As_IOR1R6_q.csv")

#get data ready for regressions
#### regressions ####
#baseline df
metalsR1R6_q<-metalsR1R6_q %>% 
  #remove metals with <60% DF
  filter(metal!="Sn") %>% filter(metal!="Sb") %>% filter(metal!="W") %>% filter(metal!="Be") %>% 
  filter(Dept4!="Cabanas"&Dept4!="Cuscatlan"&Dept4!="SantaAna") %>% 
  mutate(PestYN=ifelse(is.na(PestYN),2,PestYN))

# subset to people in both rounds #
metalsR1andR6_q<-metalsR1R6_q %>% 
  group_by(MANOS_ID) %>%
  filter(n_distinct(round) > 1) %>%
  #remove metals with <60% DF
  filter(metal!="Sn") %>% filter(metal!="Sb") %>% filter(metal!="W") %>% filter(metal!="Be") %>%
  filter(Dept4!="Cabanas"&Dept4!="Cuscatlan"&Dept4!="SantaAna") %>% 
  ungroup() %>% 
  mutate(SeafoodAnyYN=ifelse(is.na(SeafoodAnyYN),2,SeafoodAnyYN),
         SeafoodAnyYN=as.factor(SeafoodAnyYN))

# baseline df
As_IOR1_q<-As_IOR1R6_q %>% 
  filter(round==1) %>% 
  mutate(d_salvador=ifelse(Country=="El Salvador",1,0),
         d_nicaragua=ifelse(Country=="Nicaragua",1,0)) %>% 
  filter(Dept4!="Cabanas"&Dept4!="Cuscatlan"&Dept4!="SantaAna") %>% 
  mutate(PestYN=ifelse(is.na(PestYN),2,PestYN))

# R1R6 df
As_IOR1andR6_q<-As_IOR1R6_q %>% 
  group_by(MANOS_ID) %>%
  filter(n_distinct(round) > 1) %>%
  ungroup() %>% 
  filter(Dept4!="Cabanas"&Dept4!="Cuscatlan"&Dept4!="SantaAna") %>% 
  mutate(SeafoodAnyYN=ifelse(is.na(SeafoodAnyYN),2,SeafoodAnyYN))

# metal regression models ####
#baseline df
Dept4_class <- c("Usulutan","Leon","Chinandega","Sonsonate","SanSalvador","Ahuachapan","Other",
                 "LaLibertad")
worksite_class<- c("AZ1","AZ2","AZ5","AZ7","MA3","PL1","PL9","LA8","CO4")

##### inorganic As ####
#baseline df
MetiAs<-As_IOR1_q %>%
  filter(species=="Inorganic"&!is.na(As_osmadj)) %>% 
  mutate(worksite=as.factor(worksite)) %>% 
  select(MANOS_ID,round,country,Age,PestYN,worksite,As_osmadj,Dept4,BMI)

# R1R6 df
MetiAs6<- As_IOR1andR6_q %>% 
  filter(species=="Inorganic"&!is.na(As_osmadj)) %>% 
  filter(worksite!="PL1"&worksite!="PL9"&worksite!="LA8") %>% 
  mutate(worksite=as.factor(worksite)) %>% 
  select(MANOS_ID,round,Age,worksite,As_osmadj,Dept4,BMI)

# baseline adjusted
MetiAs_reg<- MetiAs %>% 
  lm(log(As_osmadj)~Age+BMI+relevel(worksite,ref="CO4"),data=.) %>% 
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#R1R6 adjusted
MetiAs6_reg<- MetiAs6 %>% 
  lm(log(As_osmadj)~Age+BMI+relevel(worksite,ref="CO4"),data=.) %>% 
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### AB ####
#baseline df
MetoAs<-As_IOR1_q %>%
  filter(species=="Organic"&!is.na(As_osmadj)) %>% 
  mutate(worksite=as.factor(worksite)) %>% 
  select(MANOS_ID,round,country,Age,SeafoodAnyYN,worksite,As_osmadj,Dept4,BMI)

#R1R6 df
MetoAs6<-As_IOR1andR6_q %>%
  filter(species=="Organic"&!is.na(As_osmadj)) %>% 
  filter(worksite!="PL1"&worksite!="PL9"&worksite!="LA8") %>% 
  mutate(worksite=as.factor(worksite)) %>% 
  select(MANOS_ID,round,Age,SeafoodAnyYN,worksite,As_osmadj,Dept4,BMI)

# baseline adjusted
MetoAs_reg<- MetoAs %>% 
  lmer(log(As_osmadj)~Age+BMI+SeafoodAnyYN+relevel(worksite,ref="CO4")+(1|Dept4),data=.) %>% 
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetoAs6_reg<- MetoAs6 %>% 
  lmer(log(As_osmadj)~Age+BMI+SeafoodAnyYN+relevel(worksite,ref="CO4")+(1|Dept4),data=.) %>% 
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# #### UTAS
# MetUTAS<-metalsR1R6_q %>% filter(round==1) %>%
#   filter(metal=="As"&!is.na(Osmadjconc1)) %>% 
#   mutate(worksite=as.factor(worksite),
#          Dept4=as.factor(Dept4)) %>% 
#   select(MANOS_ID,round,country,Age,PestYN,worksite,Dept4,Osmadjconc1,BMI)
# 
# # baseline adjusted
# MetUTAS_reg<- MetUTAS %>% 
#   lmer(log(Osmadjconc1)~Age+BMI+PestYN+relevel(worksite,ref="CO4")+(1|Dept4),data=.) %>% 
#   tidy() %>% 
#   #transform estimates into % changes with (exp(beta)-1)*100 
#   mutate(transformed = (exp(estimate)-1)*100) %>% 
#   mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
#   mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
#   mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
#                     ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
#   mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
#                     ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Ba ####
#baseline df
MetBa<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Ba"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),
         DWhome3=as.factor(DWhome3),
         kidfunc=as.factor(kidfunc)) %>% 
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  select(MANOS_ID,round,country,Age,SeafoodAnyYN,DWhome3,kidfunc,worksite,Osmadjconc1,BMI)

#R1R6 df
MetBa6<-metalsR1andR6_q %>%
  filter(metal=="Ba"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),
         DWhome3=as.factor(DWhome3),
         kidfunc=as.factor(kidfunc)) %>% 
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  select(MANOS_ID,round,country,Age,SeafoodAnyYN,DWhome3,kidfunc,worksite,Osmadjconc1,BMI)

# BA
# baseline adjusted
MetBa_reg<- MetBa %>% 
  lm(log(Osmadjconc1)~Age+BMI+SeafoodAnyYN+relevel(worksite,ref="CO4")+
       relevel(DWhome3,ref="DWhomeprivatewell")+relevel(kidfunc,ref="2"),data=.) %>% 
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetBa6_reg<- MetBa6 %>% 
  lm(log(Osmadjconc1)~Age+BMI+relevel(worksite,ref="CO4")+
       relevel(DWhome3,ref="DWhomeprivatewell")+relevel(kidfunc,ref="2")+
       round+(1|MANOS_ID),data=.) %>% 
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Cd ####
#baseline df
MetCd<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Cd"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),
         Dept4=as.factor(Dept4),
         work_department=as.factor(work_department)) %>% 
  select(MANOS_ID,round,Age,Smokecurrent1,PestYN,worksite,work_department,Dept4,Osmadjconc1,BMI,
         Totalwaterwork)

#R1R6 df
MetCd6<-metalsR1andR6_q %>%
  filter(metal=="Cd") %>% #!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),
         Dept4=as.factor(Dept4),
         work_department=as.factor(work_department)) %>% 
  select(MANOS_ID,round,Age,Smokecurrent1,PestYN,worksite,work_department,Dept4,Osmadjconc1,BMI)

# baseline adjusted
MetCd_reg1<- MetCd %>% 
  lmer(log(Osmadjconc1)~Age+BMI+Smokecurrent1+PestYN+Totalwaterwork+
         relevel(worksite,ref="CO4")+(1|Dept4),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetCd6_reg<- MetCd6 %>% 
  lmer(log(Osmadjconc1)~Age+BMI+Smokecurrent1+relevel(worksite,ref="CO4")+ 
         (1|Dept4)+round+(1|MANOS_ID),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Co ####
#baseline df
MetCo<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Co"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),Dept4=as.factor(Dept4),
         Dept4=factor(Dept4,levels=unique(Dept4_class))) %>% 
  select(MANOS_ID,round,country,Age,Totalwaterwork,Totalnotwaterwork,worksite,
         Dept4,Osmadjconc1,BMI)

#R1R6 df
MetCo6<-metalsR1andR6_q %>% 
  filter(metal=="Co"&!is.na(Osmadjconc1)) %>% filter(MANOS_ID!=140) %>% 
  mutate(worksite=as.factor(worksite),Dept4=as.factor(Dept4),kidfunc=as.factor(kidfunc)) %>% 
  select(MANOS_ID,round,country,Age,worksite,kidfunc,Dept4,Osmadjconc1,BMI)

# baseline adjusted
MetCo_reg1<- MetCo %>% 
  lmer(log(Osmadjconc1)~Age+BMI+Totalwaterwork+Totalnotwaterwork+
         relevel(worksite,ref="CO4")+(1|Dept4),data=.) %>% #(1|country)
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

MetCo6_reg<- MetCo6 %>%
  lmer(log(Osmadjconc1)~Age+BMI+relevel(kidfunc,ref="2")+ 
         relevel(worksite,ref="CO4")+(1|Dept4)+round+(1|MANOS_ID),data=.) %>% #round, MANOS_ID
  tidy() %>%
  #transform estimates into % changes with (exp(beta)-1)*100
  mutate(transformed = (exp(estimate)-1)*100) %>%
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>%
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>%
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>%
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Cs ####
#baseline df
MetCs<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Cs"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),kidfunc=as.factor(kidfunc),
         Dept4=as.factor(Dept4)) %>% 
  select(MANOS_ID,round,country,Age,SeafoodAnyYN,worksite,Dept4,Osmadjconc1,BMI)

#R1R6 df
MetCs6<-metalsR1andR6_q %>%
  filter(metal=="Cs") %>% #&!is.na(Osmadjconc1)) %>% 
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  mutate(worksite=as.factor(worksite),kidfunc=as.factor(kidfunc),
         Dept4=as.factor(Dept4),DWhome3=as.factor(DWhome3)) %>% 
  select(MANOS_ID,round,country,Age,SeafoodAnyYN,worksite,kidfunc,DWhome3,Dept4,Osmadjconc1,BMI)

# baseline adjusted
MetCs_reg1<- MetCs %>% 
  lmer(log(Osmadjconc1)~Age+BMI+SeafoodAnyYN+
         relevel(worksite,ref="CO4")+(1|Dept4),data=.) %>%
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetCs6_reg<- MetCs6 %>% 
  lmer(log(Osmadjconc1)~Age+BMI+relevel(DWhome3,ref="DWhomeprivatewell")+relevel(kidfunc,ref="2")+
         relevel(worksite,ref="CO4")+(1|Dept4)+round+(1|MANOS_ID),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Hg ####
#baseline df
MetHg<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Hg"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),kidfunc=as.factor(kidfunc),
         Dept4=as.factor(Dept4)) %>% 
  select(MANOS_ID,round,country,Age,worksite,Dept4,Osmadjconc1,BMI)

#R1R6 df
MetHg6<-metalsR1andR6_q %>%
  filter(metal=="Hg") %>% #&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),kidfunc=as.factor(kidfunc),
         Dept4=as.factor(Dept4)) %>% 
  select(MANOS_ID,round,country,Age,worksite,Dept4,Osmadjconc1,BMI)

# baseline adjusted
MetHg_reg1<- MetHg %>% 
  lmer(log(Osmadjconc1)~Age+BMI+relevel(worksite,ref="CO4")+(1|Dept4),data=.) %>%
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetHg6_reg<- MetHg6 %>% 
  lmer(log(Osmadjconc1)~Age+BMI+relevel(worksite,ref="CO4")+
         round+(1|MANOS_ID),data=.) %>%
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Mn ####
#baseline df
MetMn<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Mn"&!is.na(Osmadjconc1)) %>% 
  mutate(Dept4=as.factor(Dept4),worksite=as.factor(worksite),kidfunc=as.factor(kidfunc)) %>% 
  select(MANOS_ID,round,Age,Totalwaterwork,kidfunc,Dept4,worksite,Osmadjconc1,BMI)

#R1R6 df
MetMn6<-metalsR1andR6_q %>%
  filter(metal=="Mn"&!is.na(Osmadjconc1)) %>% 
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  mutate(Dept4=as.factor(Dept4),worksite=as.factor(worksite),kidfunc=as.factor(kidfunc),
         DWhome3=as.factor(DWhome3)) %>% 
  select(MANOS_ID,round,Age,Typical_totalwaterwork,kidfunc,Dept4,DWhome3,worksite,Osmadjconc1,BMI)

# baseline adjusted
MetMn_reg<- MetMn %>% 
  lm(log(Osmadjconc1)~Age+BMI+relevel(kidfunc,ref="2")+relevel(worksite,ref="CO4"),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetMn6_reg<- MetMn6 %>% 
  lm(log(Osmadjconc1)~Age+BMI+relevel(kidfunc,ref="2")+relevel(DWhome3,ref="DWhomeprivatewell")+
       relevel(worksite,ref="CO4")+round+(1|MANOS_ID),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Mo ####
#baseline df
MetMo<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Mo"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),work_department=as.factor(work_department),
         kidfunc=as.factor(kidfunc)) %>% 
  select(MANOS_ID,round,Age,PestYN,Totalnotwaterwork,kidfunc,worksite,work_department,Osmadjconc1,BMI)

#R1R6 df
MetMo6<-metalsR1andR6_q %>% 
  filter(metal=="Mo"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),work_department=as.factor(work_department),
         kidfunc=as.factor(kidfunc)) %>% 
  select(MANOS_ID,round,Age,PestYN,kidfunc,worksite,work_department,Osmadjconc1,BMI)

# baseline adjusted
MetMo_reg<- MetMo %>% 
  lm(log(Osmadjconc1)~Age+BMI+PestYN+Totalnotwaterwork+relevel(kidfunc,ref="2")+
       relevel(worksite,ref="CO4"),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetMo6_reg<- MetMo6 %>% 
  lmer(log(Osmadjconc1)~Age+BMI+relevel(kidfunc,ref="2")+
         relevel(worksite,ref="CO4")+round+(1|MANOS_ID),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Pb ####
#baseline df
MetPb<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Pb"&!is.na(Osmadjconc1)) %>% 
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  mutate(worksite=as.factor(worksite),Dept4=as.factor(Dept4),DWhome3=as.factor(DWhome3),
         work_department=as.factor(work_department),kidfunc=as.factor(kidfunc)) %>% 
  select(MANOS_ID,round,country,Age,Smokecurrent1,DWhome3,worksite,Dept4,kidfunc,
         Osmadjconc1,BMI)

MetPb6<-metalsR1andR6_q %>%
  filter(metal=="Pb"&!is.na(Osmadjconc1)) %>% 
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  filter(SeafoodAnyYN==0|SeafoodAnyYN==1) %>% 
  mutate(worksite=as.factor(worksite),Dept4=as.factor(Dept4),DWhome3=as.factor(DWhome3),
         work_department=as.factor(work_department),kidfunc=as.factor(kidfunc)) %>% 
  select(MANOS_ID,round,country,Age,Smokecurrent1,DWhome3,worksite,Dept4,work_department,
         kidfunc,SeafoodAnyYN,Osmadjconc1,BMI)

# baseline adjusted
MetPb_reg1<- MetPb %>% 
  lmer(log(Osmadjconc1)~Age+BMI+Smokecurrent1+relevel(DWhome3,ref="DWhomeprivatewell")+ 
         relevel(worksite,ref="CO4")+relevel(kidfunc,ref="2")+(1|Dept4),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetPb6_reg<- MetPb6 %>% 
  lmer(log(Osmadjconc1)~Age+BMI+Smokecurrent1+SeafoodAnyYN+
         relevel(worksite,ref="CO4")+relevel(kidfunc,ref="2")+(1|Dept4)+
         round+(1|MANOS_ID),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### Tl ####
#baseline df
MetTl<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="Tl"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),Dept4=as.factor(Dept4),
         work_department=as.factor(work_department)) %>% 
  select(MANOS_ID,round,country,Age,worksite,Totalwaterwork,Dept4,Osmadjconc1,BMI)

#R1R6 df
MetTl6<-metalsR1andR6_q %>% 
  filter(metal=="Tl"&!is.na(Osmadjconc1)) %>% 
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  mutate(worksite=as.factor(worksite),Dept4=as.factor(Dept4),
         DWhome3=as.factor(DWhome3)) %>% 
  select(MANOS_ID,round,country,Age,worksite,work_department,Dept4,DWhome3,Osmadjconc1,BMI)

# baseline adjusted
MetTl_reg1<- MetTl %>% 
  lmer(log(Osmadjconc1)~Age+BMI+Totalwaterwork+relevel(worksite,ref="CO4")+(1|Dept4)
       ,data=.) %>%
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetTl6_reg<- MetTl6 %>% 
  lmer(log(Osmadjconc1)~Age+BMI+relevel(DWhome3,ref="DWhomeprivatewell")+
         relevel(worksite,ref="CO4")+(1|Dept4)+round+(1|MANOS_ID),data=.) %>%  
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### U ####
#baseline df
MetU<-metalsR1R6_q %>% filter(round==1) %>%
  filter(metal=="U"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),Dept4=as.factor(Dept4),
         work_department=as.factor(work_department),kidfunc=as.factor(kidfunc)) %>% 
  select(MANOS_ID,round,country,Age,PestYN,worksite,work_department,Dept4,kidfunc,Osmadjconc1,BMI)

#R1R6 df
MetU6<-metalsR1andR6_q %>% 
  filter(metal=="U"&!is.na(Osmadjconc1)) %>% 
  mutate(worksite=as.factor(worksite),Dept4=as.factor(Dept4),
         work_department=as.factor(work_department),kidfunc=as.factor(kidfunc)) %>% 
  select(MANOS_ID,round,country,Age,PestYN,worksite,work_department,Dept4,kidfunc,Osmadjconc1,BMI)

# baseline adjusted
MetU_reg1<- MetU %>% 
  lmer(log(Osmadjconc1)~Age+BMI+PestYN+relevel(worksite,ref="CO4")+relevel(kidfunc,ref="2")+
         (1|Dept4),data=.) %>%
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
MetU6_reg<- MetU6 %>% 
  lmer(log(Osmadjconc1)~Age+BMI+relevel(worksite,ref="CO4")+relevel(kidfunc,ref="2")+
         (1|Dept4)+round+(1|MANOS_ID),data=.) %>%  #have to drop work department and country
  tidy() %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#### bring reg results together ####
Metreglist1<-list("MetBa_reg"=MetBa_reg %>% 
                    filter(term!="(Intercept)"),
                  "MetCd_reg1"=MetCd_reg1 %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
                  "MetCo_reg1"=MetCo_reg1 %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
                  "MetCs_reg1"=MetCs_reg1 %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
                  "MetHg_reg1"=MetHg_reg1 %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
                  "MetMn_reg"=MetMn_reg %>% 
                    filter(term!="(Intercept)"),
                  "MetMo_reg"=MetMo_reg %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
                  "MetPb_reg1"=MetPb_reg1 %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
                  "MetTl_reg1"=MetTl_reg1 %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
                  "MetU_reg1"=MetU_reg1 %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"))

Asreglist1<-list("MetiAs_reg"=MetiAs_reg %>% filter(term!="(Intercept)"),
                 "MetoAs_reg"=MetoAs_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&
                                                      term!="sd__Observation"))

longAsregs<-bind_rows(Asreglist1, .id = "column_label") %>% 
  select(column_label,term,transformed,lower_CI95,upper_CI95,sig) %>% 
  mutate(transformed=signif(transformed,2),
         lower_CI95=signif(lower_CI95,2),
         upper_CI95=signif(upper_CI95,2))

longMetregs<-bind_rows(Metreglist1, .id = "column_label") %>% 
  bind_rows(longAsregs) %>% 
  select(column_label,term,transformed,lower_CI95,upper_CI95,sig) %>% 
  mutate(transformed=signif(transformed,2),
         lower_CI95=signif(lower_CI95,2),
         upper_CI95=signif(upper_CI95,2)) %>% 
  mutate(metal=case_when(column_label=="MetBa_reg"~"Ba",
                         grepl("MetCd",column_label)~"Cd",
                         grepl("MetCo",column_label)~"Co",
                         grepl("MetCs",column_label)~"Cs",
                         grepl("MetHg",column_label)~"Hg",
                         grepl("MetMn",column_label)~"Mn",
                         column_label=="MetMo_reg"~"Mo",
                         grepl("MetPb",column_label)~"Pb",
                         grepl("MetTl",column_label)~"Tl",
                         grepl("MetU",column_label)~"U",
                         column_label=="MetUTAS_reg"~"UTAS",
                         column_label=="MetiAs_reg"~"iAs",
                         grepl("MetoAs",column_label)~"AB",
                         TRUE~as.character(column_label))) %>% 
  mutate(term=case_when(grepl("DWhomecommonwell",term)~"CW",
                        term=="relevel(DWhome3, ref = \"DWhomeprivatewell\")DWhomemuni"~"M",
                        term=="relevel(DWhome3, ref = \"DWhomeprivatewell\")DWhomemunicommon"~"MC",
                        grepl(")0",term)~"moderate kidney loss",
                        grepl(")1",term)~"mild kidney loss",
                        grepl("AZ1",term)~"AZ1",
                        grepl("AZ2",term)~"AZ2",
                        grepl("AZ5",term)~"AZ5",
                        grepl("AZ7",term)~"AZ7",
                        grepl("LA8",term)~"LA8",
                        grepl("MA3",term)~"MA3",
                        grepl("PL1",term)~"PL1",
                        grepl("PL9",term)~"PL9",
                        grepl("Chinandega",term)~"RD3",
                        grepl("Leon",term)~"RD2",
                        grepl("SanSalvador",term)~"RD6",
                        grepl("Ahuachapan",term)~"RD7",
                        grepl("Sonsonate",term)~"RD5",
                        grepl(".Usulutan",term)~"RD1",
                        TRUE~as.character(term)))

newterms<-longMetregs %>% 
  filter(term=="Age"|term=="BMI"|term=="AZ1"|term=="AZ2") %>% #term=="AZ5"
  filter(metal!="AB") %>% 
  mutate(term=case_when(term=="Age"~"Demographics and diet",
                        term=="BMI"~"Home drinking water source",
                        term=="AZ1"~"Kidney function",
                        term=="AZ2"~"Worksite"),
         #term=="AZ5"~"Residential department"),
         transformed=ifelse(!is.na(transformed),NA,transformed),
         lower_CI95=ifelse(!is.na(lower_CI95),NA,lower_CI95),
         upper_CI95=ifelse(!is.na(upper_CI95),NA,upper_CI95),
         sig=ifelse(!is.na(sig),NA,sig))

#order terms
term_class <- c("LA8","PL9","PL1","AZ7","AZ5","AZ2","AZ1","MA3","Worksite",
                "mild kidney loss","moderate kidney loss","Kidney function",
                "MC","M","CW","Home drinking water source","Totalnotwaterwork","Totalwaterwork",
                "SeafoodAnyYN","PestYN","Smokecurrent1Si","BMI","Age","Demographics and diet")
metal_class<-c("iAs","AB","UTAS","Ba","Cd","Co","Cs","Hg","Mn","Mo","Pb","Tl","U")

longMetregs1<-longMetregs %>% 
  filter(metal!="UTAS") %>% 
  rbind(newterms) %>% 
  mutate(term=factor(term,levels=unique(term_class)),
         metal=factor(metal,levels=unique(metal_class)))

#write.csv(longMetregs1,"../results/longMetregs1.csv")

#### bring together R1R6 regression results ####
As6reglist1<-list("MetiAs6_reg"=MetiAs6_reg %>% filter(term!="(Intercept)"),
                  "MetoAs6_reg"=MetoAs6_reg %>%
                    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"))

longAs6reg1<-bind_rows(As6reglist1, .id = "column_label")

Metreg6list<-list(#"MetUTAS_reg"=MetUTAS_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
  "MetBa6_reg"=MetBa6_reg %>%
    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"&term!="1 | MANOS_IDTRUE"),
  "MetCd6_reg"=MetCd6_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
  "MetCo6_reg"=MetCo6_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
  "MetCs6_reg"=MetCs6_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
  "MetHg6_reg"=MetHg6_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
  "MetMn6_reg"=MetMn6_reg %>%
    filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"&term!="1 | MANOS_IDTRUE"),
  "MetMo6_reg"=MetMo6_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
  "MetPb6_reg"=MetPb6_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
  "MetTl6_reg"=MetTl6_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"),
  "MetU6_reg"=MetU6_reg %>% filter(term!="(Intercept)"&term!="sd__(Intercept)"&term!="sd__Observation"))

longMetreg6<-bind_rows(Metreg6list, .id = "column_label") %>% 
  bind_rows(longAs6reg1) %>% 
  select(column_label,term,transformed,lower_CI95,upper_CI95,sig) %>% 
  mutate(transformed=signif(transformed,2),
         lower_CI95=signif(lower_CI95,2),
         upper_CI95=signif(upper_CI95,2)) %>% 
  mutate(metal=case_when(grepl("MetBa",column_label)~"Ba",
                         grepl("MetCd",column_label)~"Cd",
                         grepl("MetCo",column_label)~"Co",
                         grepl("MetCs",column_label)~"Cs",
                         grepl("MetHg",column_label)~"Hg",
                         grepl("MetMn",column_label)~"Mn",
                         grepl("MetMo",column_label)~"Mo",
                         grepl("MetPb",column_label)~"Pb",
                         grepl("MetTl",column_label)~"Tl",
                         grepl("MetU",column_label)~"U",
                         grepl("MetUTAS",column_label)~"UTAS",
                         grepl("MetiAs",column_label)~"iAs",
                         grepl("MetoAs",column_label)~"AB",
                         TRUE~as.character(column_label))) %>% 
  mutate(term=case_when(grepl("DWhomecommonwell",term)~"CW",
                        term=="relevel(DWhome3, ref = \"DWhomeprivatewell\")DWhomemuni"~"M",
                        term=="relevel(DWhome3, ref = \"DWhomeprivatewell\")DWhomemunicommon"~"MC",
                        term=="SeafoodAnyYN1"~"SeafoodAnyYN",
                        grepl(")0",term)~"moderate kidney loss",
                        grepl(")1",term)~"mild kidney loss",
                        grepl("AZ1",term)~"AZ1",
                        grepl("AZ2",term)~"AZ2",
                        grepl("MA3",term)~"MA3",
                        grepl(")Other",term)~"Other",
                        grepl("Chinandega",term)~"RD3",
                        grepl("Leon",term)~"RD2",
                        grepl("SanSalvador",term)~"RD6",
                        grepl("Ahuachapan",term)~"RD7",
                        grepl("Sonsonate",term)~"RD5",
                        grepl(".Usulutan",term)~"RD1",
                        TRUE~as.character(term)))

#write.csv(longMetreg6,"../results/longMetreg6.csv")

