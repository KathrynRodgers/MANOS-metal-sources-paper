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

#### COUNTRY: regressions to check for differences by country at baseline ####
Metcount<-metalsR1R6_q %>% filter(round==1) %>%
  mutate(d_salvador=ifelse(country=="El Salvador",1,0),
         d_nicaragua=ifelse(country=="Nicaragua",1,0))

# baseline unadjusted with Osmadjconc1 for Table 2 comparisons
Metcount_regTab1 <- Metcount %>%  
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ d_salvador, data=.)) %>% #nicaragua is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100)
#write.csv(Metcount_regTab1,"..//results/metcountregTab1.csv")

# baseline multilevel
Metcount_reg4 <- Metcount %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ country+(1|Dept4), data=.)) %>% #nicaragua is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))
#write.csv(Metcount_reg4,"..//results/metcountreg4.csv")

# R1R6 unadjusted with Osmadjconc1 for Table 2 comparisons
Metcount6_regTab1 <- metalsR1andR6_q %>%  
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ round, data=.)) %>% #nicaragua is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100)
#write.csv(Metcount6_regTab1,"..//results/metcountreg6Tab1.csv")

# R1R6 adjusted test differecnes by round
Metcount6_reg4 <- metalsR1andR6_q %>%  
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ round+(1|Dept4)+(1|MANOS_ID), data=.)) %>% #R1 is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
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

# baseline unadjusted with Osmadjconc1
Ascount_regTab1 <- As_IOR1_q %>%  
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ d_salvador, data=.)) %>% #nicaragua is reference
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100)
#write.csv(Ascount_regTab1,"..//results/AscountregTab1.csv")

# baseline multilevel
Ascount_reg4 <- As_IOR1_q %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ country+(1|Dept4), data=.)) %>% #nicaragua is reference
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 unadjusted test differecnes by round Table 1
Ascount6_regTable1 <- As_IOR1andR6_q %>%  
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ round, data=.)) %>% #R1 is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100)

# R1R6 adjusted test differecnes by round
Ascount6_reg4 <- As_IOR1andR6_q %>%  
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ round+(1|Dept4)+(1|MANOS_ID), data=.)) %>% #R1 is reference
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

### INDUSTRY: regressions to check for differences by industry ####
# baseline df
Metind<-metalsR1R6_q %>% filter(round==1) %>% 
  mutate(industry=ifelse(industry=="brick","zbrick",industry),
         log_Osmadjconc1=log(Osmadjconc1)) #forces construction to be reference

# R1R6 df
Metind6<-metalsR1andR6_q %>% 
  mutate(industry=ifelse(industry=="brick","zbrick",industry))

# baseline multilevel
Metind_reg4 <- Metind %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1)~industry+
              DWhome3+Smokecurrent1+SeafoodAnyYN+country+(1|worksite), data=.)) %>% #construction is ref
  #map(.,tidy(conf.int = TRUE))
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#R1R6 adjusted
Metind6_reg4 <-Metind6 %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ industry+
              DWhome3+Smokecurrent1+SeafoodAnyYN+
              round+(1|MANOS_ID)+(1|worksite), data=.)) %>% #construction is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speicated As ####
# baseline df
Asind<-As_IOR1_q %>% 
  mutate(industry=ifelse(industry=="brick","zbrick",industry)) %>%  #forces construction to be ref
  filter(round==1)

# R1R6 df
Asind6<-As_IOR1andR6_q %>% 
  mutate(industry=ifelse(industry=="brick","zbrick",industry)) #forces construction to be ref

#baseline multilevel
Asind_reg4 <- Asind %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj)~industry+
              DWhome3+Smokecurrent1+SeafoodAnyYN+country+(1|worksite), data=.)) %>% #construction is ref
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#R1R6 adjusted
Asind6_reg4 <-Asind6 %>% 
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ industry+
              DWhome3+Smokecurrent1+SeafoodAnyYN+
              round+(1|MANOS_ID)+(1|worksite), data=.)) %>% #construction is reference
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#### WORKSITE: regressions to check for differences by worksite ####
# baseline df
Metws<-metalsR1R6_q %>% filter(round==1) %>% 
  mutate(worksite=ifelse(worksite=="CO4","AACO4",worksite),
         log_Osmadjconc1=log(Osmadjconc1))

#R1R6 df
Metws6<-metalsR1andR6_q %>% 
  mutate(worksite=ifelse(worksite=="CO4","AACO4",worksite))

#baseline multilevel
Metws_reg4 <-Metws %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ worksite+
              Smokecurrent1+PestYN+Totalwaterwork+(1|Dept4), data=.)) %>% #CO4 is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#baseline multilevel with typical water drank
Metws_reg4_TW <-Metws %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ worksite+
              Smokecurrent1+PestYN+Typical_totalwaterwork+(1|Dept4), data=.)) %>% #CO4 is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Metws6_reg4 <-Metws6 %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ worksite+Smokecurrent1+Typical_totalwaterwork+
              +round+(1|MANOS_ID)+(1|Dept4), data=.)) %>% #CO4 is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
# baseline df
Asws<-As_IOR1_q %>% filter(round==1) %>% 
  mutate(worksite=ifelse(worksite=="CO4","AACO4",worksite)) 

# R1R6 df
Asws6<-As_IOR1andR6_q %>% 
  mutate(worksite=ifelse(worksite=="CO4","AACO4",worksite))

# baseline multilevel
Asws_reg4 <-Asws %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ worksite+
              Smokecurrent1+PestYN+Totalwaterwork+(1|Dept4), data=.)) %>% #CO4 is reference
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline multilevel with typical water consumptoin
Asws_reg4_TW <-Asws %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ worksite+
              Smokecurrent1+PestYN+Typical_totalwaterwork+(1|Dept4), data=.)) %>% #CO4 is reference
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Asws6_reg4 <-Asws6 %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ worksite+
              Smokecurrent1+Typical_totalwaterwork+round+(1|MANOS_ID)+(1|Dept4), data=.)) %>%
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#### DWSOURCE: regressions for DW source at home ####
# baseline df
MetDWhome<-metalsR1R6_q %>% filter(round==1) %>% 
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  mutate(DWhome3=ifelse(DWhome3=="DWhomeprivatewell","AADWhomeprivatewell",DWhome3))

#R1R6 df
MetDWhome6<-metalsR1andR6_q %>%
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  mutate(DWhome3=ifelse(DWhome3=="DWhomeprivatewell","AADWhomeprivatewell",DWhome3))

# baseline adjusted
MetDWhome_reg4 <-MetDWhome %>%
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ DWhome3+Dept4, data=.)) %>% #PW is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#R1R6 adjusted
MetDWhome6_reg4 <-MetDWhome6 %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ DWhome3+round+Dept4+(1|MANOS_ID), data=.)) %>% #PW is reference
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for spceciated As #####
# baseline df
AsDWhome<-As_IOR1_q %>%
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  mutate(DWhome3=ifelse(DWhome3=="DWhomeprivatewell","AADWhomeprivatewell",DWhome3))

#R1R6 df
AsDWhome6<-As_IOR1andR6_q %>%
  filter(DWhome3!="DWhomeother") %>% 
  filter(DWhome3!="DWhomeriver") %>% 
  mutate(DWhome3=ifelse(DWhome3=="DWhomeprivatewell","AADWhomeprivatewell",DWhome3))

# baseline adjusted
AsDWhome_reg4 <-AsDWhome %>%
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ DWhome3+Dept4, data=.)) %>% #PW is reference
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#R1R6 adjusted
AsDWhome6_reg4 <-AsDWhome6 %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ DWhome3+round+Dept4+(1|MANOS_ID), data=.)) %>% #PW is reference
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#### HOME DEPT: regressions for home dept ####
# baseline df
MetDepthome<-metalsR1R6_q %>% filter(round==1) %>% 
  mutate(Dept4=ifelse(Dept4=="LaLibertad","AALaLibertad",Dept4))

#R1R6 df
MetDepthome6<-metalsR1andR6_q %>%
  mutate(Dept4=ifelse(Dept4=="LaLibertad","AALaLibertad",Dept4))

#baseline multilevel
MetDepthome_reg4 <-MetDepthome %>% 
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ Dept4+
              country+Smokecurrent1+PestYN+Totalwaterwork+(1|worksite), data=.)) %>% #LaLibertad is ref
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#baseline multilevel with typical water consumption
MetDepthome_reg4_TW <-MetDepthome %>% 
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ Dept4+
              country+Smokecurrent1+PestYN+Typical_totalwaterwork+(1|worksite), data=.)) %>% #LaLibertad ref
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#R1R6 adjusted
MetDepthome6_reg4 <-MetDepthome6 %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ Dept4+
              Smokecurrent1+Typical_totalwaterwork+round+(1|worksite)+(1|MANOS_ID), data=.)) %>% #LaLib re
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
#baseline df
AsDepthome<-As_IOR1_q %>% filter(round==1) %>% 
  mutate(Dept4=ifelse(Dept4=="LaLibertad","AALaLibertad",Dept4))

#R1R6 df
AsDepthome6<-As_IOR1andR6_q %>%
  mutate(Dept4=ifelse(Dept4=="LaLibertad","AALaLibertad",Dept4))

# multilevel regression
AsDepthome_reg4 <-AsDepthome %>% 
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ Dept4+
              country+Smokecurrent1+PestYN+Totalwaterwork+(1|worksite), data=.)) %>% #LaLibertad is ref
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# multilevel regression with typical water consumption
AsDepthome_reg4_TW <-AsDepthome %>% 
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ Dept4+
              country+Smokecurrent1+PestYN+Typical_totalwaterwork+(1|worksite), data=.)) %>%
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#R1R6 adjusted
AsDepthome6_reg4 <-AsDepthome6 %>% 
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ Dept4+
              Smokecurrent1+Typical_totalwaterwork+round+(1|worksite)+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#### WATER: regressions for water consumed at work ####
# baseline df
Metwaterwork<-metalsR1R6_q %>% filter(round==1)
# R1R6 df
Metwaterwork6<-metalsR1andR6_q

# baseline adjusted
Metwaterwork_reg4 <-Metwaterwork %>% 
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ Totalwaterwork+Dept4+Smokecurrent1+PestYN+
            Totalnotwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline adjusted with typical water consumption
Metwaterwork_reg4_TW <-Metwaterwork %>% 
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ Typical_totalwaterwork+Dept4+Smokecurrent1+PestYN+
            Typical_totalnotwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Metwaterwork6_reg4 <-Metwaterwork6 %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ Typical_totalwaterwork+Dept4+Smokecurrent1+
              Typical_totalnotwaterwork+round+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
# baseline df
Aswaterwork<-As_IOR1_q

#R1R6 df
Aswaterwork6<-As_IOR1andR6_q

# baseline adjusted
Aswaterwork_reg4 <-Aswaterwork %>% 
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ Totalwaterwork+Dept4+Smokecurrent1+PestYN+
            Totalnotwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline adjusted with typical water consumption
Aswaterwork_reg4_TW <-Aswaterwork %>% 
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ Typical_totalwaterwork+Dept4+Smokecurrent1+PestYN+
            Typical_totalnotwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Aswaterwork6_reg4 <-Aswaterwork6 %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ Typical_totalwaterwork+Dept4+Smokecurrent1+
              Typical_totalnotwaterwork+round+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# NOT WATER: regressions for not-water consumed at work ####
# baseline df
Metnotwaterwork<-metalsR1R6_q %>% filter(round==1)

# R1R6 df
Metnotwaterwork6<-metalsR1andR6_q

# baseline adjusted
Metnotwaterwork_reg4 <-Metwaterwork %>% 
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ Totalnotwaterwork+
            Dept4+Smokecurrent1+PestYN+Totalwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline adjusted with typical water consumption
Metnotwaterwork_reg4_TW <-Metwaterwork %>% 
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ Typical_totalnotwaterwork+
            Dept4+Smokecurrent1+PestYN+Typical_totalwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Metnotwaterwork6_reg4 <-Metnotwaterwork6 %>% 
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ Typical_totalnotwaterwork+Dept4+Smokecurrent1+
              Typical_totalwaterwork+round+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
# baseline df
Asnotwaterwork<-As_IOR1_q

# R1R6 df
Asnotwaterwork6<-As_IOR1andR6_q

# baseline adjusted
Asnotwaterwork_reg4 <-Asnotwaterwork %>% 
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ Totalnotwaterwork+
            Dept4+Smokecurrent1+PestYN+Totalwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline adjusted with typical water consumption
Asnotwaterwork_reg4_TW <-Asnotwaterwork %>% 
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ Typical_totalnotwaterwork+
            Dept4+Smokecurrent1+PestYN+Typical_totalwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Asnotwaterwork6_reg4 <-Asnotwaterwork6 %>% 
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ Typical_totalnotwaterwork+Dept4+Smokecurrent1+
              Typical_totalwaterwork+round+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

### AGROCHEMICAL EXPOSURES: regressions for agrichemical exposures in last 2-3 days ####
# baseline df
Metpestwork<-metalsR1R6_q %>% filter(round==1)

# baseline adjusted
Metpestwork_reg4 <-Metpestwork %>% 
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ PestYN+
              country+DWhome3+Smokecurrent1+SeafoodAnyYN+Totalwaterwork+(1|worksite), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline adjusted with typical water consumption
Metpestwork_reg4_TW <-Metpestwork %>% 
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ PestYN+
              country+DWhome3+Smokecurrent1+SeafoodAnyYN+Typical_totalwaterwork+(1|worksite), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
# baseline df
Aspest<-As_IOR1_q

# baseline adjusted
Aspest_reg4 <-Aspest %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ PestYN+
              country+DWhome3+Smokecurrent1+SeafoodAnyYN+Totalwaterwork+(1|worksite), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline adjusted with typical water consumption
Aspest_reg4_TW <-Aspest %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ PestYN+
              country+DWhome3+Smokecurrent1+SeafoodAnyYN+Typical_totalwaterwork+(1|worksite), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# SMOKING: regressions for smoking ####
# baseline df
Metsmoke<-metalsR1R6_q %>% filter(round==1)
#R1R6 df
Metsmoke6<-metalsR1andR6_q

# baseline adjusted
Metsmoke_reg4 <-Metsmoke %>%
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ Smokecurrent1+
            Dept4+PestYN+Totalwaterwork+Totalnotwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline adjusted with typical water consumption
Metsmoke_reg4_TW <-Metsmoke %>%
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ Smokecurrent1+
            Dept4+PestYN+Typical_totalwaterwork+Typical_totalnotwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Metsmoke6_reg4 <-Metsmoke6 %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ Smokecurrent1+Dept4+Typical_totalwaterwork+Typical_totalnotwaterwork+round+
              (1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
# baseline df
Assmoke<-As_IOR1_q

#R1R6 df
Assmoke6<-As_IOR1andR6_q

# baseline adjusted
Assmoke_reg4 <-Assmoke %>%
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ Smokecurrent1+
            Dept4+PestYN+Totalwaterwork+Totalnotwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# baseline adjusted with typical water consumption
Assmoke_reg4_TW <-Assmoke %>%
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ Smokecurrent1+
            Dept4+PestYN+Typical_totalwaterwork+Typical_totalnotwaterwork, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Assmoke6_reg4 <-Assmoke6 %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ Smokecurrent1+Typical_totalwaterwork+Typical_totalnotwaterwork+
              round+Dept4+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# SEAFOOD: regressions for eat seafood in last 3 days ####
# baseline df
Metseafood<-metalsR1R6_q %>% filter(round==1) %>% 
  select(MANOS_ID,country,metal,Osmadjconc1,SeafoodAnyYN,Dept4) %>% 
  filter(SeafoodAnyYN!=2)

#R1R6 df
Metseafood6<-metalsR1andR6_q %>% 
  filter(SeafoodAnyYN!=2)

# baseline adjusted
Metseafood_reg4<- Metseafood %>% 
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1)~SeafoodAnyYN+Dept4,data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Metseafood6_reg4 <-Metseafood6 %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ SeafoodAnyYN+round+Dept4+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
#baseline df
Asseafood<-As_IOR1_q %>% filter(SeafoodAnyYN!=2)

#R1R6 df
Asseafood6<-As_IOR1andR6_q %>% 
  filter(SeafoodAnyYN!=2) %>% 
  mutate(SeafoodAnyYN=as.factor(SeafoodAnyYN))

#baseline adjusted
Asseafood_reg4<-Asseafood %>% 
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ SeafoodAnyYN+Dept4, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Asseafood6_reg4 <-Asseafood6 %>%
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ SeafoodAnyYN+round+Dept4+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#### 
# KIDNEY FUNCTION: regressions for kidfunc ####
# baseline df
Metkidfunc<-metalsR1R6_q %>% 
  mutate(kidfunc=ifelse(kidfunc==0,1, #moderate loss =1
                        ifelse(kidfunc==1,2, #mild loss = 2
                               ifelse(kidfunc==2,0,NA)))) #normal=0

#R1R6 df
Metkidfunc6<-metalsR1andR6_q %>% 
  mutate(kidfunc=ifelse(kidfunc==0,1, #moderate loss =1
                        ifelse(kidfunc==1,2, #mild loss = 2
                               ifelse(kidfunc==2,0,NA)))) #normal=0

# baseline adjusted
Metkidfunc_reg4 <-Metkidfunc %>%
  split(.$metal) %>% 
  map(~lm(log(Osmadjconc1) ~ as.factor(kidfunc)+ # normal kidfunc=referent
            Age, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

# R1R6 adjusted
Metkidfunc6_reg4 <-Metkidfunc6 %>%
  split(.$metal) %>% 
  map(~lmer(log(Osmadjconc1) ~ as.factor(kidfunc)+ # normal kidfunc=referent
              round+Age+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "metal") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

##### now for speciated As ####
# baseline df
Askidfunc<-As_IOR1_q %>% 
  mutate(kidfunc=ifelse(kidfunc==0,1, #moderate loss =1
                        ifelse(kidfunc==1,2, #mild loss = 2
                               ifelse(kidfunc==2,0,NA)))) #normal=0

#R1R6 df
Askidfunc6<-As_IOR1andR6_q %>% 
  mutate(kidfunc=ifelse(kidfunc==0,1, #moderate loss =1
                        ifelse(kidfunc==1,2, #mild loss = 2
                               ifelse(kidfunc==2,0,NA)))) #normal=0

# baseline adjusted
Askidfunc_reg4 <-Askidfunc %>% 
  split(.$species) %>% 
  map(~lm(log(As_osmadj) ~ as.factor(kidfunc)+ # normal kidfunc=referent
            Age, data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))
#write.csv(Askidfunc_reg2.1,"..//results/Askidfuncreg2.1.csv")

# R1R6 adjusted
Askidfunc6_reg4 <-Askidfunc6  %>% 
  split(.$species) %>% 
  map(~lmer(log(As_osmadj) ~ as.factor(kidfunc)+ # normal kidfunc=referent
              Age+round+(1|MANOS_ID), data=.)) %>% 
  map(.,tidy) %>% 
  bind_rows(.id = "species") %>% 
  #transform estimates into % changes with (exp(beta)-1)*100 
  mutate(transformed = (exp(estimate)-1)*100) %>% 
  mutate(lower_CI95 = (exp(estimate-1.96*std.error)-1)*100) %>% 
  mutate(upper_CI95 = (exp(estimate+1.96*std.error)-1)*100) %>% 
  mutate(sig=ifelse(lower_CI95>0&upper_CI95>0,"sigpos",
                    ifelse(lower_CI95<0&upper_CI95<0,"signeg",NA))) %>% 
  mutate(dir=ifelse(is.na(sig)&transformed<0,"neg",
                    ifelse(is.na(sig)&transformed>0,"post",sig)))

#### Bring all baseline results together ####
# metal baseline resgressions
dfreglist1<-list("Metpestwork_reg4"=Metpestwork_reg4 %>% filter(term=="PestYN"),
                 "MetDWhome_reg4"=MetDWhome_reg4 %>%
                   filter(term=="DWhome3DWhomecommonwell"|term=="DWhome3DWhomemuni"|
                            term=="DWhome3DWhomemunicommon"),
                 "Metwaterwork_reg4"=Metwaterwork_reg4 %>% filter(term=="Totalwaterwork"),
                 "Metnotwaterwork_reg4"=Metnotwaterwork_reg4 %>% filter(term=="Totalnotwaterwork"),
                 "Metsmoke_reg4"=Metsmoke_reg4 %>% filter(term=="Smokecurrent1Si"),
                 "Metseafood_reg4"=Metseafood_reg4 %>% filter(term=="SeafoodAnyYN"),
                 "Metkidfunc_reg4"=Metkidfunc_reg4 %>%
                   filter(term=="as.factor(kidfunc)1"|term=="as.factor(kidfunc)2"))

dfreglist2<-list("Metcount_reg4"=Metcount_reg4 %>% filter(term=="countryNicaragua"),
                 "MetDepthome_reg4"=MetDepthome_reg4 %>%
                   filter(term=="Dept4Ahuachapan"|term=="Dept4Chinandega"|term=="Dept4Leon"|
                            term=="Dept4SanSalvador"|term=="Dept4Sonsonate"|term=="Dept4Usulutan"),
                 "Metind_reg4"=Metind_reg4 %>%
                   filter(term=="industryzbrick"|term=="industrycorn"|term=="industryplantain"|
                            term=="industrysugarcane"),
                 "Metws_reg4"=Metws_reg4 %>%
                   filter(term=="worksiteAZ1"|term=="worksiteAZ2"|term=="worksiteAZ5"|
                            term=="worksiteAZ7"|term=="worksiteMA3"|term=="worksiteLA8"|term=="worksitePL1"|                           term=="worksitePL9"))

#speciated As baseline results
Asdfreglist1<-list("Aspestwork_reg4"=Aspest_reg4 %>% filter(term=="PestYN"),
                   "AsDWhome_reg4"=AsDWhome_reg4 %>%
                     filter(term=="DWhome3DWhomecommonwell"|term=="DWhome3DWhomemuni"|
                              term=="DWhome3DWhomemunicommon"),
                   "Aswaterwork_reg4"=Aswaterwork_reg4 %>% filter(term=="Totalwaterwork"),
                   "Asnotwaterwork_reg4"=Asnotwaterwork_reg4 %>% filter(term=="Totalnotwaterwork"),
                   "Assmoke_reg4"=Assmoke_reg4 %>% filter(term=="Smokecurrent1Si"),
                   "Asseafood_reg4"=Asseafood_reg4 %>% filter(term=="SeafoodAnyYN"),
                   "Askidfunc_reg4"=Askidfunc_reg4 %>%
                     filter(term=="as.factor(kidfunc)1"|term=="as.factor(kidfunc)2"))

Asdfreglist2<-list("Ascount_reg4"=Ascount_reg4 %>% filter(term=="countryNicaragua"),
                   "AsDepthome_reg4"=AsDepthome_reg4 %>%
                     filter(term=="Dept4Ahuachapan"|term=="Dept4Chinandega"|term=="Dept4Leon"|
                              term=="Dept4SanSalvador"|term=="Dept4Sonsonate"|term=="Dept4Usulutan"),
                   "Asind_reg4"=Asind_reg4 %>%
                     filter(term=="industryzbrick"|term=="industrycorn"|term=="industryplantain"|
                              term=="industrysugarcane"),
                   "Asws_reg4"=Asws_reg4 %>%
                     filter(term=="worksiteAZ1"|term=="worksiteAZ2"|term=="worksiteAZ5"|
                              term=="worksiteAZ7"|term=="worksiteMA3"|term=="worksiteLA8"|
                              term=="worksitePL1"|term=="worksitePL9"))                                     

longMod1Asreg4s1<-bind_rows(Asdfreglist1, .id = "column_label") %>% 
  rename(metal=species) %>% 
  filter(metal!="Total")

longMod1Asreg4s2<-bind_rows(Asdfreglist2, .id = "column_label") %>% 
  rename(metal=species) %>% 
  filter(metal!="Total")

# bring metal and speciated As results together
longMod1reg4s1<-bind_rows(dfreglist1, .id = "column_label") %>% 
  bind_rows(longMod1Asreg4s1)

longMod1reg4s2<-bind_rows(dfreglist2, .id = "column_label") %>% 
  bind_rows(longMod1Asreg4s2)

#order metals
metorder<-longMod1reg4s1 %>%
  select(metal) %>%
  unique() %>%
  mutate(ChemOrder=case_when(
    metal=="iAs" ~ 13,
    metal=="oAs"~12,
    metal=="UTAS"~11,
    metal=="Ba" ~ 10,
    metal=="Cd" ~ 9,
    metal=="Co" ~ 8,
    metal=="Cs" ~ 7,
    metal=="Hg" ~ 6,
    metal=="Mn" ~ 5,
    metal=="Mo" ~ 4,
    metal=="Pb" ~ 3,
    metal=="Tl" ~ 2,
    metal=="U" ~1))

longMod1reg4s1<-longMod1reg4s1 %>% 
  mutate(metal=ifelse(metal=="As","UTAS",
                      ifelse(metal=="Organic","oAs",
                             ifelse(metal=="Inorganic","iAs",metal)))) %>% 
  left_join(.,{metorder},by="metal") %>% 
  select(-p.value)

longMod1reg4s2<-bind_rows(dfreglist2, .id = "column_label") %>% 
  bind_rows(longMod1Asreg4s2) %>% 
  mutate(metal=ifelse(metal=="As","UTAS",
                      ifelse(metal=="Organic","oAs",
                             ifelse(metal=="Inorganic","iAs",metal)))) %>% 
  left_join(.,{metorder},by="metal")

#write.csv(longMod1reg4s1,"../results/longMod1reg4s1.csv")
#write.csv(longMod1reg4s2,"../results/longMod1reg4s2.csv")

#re order
longMod1reg4s1$term<-factor(longMod1reg4s1$term, levels=c("Smokecurrent1Si","PestYN","SeafoodAnyYN","Totalwaterwork","Totalnotwaterwork",                           "DWhome3DWhomecommonwell","DWhome3DWhomemuni","DWhome3DWhomemunicommon",
                                                          "as.factor(kidfunc)1","as.factor(kidfunc)2"))
longMod1reg4s2$term<-factor(longMod1reg4s2$term, levels=c("countryNicaragua","industrycorn","industrysugarcane","industryplantain","industryzbrick",
                                                          "worksiteMA3","worksiteAZ1","worksiteAZ2","worksiteAZ5","worksiteAZ7","worksitePL1",
                                                          "worksitePL9","worksiteLA8","Dept4Usulutan","Dept4Sonsonate","Dept4SanSalvador",
                                                          "Dept4Ahuachapan","Dept4Chinandega","Dept4Leon"))

#### Put together results for TYPICAL drinking water at baseline ####
dfreglist1_TW<-list("Metpestwork_reg4_TW"=Metpestwork_reg4_TW %>% filter(term=="PestYN"),
                    "Metwaterwork_reg4_TW"=Metwaterwork_reg4_TW %>% filter(term=="Typical_totalwaterwork"),
                    "Metnotwaterwork_reg4_TW"=Metnotwaterwork_reg4_TW %>%
                      filter(term=="Typical_totalnotwaterwork"),
                    "Metsmoke_reg4_TW"=Metsmoke_reg4_TW %>% filter(term=="Smokecurrent1Si"))

dfreglist2_TW<-list("MetDepthome_reg4_TW"=MetDepthome_reg4_TW %>%
                      filter(term=="Dept4Ahuachapan"|term=="Dept4Chinandega"|term=="Dept4Leon"|
                               term=="Dept4SanSalvador"|term=="Dept4Sonsonate"|term=="Dept4Usulutan"),
                    "Metws_reg4_TW"=Metws_reg4_TW %>%
                      filter(term=="worksiteAZ1"|term=="worksiteAZ2"|term=="worksiteAZ5"|
                               term=="worksiteAZ7"|term=="worksiteMA3"|term=="worksiteLA8"|
                               term=="worksitePL1"|term=="worksitePL9"))

# now for speciated As TYPICAL water at baseline
Asdfreglist1_TW<-list("Aspestwork_reg4_TW"=Aspest_reg4_TW %>% filter(term=="PestYN"),
                      "Aswaterwork_reg4_TW"=Aswaterwork_reg4_TW %>%
                        filter(term=="Typical_totalwaterwork"),
                      "Asnotwaterwork_reg4_TW"=Asnotwaterwork_reg4_TW %>%
                        filter(term=="Typical_totalnotwaterwork"),
                      "Assmoke_reg4_TW"=Assmoke_reg4_TW %>% filter(term=="Smokecurrent1Si"))

Asdfreglist2_TW<-list("AsDepthome_reg4_TW"=AsDepthome_reg4_TW %>%
                        filter(term=="Dept4Ahuachapan"|term=="Dept4Chinandega"|term=="Dept4Leon"|
                                 term=="Dept4SanSalvador"|term=="Dept4Sonsonate"|term=="Dept4Usulutan"),
                      "Asws_reg4_TW"=Asws_reg4_TW %>%
                        filter(term=="worksiteAZ1"|term=="worksiteAZ2"|term=="worksiteAZ5"|
                                 term=="worksiteAZ7"|term=="worksiteMA3"|term=="worksiteLA8"|
                                 term=="worksitePL1"|term=="worksitePL9"))

longMod1Asreg4s1_TW<-bind_rows(Asdfreglist1_TW, .id = "column_label") %>% 
  rename(metal=species) %>% 
  filter(metal!="Total")

longMod1Asreg4s2_TW<-bind_rows(Asdfreglist2_TW, .id = "column_label") %>% 
  rename(metal=species) %>% 
  filter(metal!="Total")

longMod1reg4s1_TW<-bind_rows(dfreglist1_TW, .id = "column_label") %>% 
  bind_rows(longMod1Asreg4s1_TW) %>% 
  mutate(metal=ifelse(metal=="As","UTAS",
                      ifelse(metal=="Organic","oAs",
                             ifelse(metal=="Inorganic","iAs",metal)))) %>% 
  left_join(.,{metorder},by="metal") %>% 
  select(-p.value)

longMod1reg4s2_TW<-bind_rows(dfreglist2_TW, .id = "column_label") %>% 
  bind_rows(longMod1Asreg4s2_TW) %>% 
  mutate(metal=ifelse(metal=="As","UTAS",
                      ifelse(metal=="Organic","oAs",
                             ifelse(metal=="Inorganic","iAs",metal)))) %>% 
  left_join(.,{metorder},by="metal")

#re order
longMod1reg4s1_TW$term<-factor(longMod1reg4s1_TW$term, levels=c("Smokecurrent1Si","PestYN","SeafoodAnyYN","Typical_totalwaterwork","Typical_totalnotwaterwork",           "DWhome3DWhomecommonwell","DWhome3DWhomemuni","DWhome3DWhomemunicommon",
                                                                "as.factor(kidfunc)1","as.factor(kidfunc)2"))
longMod1reg4s2_TW$term<-factor(longMod1reg4s2_TW$term, levels=c("countryNicaragua","industrycorn","industrysugarcane","industryplantain","industryzbrick",
                                                                "worksiteMA3","worksiteAZ1","worksiteAZ2","worksiteAZ5","worksiteAZ7","worksitePL1",
                                                                "worksitePL9","worksiteLA8", 
                                                                "Dept4Usulutan","Dept4Sonsonate","Dept4SanSalvador","Dept4Ahuachapan","Dept4Chinandega",
                                                                "Dept4Leon"))

#write.csv(longMod1reg4s1_TW,"../results/longMod1reg4s1_TW.csv")
#write.csv(longMod1reg4s2_TW,"../results/longMod1reg4s2_TW.csv")

#### Bring all the R1R6 results together ####
df6reglist1<-list("MetDWhome6_reg4"=MetDWhome6_reg4 %>%
                    filter(term=="DWhome3DWhomecommonwell"|term=="DWhome3DWhomemuni"|
                             term=="DWhome3DWhomemunicommon"),
                  "Metwaterwork6_reg4"=Metwaterwork6_reg4 %>% filter(term=="Typical_totalwaterwork"),
                  "Metnotwaterwork6_reg4"=Metnotwaterwork6_reg4 %>%
                    filter(term=="Typical_totalnotwaterwork"),
                  "Metsmoke6_reg4"=Metsmoke6_reg4 %>% filter(term=="Smokecurrent1Si"),
                  "Metseafood6_reg4"=Metseafood6_reg4 %>% filter(term=="SeafoodAnyYN1"),
                  "Metkidfunc6_reg4"=Metkidfunc6_reg4 %>%
                    filter(term=="as.factor(kidfunc)1"|term=="as.factor(kidfunc)2"))

df6reglist2<-list("Metcount6_reg4"=Metcount6_reg4 %>% filter(term=="round"),
                  "MetDepthome6_reg4"=MetDepthome6_reg4 %>%
                    filter(term=="Dept4Ahuachapan"|term=="Dept4SanSalvador"|term=="Dept4Sonsonate"|
                             term=="Dept4Usulutan"),
                  "Metind6_reg4"=Metind6_reg4 %>%
                    filter(term=="industrycorn"|term=="industrysugarcane"|term=="industryother") %>% 
                    mutate(term=case_when(term=="industryother"~"d_Indother",
                                          TRUE~as.character(term))),
                  "Metws6_reg4"=Metws6_reg4 %>%
                    filter(term=="worksiteAZ1"|term=="worksiteAZ2"|term=="worksiteMA3"|
                             term=="worksiteOther"))

# speciated As R1R6 results
Asdf6reglist1<-list("AsDWhome6_reg4"=AsDWhome6_reg4 %>%
                      filter(term=="DWhome3DWhomecommonwell"|term=="DWhome3DWhomemuni"|
                               term=="DWhome3DWhomemunicommon"),
                    "Aswaterwork6_reg4"=Aswaterwork6_reg4 %>% filter(term=="Typical_totalwaterwork"),
                    "Asnotwaterwork6_reg4"=Asnotwaterwork6_reg4 %>%
                      filter(term=="Typical_totalnotwaterwork"),
                    "Assmoke6_reg4"=Assmoke6_reg4 %>% filter(term=="Smokecurrent1Si"),
                    "Asseafood6_reg4"=Asseafood6_reg4 %>% filter(term=="SeafoodAnyYN1"),
                    "Askidfunc6_reg4"=Askidfunc6_reg4 %>%
                      filter(term=="as.factor(kidfunc)1"|term=="as.factor(kidfunc)2"))

Asdf6reglist2<-list("Ascount6_reg4"=Ascount6_reg4 %>% filter(term=="round"),
                    "AsDepthome6_reg4"=AsDepthome6_reg4 %>%
                      filter(term=="Dept4Ahuachapan"|term=="Dept4SanSalvador"|term=="Dept4Sonsonate"|
                               term=="Dept4Usulutan"),
                    "Asind6_reg4"=Asind6_reg4 %>% filter(term=="industrycorn"|term=="industrysugarcane"|term=="industryother") %>% 
                      mutate(term=ifelse(term=="industryother","d_Indother",term)),
                    "Asws6_reg4"=Asws6_reg4 %>%
                      filter(term=="worksiteAZ1"|term=="worksiteAZ2"|term=="worksiteMA3"|
                               term=="worksiteOther"))

longMod2Asregs1<-bind_rows(Asdf6reglist1, .id = "column_label") %>% 
  rename(metal=species) %>% 
  filter(metal!="Total")

longMod2Asregs2<-bind_rows(Asdf6reglist2, .id = "column_label") %>% 
  rename(metal=species) %>% 
  filter(metal!="Total")

longMod2regs1<-bind_rows(df6reglist1, .id = "column_label") %>% 
  bind_rows(longMod2Asregs1) %>% 
  mutate(metal=ifelse(metal=="As","UTAS",
                      ifelse(metal=="Organic","oAs",
                             ifelse(metal=="Inorganic","iAs",metal)))) %>% 
  left_join(.,{metorder},by="metal")

longMod2regs2<-bind_rows(df6reglist2, .id = "column_label") %>% 
  bind_rows(longMod2Asregs2) %>% 
  mutate(metal=ifelse(metal=="As","UTAS",
                      ifelse(metal=="Organic","oAs",
                             ifelse(metal=="Inorganic","iAs",metal)))) %>% 
  left_join(.,{metorder},by="metal")

#re order
longMod2regs1$term<-factor(longMod2regs1$term, levels=c("Smokecurrent1Si","SeafoodAnyYN1","Typical_totalwaterwork","Typical_totalnotwaterwork",
                                                        "DWhome3DWhomecommonwell","DWhome3DWhomemuni","DWhome3DWhomemunicommon",
                                                        "as.factor(kidfunc)1","as.factor(kidfunc)2"))
longMod2regs2$term<-factor(longMod2regs2$term, levels=c("round","industrycorn","industrysugarcane","d_Indother",
                                                        "worksiteMA3","worksiteAZ1","worksiteAZ2","worksiteOther",
                                                        "Dept4Usulutan","Dept4Sonsonate","Dept4SanSalvador","Dept4Ahuachapan"))

#write.csv(longMod2regs1,"../results/longMod2regs1.csv")
#write.csv(longMod2regs2,"../results/longMod2regs2.csv")


