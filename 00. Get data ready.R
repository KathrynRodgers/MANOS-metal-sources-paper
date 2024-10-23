# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)

#read in file from metals processing R script
path <- "C:/Users/krodger/Dropbox/MANOS_Repository/papers/metals sources paper/R/processing"
setwd(path)
source("pMANOS_metalsR1R6.R")

#read in Qaire R script
path <- "C:/Users/krodger/Dropbox/MANOS_Repository/papers/metals sources paper/R/processing"
setwd(path)
source("pMANOS_qaire.R")

#read in kidney function script
path <- "C:/Users/krodger/Dropbox/MANOS_Repository/papers/metals sources paper/R/processing"
setwd(path)
source("pMANOS_eGFR.R")

# #read in speciated arsenic R script
path <- "C:/Users/krodger/Dropbox/MANOS_Repository/Data analysis/R/processing"
setwd(path)
source("pMANOS_specAsR1R5R6.R")

kid2<-kid %>% 
  select(MANOS_ID,round,eGFRPre_RF,eGFRPost_RF) %>% 
  mutate(eGFR_RF=ifelse(round==1,eGFRPost_RF,
                        ifelse(round==6,eGFRPre_RF,NA))) %>% 
  select(MANOS_ID,round,eGFR_RF) %>% 
  filter(!is.na(eGFR_RF)) %>% 
  distinct() %>% 
  group_by(round,MANOS_ID) %>%
  mutate(kidfunc=case_when(eGFR_RF<=60~0,
                           eGFR_RF>=60.01&eGFR_RF<=90~1,
                           eGFR_RF>=90.01~2))

# merge metals and questionnaire data
metalsR1R6_q<-left_join(metalsR1R6 %>%
                          mutate(MANOS_ID=as.numeric(MANOS_ID),
                                 round=as.numeric(round)),
                        R1_R6Q,by=c("MANOS_ID","round")) %>% 
  mutate(worksite=ifelse(Worksite=="Other","Other",
                         ifelse(round==6&worksite=="CO4","Other",worksite))) %>% 
  mutate(work_department=ifelse(round==6&worksite=="Other",
                                "Other",work_department)) %>%
  mutate(Industry=case_when(MANOS_ID==140~"other",
                            MANOS_ID==175~"other",
                            MANOS_ID==182~"other",
                            MANOS_ID==212~"other",
                            MANOS_ID==145~"other",
                            MANOS_ID==169~"other",
                            TRUE~as.character(Industry)),
         industry=ifelse(round==6,Industry,
                         ifelse(round==1,industry,NA))) %>% 
  mutate(Dept4=ifelse(is.na(Dept3),work_department,Dept3)) %>% 
  mutate(samedept=ifelse(work_department==Dept4&
                           work_department!="Other","same",
                         ifelse(work_department!=Dept4&
                                  work_department!="Other","diff",
                                ifelse(work_department=="Other"&
                                         worksite=="Other","other",NA)))) %>% 
  left_join(.,{kid2},by=c("MANOS_ID","round"))

ws<-metalsR1R6_q %>% ungroup() %>% 
  select(MANOS_ID,worksite,Worksite,work_department,Dept4,round) %>% unique() %>% 
  filter(!is.na(Worksite))

#fix worksite for people who changed jobs at R6
metalsR1R6_q<-metalsR1R6_q %>% 
  mutate(worksite=ifelse(Worksite=="Other","Other",worksite)) %>% 
  mutate(worksite1=ifelse(round==6&industry=="construction"&worksite=="Other",
                          "CO4",worksite))
#write.csv(metalsR1R6_q,"..//results/metalsR1R6_q.csv")

# 3. Clean + merge speciated As and q-aire
# merge speciated As I/O with questionaire data
As_IOR1R6_q<-left_join(As_IO %>% mutate(MANOS_ID=as.numeric(MANOS_ID),
                                        round=as.numeric(round)),
                       R1_R6Q,by=c("MANOS_ID","round")) %>% 
  mutate(Dept4=ifelse(is.na(Dept3),work_department,Dept3)) %>% 
  left_join(.,{kid2},by=c("MANOS_ID","round")) %>% 
  left_join(.,{ws},by=c("MANOS_ID","round")) %>% 
  mutate(Industry=case_when(MANOS_ID==140~"other",
                            MANOS_ID==175~"other",
                            MANOS_ID==182~"other",
                            MANOS_ID==212~"other",
                            MANOS_ID==145~"other",
                            MANOS_ID==169~"other",
                            TRUE~as.character(Industry))) %>% 
  mutate(industry=ifelse(round==6,Industry,
                         ifelse(round==1,industry,NA))) %>% 
  # some participants with As measures did not have trace metals analyzed
  mutate(worksite=case_when(grepl("Piñal",Worksite.x)~"AZ5",
                            grepl("Zapotillo",Worksite.x)~"AZ7",
                            grepl("Verde",Worksite.x)~"PL9",
                            grepl("Matas",Worksite.x)~"AZ7",
                            grepl("Valientes",Worksite.x)~"AZ7",
                            grepl("ROGFA",Worksite.x)~"PL1",
                            grepl("Gabriel",Worksite.x)~"PL1",
                            grepl("Blanco",Worksite.x)~"AZ7",
                            grepl("Jicarito",Worksite.x)~"LA8",
                            grepl("Jacinto",Worksite.x)~"LA8",
                            grepl("Vargas",Worksite.x)~"LA8",
                            grepl("Chilamates",Worksite.x)~"LA8",
                            grepl("Ramón",Worksite.x)~"LA8",
                            grepl("Martín",Worksite.x)~"LA8",
                            grepl("Amparo",Worksite.x)~"LA8",
                            grepl("Atol",Worksite.x)~"LA8",
                            grepl("Toruño",Worksite.x)~"LA8",
                            grepl("Arauz",Worksite.x)~"LA8",
                            grepl("Marcial",Worksite.x)~"LA8",
                            .default = as.character(worksite.y))) %>% 
  select(-Worksite.x,-Worksite.y,-worksite.y,-worksite.x) %>% 
  # for some reason industry is blank for 45 Nicaraguans at visit 1, need to fill in
  mutate(industry=case_when(worksite=="AZ5"~"sugarcane",
                            worksite=="AZ7"~"sugarcane",
                            worksite=="LA8"~"brick",
                            worksite=="PL1"~"plantain",
                            worksite=="PL9"~"plantain",
                            TRUE~as.character(industry))) %>% 
  mutate(work_department=case_when(worksite=="AZ5"~"Chinandega",
                                   worksite=="AZ7"~"Chinandega",
                                   worksite=="LA8"~"Leon",
                                   worksite=="PL1"~"Leon",
                                   worksite=="PL9"~"Leon",
                                   .default = as.character(work_department.y))) %>% 
  select(-work_department.x,-work_department.y) %>% 
  mutate(Dept4=ifelse(is.na(Dept3),work_department,Dept3)) %>% 
  #mutate(Dept4=ifelse(is.na(Dept4.x),Dept4.y,Dept4.x)) %>% 
  #select(-Dept4.x,-Dept4.y) %>% 
  mutate(samedept=ifelse(work_department==Dept4&work_department!="Other","same",
                         ifelse(work_department!=Dept4&work_department!="Other","diff",
                                ifelse(work_department=="Other"&worksite=="Other","other",NA)))) %>% 
  mutate(worksite1=ifelse(round==6&industry=="construction"&worksite=="Other","CO4",worksite))
#write.csv(As_IOR1R6_q,"..//results/As_IOR1R6_q.csv")


# merge As species and questionnaire data
AsR1R6_q<-left_join(AsR1R6 %>% mutate(MANOS_ID=as.numeric(MANOS_ID),
                                      round=as.numeric(round)),
                    R1_R6Q,by=c("MANOS_ID","round")) %>% 
  left_join(.,{kid2},by=c("MANOS_ID","round")) %>% 
  unique() %>% 
  left_join(.,{ws},by=c("MANOS_ID","round")) %>% #ws from metals script
  mutate(Industry6=case_when(MANOS_ID==140~"other",MANOS_ID==175~"other",
                             MANOS_ID==182~"other",MANOS_ID==212~"other",
                             MANOS_ID==145~"other",MANOS_ID==169~"other",
                             TRUE~as.character(Industry.y))) %>% 
  mutate(industry=ifelse(round==6,Industry6,
                         ifelse(round==1,industry,NA))) %>% 
  # some participants with As measures did not have trace metals analyzed
  mutate(worksite=case_when(grepl("Piñal",Worksite.x)~"AZ5",
                            grepl("Zapotillo",Worksite.x)~"AZ7",
                            grepl("Verde",Worksite.x)~"PL9",
                            grepl("Matas",Worksite.x)~"AZ7",
                            grepl("Valientes",Worksite.x)~"AZ7",
                            grepl("ROGFA",Worksite.x)~"PL1",
                            grepl("Gabriel",Worksite.x)~"PL1",
                            grepl("Blanco",Worksite.x)~"AZ7",
                            grepl("Jicarito",Worksite.x)~"LA8",
                            grepl("Jacinto",Worksite.x)~"LA8",
                            grepl("Vargas",Worksite.x)~"LA8",
                            grepl("Chilamates",Worksite.x)~"LA8",
                            grepl("Ramón",Worksite.x)~"LA8",
                            grepl("Martín",Worksite.x)~"LA8",
                            grepl("Amparo",Worksite.x)~"LA8",
                            grepl("Atol",Worksite.x)~"LA8",
                            grepl("Toruño",Worksite.x)~"LA8",
                            grepl("Arauz",Worksite.x)~"LA8",
                            grepl("Marcial",Worksite.x)~"LA8",
                            .default = as.character(worksite.y))) %>% 
  select(-Worksite.x,-Worksite.y,-worksite.y,-worksite.x) %>% 
  mutate(work_department=case_when(worksite=="AZ5"~"Chinandega",
                                   worksite=="AZ7"~"Chinandega",
                                   worksite=="LA8"~"Leon",
                                   worksite=="PL1"~"Leon",
                                   worksite=="PL9"~"Leon",
                                   .default = as.character(work_department.y))) %>% 
  select(-work_department.x,-work_department.y) %>% 
  mutate(Dept4=ifelse(is.na(Dept3),work_department,Dept3)) %>% 
  #mutate(Dept4=ifelse(is.na(Dept4.x),Dept4.y,Dept4.x)) %>% 
  #select(-Dept4.x,-Dept4.y) %>% 
  mutate(samedept=ifelse(work_department==Dept4&work_department!="Other","same",
                         ifelse(work_department!=Dept4&work_department!="Other","diff",
                                ifelse(work_department=="Other"&worksite=="Other","other",NA)))) %>% 
  mutate(worksite1=ifelse(round==6&industry=="construction"&worksite=="Other","CO4",worksite))
#write.csv(AsR1R6_q,"..//results/AsR1R6_q.csv")


