# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(lme4)
library(broom.mixed)

#read in metals and speciated As data
longMetregs1<- read.csv("../results/longMetreg1.csv")

#for labels
newlab <- as_labeller(c("iAs"="Sigma*As","AB"="AB","Ba"="Ba","Cd"="Cd","Co"="Co","Cs"="Cs",
                        "Hg"="Hg","Mn"="Mn","Mo"="Mo","Pb"="Pb","Tl"="Tl","U"="U"),label_parsed)
bold.terms <- c("Worksite","Kidney function","Home drinking water source",
                "Demographics and diet") #"Residential department"
bold.labels <- ifelse(levels(longMetregs1$term) %in% bold.terms, yes = "bold", no = "plain")
indent.labels<-ifelse(levels(longMetregs1$term) %in% bold.terms, yes=1,no=0.9)

#plot
ggplot(data=longMetregs1, aes(x=term,y=transformed,ymin=lower_CI95,ymax=upper_CI95)) +
  geom_vline(xintercept = 9,color="grey",lty=5)+
  geom_vline(xintercept = 12,color="grey",lty=5)+
  geom_vline(xintercept = 16,color="grey",lty=5)+
  geom_vline(xintercept = 23,color="grey",lty=5)+
  geom_pointrange(size=0.3) + 
  geom_hline(yintercept=1, lty=2) +   # add a dotted line at x=1 after flip
  scale_x_discrete(labels = c("B1","P2","P1","S4","S3","S2","S1","C1","Worksite",
                              "mild kidney loss","moderate kidney loss","Kidney function",
                              "Common municipal","Municipal","Commonwell", 
                              "Home drinking water source",
                              "Other beverages", "Water",'Seafood','Agrochemicals','Smoker',
                              "BMI","Age","Demographics and diet"))+
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Percent difference (95% CI)") +
  theme_bw()+
  theme(axis.text.y = element_text(face=bold.labels,size = 6.5, hjust=indent.labels),
        axis.ticks.y=element_blank())+
  facet_wrap(metal~.,scales="free_x",labeller = newlab)
#ggsave("..//results/metregR1plot.jpg", height=7, width=8, units="in")



