# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(lme4)
library(broom.mixed)

#read in metals and speciated As data
longMetreg6<- read.csv("../results/longMetreg6.csv")

# now for R1R6 metal forest plots
#order terms
term_class <- c("Other","AZ2","AZ1","MA3","Worksite",
                "mild kidney loss","moderate kidney loss","Kidney function",
                "MC","M","CW","Home drinking water source","round",
                "SeafoodAnyYN","Smokecurrent1Si","BMI","Age","Demographics and diet")
metal_class<-c("iAs","AB","UTAS","Ba","Cd","Co","Cs","Hg","Mn","Mo","Pb","Tl","U")

longMetreg6.1<-longMetreg6 %>% 
  filter(metal!="UTAS") %>% 
  rbind(newterms) %>% 
  mutate(term=factor(term,levels=unique(term_class)),
         metal=factor(metal,levels=unique(metal_class)))

#for labels
newlab <- as_labeller(c("iAs"="Sigma*As","AB"="AB","Ba"="Ba","Cd"="Cd","Co"="Co","Cs"="Cs",
                        "Hg"="Hg","Mn"="Mn","Mo"="Mo","Pb"="Pb","Tl"="Tl","U"="U"),label_parsed)
bold.terms <- 
  c("Worksite","Kidney function","Home drinking water source","Demographics and diet")

bold.labels <- ifelse(levels(longMetreg6.1$term) %in% bold.terms, yes = "bold", no = "plain")
indent.labels <-ifelse(levels(longMetreg6.1$term) %in% bold.terms, yes=1,no=0.9)

ggplot(data=longMetreg6.1, aes(x=term, y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_vline(xintercept = 5,color="grey",lty=5)+
  geom_vline(xintercept = 8,color="grey",lty=5)+
  geom_vline(xintercept = 12,color="grey",lty=5)+
  geom_vline(xintercept = 20,color="grey",lty=5)+
  geom_pointrange(size=0.3) + 
  geom_hline(yintercept=1, lty=2) +   # add a dotted line at x=1 after flip
  scale_x_discrete(labels = c("Other","S2","S1","C1","Worksite",
                              "mild kidney loss","moderate kidney loss","Kidney function",
                              "Common municipal","Municipal","Commonwell",
                              "Home drinking water source",
                              "Study visit",'Seafood','Smoker',
                              "BMI","Age","Demographics and diet"))+
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Percent difference (95% CI)") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 6.5,face=bold.labels,hjust=indent.labels),
        axis.ticks.y=element_blank())+
  facet_wrap(metal~.,scales="free_x",labeller = newlab)
#ggsave("..//results/metregR1R6plot.jpg", height=7, width=8, units="in")



