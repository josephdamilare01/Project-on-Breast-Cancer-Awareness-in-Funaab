library(tidyverse)
library(summarytools)
library(readxl)
library("lubridate")
library(visdat)
library(dlookr)
library(missRanger)
library(vcd)


awareness <- read_excel(file.choose())


renamed <- awareness %>% select(Timestamp, Duration, Gender=`What is your gender?`,
                                 Age=`What is your age range?`,
                                 `Knowledge of BC`=`Do you have Knowledge regarding breast cancer?`,
                                 `Knowledge BSE`=`Do you have Knowledge regarding breast self-examination?`,
                                 `Practice BSE`=`Have you ever practiced breast self-examination?`,
                                `Regularly PBSE`=`Do you perform breast self-examination regularly?`,
                        `Posture PBSE`=`which body posture do you obtain in performing the breast self-examination?`,
                        `Time PBSE`=`what is the best time you perform the breast self-examination?`,
                        `Reason not PSBE`=`What reason for not practicing the self breast examination?`,
                        `Diagnosed with lumps`=`Have you ever been diagnosed with lumps in the breast?`,
                        `Family history`=`Do you have a family history of cancer?`,
                        `Screening method`=`which of the breast screening methods have heard about`,
                        `Discharge in Nipple`=`Have you ever observed a discharge in the nipples?`,
                        `Nipple position change`=`Do you observe a change in the position of the nipples recently?`,
                        `Color Discharge`=`If yes, what's the color of discharge observed?`)
plot(vis_miss(renamed%>%select(-c(Timestamp,Duration))))
plot_na_pareto(renamed%>%select(-c(Timestamp,Duration)), only_na = T)
plot_na_pareto(renamed%>%select(-c(Timestamp,Duration)), col="blue")

J <- renamed %>% 
  diagnose %>% 
  arrange(desc(missing_count))

print(J)

p <- awareness %>% select(Duration) %>% group_by(Duration) %>% 
  summarize( n = n()) %>% as.data.frame() 
p$Duration <- as.Date(p$Duration)
p%>% 
  ggplot(aes(x = Duration, y= n))+geom_line(col= "red", shape = 1, size =2) +geom_text(aes(label = n), hjust = -0.2, vjust = 0.5, col = "black") + ylab("Frequencie")+
scale_x_date(date_breaks = "15 day", date_labels = "%Y-%m-%d") +
  theme_minimal() + theme_classic()+ # Make text bold
  labs(title = "Number of responses over the period ") +
  theme(  
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(color = "black", face = "bold", size =14),
        axis.text.y = element_text(color = "black", face = "bold", size =14))

awareness
######Descritpive Analysis
#Gender
dfSummary(awareness$`What is your gender?`)
#AGE
renamed %>% select(Age)%>% drop_na() %>% dfSummary()
renamed %>% select(`Knowledge of BC`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Knowledge of BC`)%>% drop_na() %>% group_by(`Knowledge of BC`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Knowledge of BC`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "Do you have Knowledge regarding breast cancer? ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Knowledge BSE`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Knowledge BSE`)%>% drop_na()%>% group_by(`Knowledge BSE`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Knowledge BSE`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5,vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ") + labs(title = "`Do you have Knowledge regarding breast self-examination?` ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Practice BSE`)%>% drop_na() %>% dfSummary()
renamed%>%  select(`Practice BSE`)%>% drop_na() %>% group_by(`Practice BSE`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Practice BSE`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ") + labs(title = "Have you ever practiced breast self-examination?` ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Regularly PBSE`)%>% drop_na() %>% dfSummary()
renamed%>%  select(`Regularly PBSE`)%>% drop_na() %>% group_by(`Regularly PBSE`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Regularly PBSE`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5,vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "Do you perform breast self-examination regularly? ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Posture PBSE`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Posture PBSE`)%>% drop_na() %>% group_by(`Posture PBSE`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Posture PBSE`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "which body posture do you obtain in performing the breast self-examination? ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Time PBSE`)%>% drop_na() %>% dfSummary()
renamed%>%  select(`Time PBSE`)%>% drop_na() %>% group_by(`Time PBSE`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Time PBSE`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "what is the best time you perform the breast self-examination?") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14, angle = 45, hjust=1),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Reason not PSBE`)%>% drop_na() %>% dfSummary()
renamed%>%  select(`Reason not PSBE`)%>% drop_na() %>% group_by(`Reason not PSBE`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Reason not PSBE`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "`What reason for not practicing the self breast examination?` ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Diagnosed with lumps`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Diagnosed with lumps`)%>% drop_na() %>% group_by(`Diagnosed with lumps`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Diagnosed with lumps`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5,vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "Have you ever been diagnosed with lumps in the breast? ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Family history`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Family history`)%>% drop_na()  %>% group_by(`Family history`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Family history`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "Do you have a family history of cancer? ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Screening method`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Screening method`)%>% drop_na() %>% group_by(`Screening method`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Screening method`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "which of the breast screening methods have heard about ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Discharge in Nipple`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Discharge in Nipple`) %>%drop_na() %>% group_by(`Discharge in Nipple`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Discharge in Nipple`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "Have you ever observed a discharge in the nipples?") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Nipple position change`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Nipple position change`) %>%drop_na() %>% group_by(`Nipple position change`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Nipple position change`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "Do you observe a change in the position of the nipples recently? ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

renamed %>% select(`Color Discharge`)%>% drop_na() %>% dfSummary()
renamed%>% select(`Color Discharge`) %>%drop_na() %>% group_by(`Color Discharge`) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(`Color Discharge`, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "If yes, what's the color of discharge observed?") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))



renamed%>% select(Age) %>%drop_na() %>% group_by(Age) %>% summarise(count = n()) %>%
  mutate(percent = (count / sum(count) * 100)) %>% arrange(percent)%>% ggplot(aes(x=reorder(Age, percent), y= percent))+geom_bar(stat="identity", fill="red")+
  geom_text(aes(label = paste0(round(percent,1), "%")), size =5, vjust=-0.9, col = "black") +
  ylab(" ") + xlab(" ")+ labs(title = "I have the knowlege of breast cancer ") +
  theme(  
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 14),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black", face = "bold", size =14),
    axis.text.y = element_text(color = "black", face = "bold", size =14))

###Recoding: Changing the labels to diachotomous, polychotomous, trichotomous  variables 


###Checking the sample size left after dropping the empty
before <- awareness %>% count()
after <- renamed %>% count()
Sample_left <- 
 renamed_decoded <- renamed %>%
   mutate(
     Age = recode(Age, "16 -20" = 0, "21 -25" = 1, "26 -30" = 2),
     `Knowledge of BC` = recode(`Knowledge of BC`, "No" = 0, "Yes" = 1),
     `Knowledge BSE` = recode(`Knowledge BSE`, "No" = 0, "Yes" = 1),
     `Practice BSE` = recode(`Practice BSE`, "No" = 0, "Yes" = 1),
     `Regularly PBSE` = recode(`Regularly PBSE`, "Not regular" = 0, "Regular" = 1),
     `Posture PBSE` = recode(`Posture PBSE`, "Laying" = 0, "Sitting" = 1, "Standing" = 2),
     `Time PBSE` = recode(`Time PBSE`, "I don’t know" = 0, "Just after menstruation" = 1, "During menstruation" = 2),
     `Reason not PSBE` = recode(`Reason not PSBE`, "Don’t known" = 0, "Don’t know what I should do" = 1, "Don’t have a breast" = 2),
     `Diagnosed with lumps` = recode(`Diagnosed with lumps`, "No" = 0, "Yes" = 1),
     `Family history` = recode(`Family history`, "No" = 0, "Yes" = 1),
     `Screening method` = recode(`Screening method`, "None" = 0, "Mammogram" = 1, "Breast self- examination" = 2, "I have heard about both" = 3),
     `Discharge in Nipple` = recode(`Discharge in Nipple`, "No" = 0, "Yes" = 1),
     `Nipple position change` = recode(`Nipple position change`, "No" = 0, "Yes" = 1),
     `Color Discharge` = recode(`Color Discharge`, "Greenish brown" = 0, "Yellow" = 1, "Crystal clear" = 2)
   )
 

###Removing Timestamp, Duration, and Gender
renamed_decoded <- renamed_decoded %>% select(-c(Timestamp, Duration, Gender))
#Do you have Knowledge regarding breast self-examination?

# Create a contingency table

cont_table1 <- table(renamed_decoded$`Knowledge of BC`, renamed_decoded$Age) 
cont_table2 <- table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Practice BSE` ) 
cont_table3 <- table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Regularly PBSE` ) 
cont_table4 <- table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Posture PBSE` ) 
cont_table5 <- table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Time PBSE` ) 
cont_table6 <- table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Reason not PSBE`) 
cont_table7 <- table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Diagnosed with lumps`) 
cont_table8 <- table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Family history`) 
cont_table9 <-table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Screening method`) 
cont_table10 <-table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Discharge in Nipple`) 
cont_table11 <-table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Nipple position change`) 
cont_table12 <-table(renamed_decoded$`Knowledge of BC`, renamed_decoded$`Color Discharge`) 


# Calculate Cramer's V statistic
assocstats(cont_table1)$chisq_tests
assocstats(cont_table2)$chisq_tests
assocstats(cont_table3)$chisq_tests
assocstats(cont_table4)$chisq_tests
assocstats(cont_table5)$chisq_tests
assocstats(cont_table6)$chisq_tests
assocstats(cont_table7)$chisq_tests
assocstats(cont_table8)$chisq_tests
assocstats(cont_table9)$chisq_tests
assocstats(cont_table10)$chisq_tests
assocstats(cont_table11)$chisq_tests
assocstats(cont_table12)$chisq_tests
# Cramer'V
a<-assocstats(cont_table1)$cramer
b<-assocstats(cont_table2)$cramer
c<-assocstats(cont_table3)$cramer
d<-assocstats(cont_table4)$cramer
e<-assocstats(cont_table5)$cramer
f<-assocstats(cont_table6)$cramer
g<-assocstats(cont_table7)$cramer
h<-assocstats(cont_table8)$cramer
i<-assocstats(cont_table9)$cramer
j<-assocstats(cont_table10)$cramer
k<-assocstats(cont_table11)$cramer
l<-assocstats(cont_table12)$cramer

# Contingency Coeff.
aa<-assocstats(cont_table1)$contingency
ba<-assocstats(cont_table2)$contingency
ca<-assocstats(cont_table3)$contingency
da<-assocstats(cont_table4)$contingency
ea<-assocstats(cont_table5)$contingency
fa<-assocstats(cont_table6)$contingency
ga<-assocstats(cont_table7)$contingency
ha<-assocstats(cont_table8)$contingency
ia<-assocstats(cont_table9)$contingency
ja<-assocstats(cont_table10)$contingency
ka<-assocstats(cont_table11)$contingency
la<-assocstats(cont_table12)$contingency
# Phi-coefficient
ab<-assocstats(cont_table1)$phi
bb<-assocstats(cont_table2)$phi
cb<-assocstats(cont_table3)$phi
db<-assocstats(cont_table4)$phi
eb<-assocstats(cont_table5)$phi
fb<-assocstats(cont_table6)$phi
gb<-assocstats(cont_table7)$phi
hb<-assocstats(cont_table8)$phi
ib<-assocstats(cont_table9)$phi
jb<-assocstats(cont_table10)$phi
kb<-assocstats(cont_table11)$phi
lb<-assocstats(cont_table12)$phi
#Combining the values 
Cramer_value <- data.frame( Variable = c("Age", 
                            "Practice BSE", 
                            "Regularly PBSE",  
                            "Posture PBSE" ,
                            "Time PBSE",
                            "Reason not PSBE",
                            "Diagnosed with lumps", 
                            "Family history",
                            "Screening method",
                            "Discharge in Nipple",
                            "Nipple position change", 
                            "Color Discharge"), Cramer_v_values = rbind(a,b,c,d,e,f,g,h,i,j,k,l),
                            Contingency_Coeff= rbind(aa,ba,ca,da,ea,fa,ga,ha,ia,ja,ka,la),
                            Phi_coefficient = rbind(ab,bb,cb,db,eb,fb,gb,hb,ib,jb,kb,lb))



#Do you have Knowledge regarding breast cancer?

####################################################################
cont_t1 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$Age) 
cont_t2 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Practice BSE` ) 
cont_t3 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Regularly PBSE` ) 
cont_t4 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Posture PBSE` ) 
cont_t5 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Time PBSE` ) 
cont_t6 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Reason not PSBE`) 
cont_t7 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Diagnosed with lumps`) 
cont_t8 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Family history`) 
cont_t9 <- table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Screening method`) 
cont_t10 <-table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Discharge in Nipple`) 
cont_t11 <-table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Nipple position change`) 
cont_t12 <-table(renamed_decoded$`Knowledge BSE` , renamed_decoded$`Color Discharge`)

# Calculate Cramer's V statistic
assocstats(cont_t1)$chisq_tests
assocstats(cont_t2)$chisq_tests
assocstats(cont_t3)$chisq_tests
assocstats(cont_t4)$chisq_tests
assocstats(cont_t5)$chisq_tests
assocstats(cont_t6)$chisq_tests
assocstats(cont_t7)$chisq_tests
assocstats(cont_t8)$chisq_tests
assocstats(cont_t9)$chisq_tests
assocstats(cont_t10)$chisq_tests
assocstats(cont_t11)$chisq_tests
assocstats(cont_t12)$chisq_tests
# Cramer'V
aq<-assocstats(cont_t1)$cramer
bq<-assocstats(cont_t2)$cramer
cq<-assocstats(cont_t3)$cramer
dq<-assocstats(cont_t4)$cramer
eq<-assocstats(cont_t5)$cramer
fq<-assocstats(cont_t6)$cramer
gq<-assocstats(cont_t7)$cramer
hq<-assocstats(cont_t8)$cramer
iq<-assocstats(cont_t9)$cramer
jq<-assocstats(cont_t10)$cramer
kq<-assocstats(cont_t11)$cramer
lq<-assocstats(cont_t12)$cramer

# Contingency Coeff.
aw<-assocstats(cont_t1)$contingency
bw<-assocstats(cont_t2)$contingency
cw<-assocstats(cont_t3)$contingency
dw<-assocstats(cont_t4)$contingency
ew<-assocstats(cont_t5)$contingency
fw<-assocstats(cont_t6)$contingency
gw<-assocstats(cont_t7)$contingency
hw<-assocstats(cont_t8)$contingency
iw<-assocstats(cont_t9)$contingency
jw<-assocstats(cont_t10)$contingency
kw<-assocstats(cont_t11)$contingency
lw<-assocstats(cont_t12)$contingency
# Phi-coefficient
abq<-assocstats(cont_table1)$phi
bbq<-assocstats(cont_table2)$phi
cbq<-assocstats(cont_table3)$phi
dbq<-assocstats(cont_table4)$phi
ebq<-assocstats(cont_table5)$phi
fbq<-assocstats(cont_table6)$phi
gbq<-assocstats(cont_table7)$phi
hbq<-assocstats(cont_table8)$phi
ibq<-assocstats(cont_table9)$phi
jbq<-assocstats(cont_table10)$phi
kbq<-assocstats(cont_table11)$phi
lbq<-assocstats(cont_table12)$phi
#Combing the values to a dataframe
CramerValue <- data.frame( Variable = c("Age", 
                                         "Practice BSE", 
                                         "Regularly PBSE",  
                                         "Posture PBSE" ,
                                         "Time PBSE",
                                         "Reason not PSBE",
                                         "Diagnosed with lumps", 
                                         "Family history",
                                         "Screening method",
                                         "Discharge in Nipple",
                                         "Nipple position change", 
                                         "Color Discharge"), Cramer_v_values = rbind(aq,bq,cq,dq,eq,fq,gq,hq,iq,jq,kq,lq),
                            Contingency_Coeff= rbind(aw,bw,cw,dw,ew,fw,gw,hw,iw,jw,kw,lw),
                            Phi_coefficient = rbind(abq,bbq,cbq,dbq,ebq,fbq,gbq,hbq,ibq,jbq,kbq,lbq))

CramerValue

