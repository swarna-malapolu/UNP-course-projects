data <- read.csv("C:/Users/bhaskarnani/OneDrive/Desktop/unp1/Response1.csv",header=T) 
head(data) 
colnames(data)

colnames(data) <- c("Gender","Age","Qualification","Occupation","AnnualIncome","EV","Costlier","MoreRange",
                    "Expensive","MoreSpeed","NotPowerful","ChargingStat","DistanceBwStat","Feel","OwningEV",
                    "Advantages","Govt","ChareTime","TATANEXON","MGZSEV","TATATIGOREV","BMWi_ERIES","TATAULTRAT")
head(data) 
colnames(data)

#EDA
data %>% filter(!Occupation =="Others",!Occupation=="Transportation Department") %>% ggplot(aes(Occupation,fill=Occupation)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle=90, hjust =1))

data %>%  
  ggplot(aes(Qualification,fill=Qualification)) + 
  geom_bar() + ggtitle("Qualification") +
  theme(axis.text.x=element_text(angle = 90,hjust=1)) 


data %>% 
  filter(Occupation == c("Private employee","Students")) %>%
  ggplot(aes(Occupation,fill=Occupation)) + 
  geom_bar() + facet_grid(Gender~.) +
  theme(axis.text.x=element_text(angle = 90,hjust=1)) 

#barplot for Gender vs EV
filter(data,Gender %in% c("Male","Female")) %>% 
  ggplot(aes(EV,fill = EV)) + 
  geom_bar()+facet_grid(Gender~EV)

filter(data, Gender %in% c("Male")) %>%
  ggplot(aes(EV,)) + geom_bar()

filter(data,Gender %in% c("Male","Female") ) %>%
  ggplot(aes(Occupation, EV)) +
  geom_point() +theme(axis.text.x=element_text())

#mutating columns maintainence will be expensive variable
data <- data %>% mutate(SAexp = ifelse(data$Expensive=="Strongly agree",1,0))
data <- data %>% mutate(Aexp = ifelse(data$Expensive=="Agree",1,0))
data <-  data %>% mutate(idkexp = ifelse(data$Expensive=="I don't know",1,0))
data <- data %>% mutate(Dexp = ifelse(data$Expensive=="Disgaree",1,0))
data <- data %>% mutate(SDexp = ifelse(data$Expensive=="Strongly disgree",1,0))
data <- data %>% select(-c("Expensive"))

#mutating gender
data  <- data %>% mutate(Male=ifelse(Gender=="Male",1,0))
data  <- data %>% mutate(Female=ifelse(Gender=="Female",1,0))
data <- data %>% select(-c("Gender"))

#Age
data <- data %>% mutate(young = ifelse(Age == "20 or younger",1,0))
data <- data %>% mutate(t2130 = ifelse(Age == "21-30",1,0))
data <- data %>% mutate(t3140 = ifelse(Age == "31-40",1,0))
data <- data %>% mutate(t4150 = ifelse(Age == "41-50",1,0))
data <- data %>% mutate(t5160 = ifelse(Age == "51-60",1,0))
data <- data %>% select(-c(Age))

#mutating columns charging time will be barrier for buying EV
data <- data %>% mutate(yes_CT = ifelse(ChareTime=="Yes",1,0))
data <- data %>% mutate(No_CT = ifelse(ChareTime=="No",1,0))
data <- data %>% mutate(Maybe_CT = ifelse(ChareTime=="Maybe",1,0))
data <- data %>% select(-c(ChareTime))

#mutation columns costlier
data <- data %>% mutate(yes_costlier = ifelse(Costlier=="Yes",1,0))
data <- data %>% mutate(No_costlier = ifelse(Costlier=="No",1,0))
data <- data %>% mutate(MCostlier = ifelse(Costlier=="Maybe",1,0))
data %>% select(-c(Costlier))

#range to 0 and 1
data <- data %>% mutate(SArange = ifelse(MoreRange == "Strongly agree",1,0))
data <- data %>% mutate(Arange = ifelse(MoreRange == "Agree",1,0))
data <- data %>% mutate(idkrange = ifelse(MoreRange == "I don't know",1,0))
data <- data %>% mutate(Drange = ifelse(MoreRange == "Disagree",1,0))
data <- data %>% mutate(SDrange = ifelse(MoreRange == "Strongly disagree",1,0))
data  <- data %>% select(-c(MoreRange))

#Occupation
data <- data %>% mutate(Student = ifelse(Occupation == "Student",1,0))
data <- data %>% mutate(govtemp = ifelse(Occupation == "Government employee",1,0))
data <- data %>% mutate(pvtemp = ifelse(Occupation == "Private employee",1,0))
data <- data %>% mutate(hw = ifelse(Occupation == "House Wives",1,0))
data <- data %>% mutate(retired = ifelse(Occupation == "Retired",1,0))
data <- data %>% mutate(self = ifelse(Occupation == "Self employee",1,0))
data <- data %>% select(-c(Occupation))



#Government role
data <- data %>% mutate(Agovt = ifelse(Govt == "Agree",1,0))
data <- data %>% mutate(Dgovt = ifelse(Govt == "Disagree",1,0))
data <- data %>% mutate(Ngovt = ifelse(Govt == "Neutral",1,0))
data <- data %>% select(-c(Govt,Qualification,Occupation,EV,AnnualIncome,MoreSpeed,
                           NotPowerful,ChargingStat,DistanceBwStat,Feel,Advantages,
                           TATANEXON,TATATIGOREV,TATAULTRAT,MGZSEV))
data %>% select(-c(`BMWi_ERIES`))

train <- data[1:100,]
test <- data[101:130,]

#remove Dgovt, Ngovt, SDrange,MCostlier,Maybe_CT, Female,Dexp. SDexp,t5160
logisticmodel <- glm(OwningEV ~ Agovt +
                       SArange + Arange + idkrange + Drange +
                       yes_costlier + No_costlier +
                       yes_CT + No_CT +  Male + 
                       SAexp + Aexp + idkexp   +
                       young + t2130 + t3140 + t4150 ,family = binomial(), data=train)

summary(logisticmodel)

prediction <- predict(logisticmodel,test)
prediction

