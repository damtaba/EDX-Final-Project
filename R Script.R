############################################################
######################## | Data Loading | ##################
############################################################

#Importing de csv downloaded from https://www.kaggle.com/sakshigoyal7/credit-card-customers. 
#It will also be provided in my GitHub repo(https://github.com/damtaba/EDX-Final-Project) along with the rest of your files 

data<- read.csv(file="BankChurners.csv")


#There are two variables that were calculated later and are not part of the original base. We'll remove them
data$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1 <- NULL
data$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2 <- NULL

str(data)

############################################################
######################## | Packages | ######################
############################################################

# Package names we'll need
packages <- c("tidyverse",
              "rmarkdown",
              "fastDummies",
              "caret",
              "fastDummies",
              "knitr",
              "gmodels",
              "C50",
              "lubridate"
              )

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

############################################################
###################### | Data Analysis | ###################
############################################################

########## Attrition_Flag
data %>% 
  group_by(Attrition_Flag) %>%
  summarize(n=n()) %>%
  mutate(Participation=n/sum(n)) %>%
  kable()

########## Customer_Age
summary(data$Customer_Age) #Fist aproach

data %>%
  ggplot(aes(x=Customer_Age)) +
  geom_bar() +
  ggtitle("Distribution of Customer's Ages") +
  xlab("Age") +
  ylab("n") # Graphically, we can anticipate that the age distribution resembles a normal distribution

shapiro.test(sample(data$Customer_Age,5000)) #Shapiro-Wilk Test, a test 

########## Gender
#At first glance it does not seem that gender can be a good predictor; is almost evenly distributed
CrossTable(data$Gender,data$Attrition_Flag,prop.chisq = FALSE) 

########## Dependent_count
summary(data$Customer_Age)

CrossTable(data$Dependent_count,data$Attrition_Flag,prop.chisq =  FALSE,prop.t = FALSE,prop.c = FALSE)

########## Education_Level
data %>%
  group_by(Education_Level) %>%
  summarize(Q=n()) %>%
  mutate(porc=Q/sum(Q)) %>%
  arrange(desc(porc)) %>%
  ggplot(aes(x="",y=Q,fill=Education_Level)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+ #Converting to pie chart
  theme_void #Cleaning the graph

data %>%
  group_by(Education_Level) %>%
  summarize(Q=n()) %>%
  mutate(Participation=Q/sum(Q)) %>%
  arrange(desc(Participation)) %>%
  kable()

########## Marital_Status
data %>%
  group_by(Marital_Status) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(Participation=n/sum(n)) %>%
  arrange(desc(Participation)) %>%
  kable()

data %>%
  group_by(Marital_Status,Attrition_Flag) %>%
  summarize(n=n()) %>%
  mutate(Participation=format(round(n/sum(n)*100), 2), nsmall = 2) %>%
  ggplot(aes(x=Marital_Status,y=Participation,fill=Attrition_Flag))+
  geom_bar(stat = "identity")+
  geom_text(aes(x=Marital_Status,y=Participation,label=Participation,vjust=-2))+
  ggtitle("Percentage of participation of the type of client by Marital Status")

########## Income_Category
data %>%
  group_by(Income_Category) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(Participation=n/sum(n)) %>%
  arrange(desc(Participation)) %>%
  kable()

data %>%
  group_by(Attrition_Flag,Income_Category) %>%
  summarize(Q=n()) %>%
  ungroup() %>% group_by(Income_Category) %>%
  mutate(Participation=Q/sum(Q)) %>%
  mutate(Participation=format(round(Participation*100,digits = 0),digits = 4)) %>%
  ggplot(aes(x=Income_Category, y = Participation, fill = Attrition_Flag))+
  geom_bar(stat="identity")+
  coord_flip()+
  ggtitle("Percentage of Income_Category by Attrition_Flag")+
  geom_text(aes(x=Income_Category, y = Participation,label=Participation),hjust=-0.25)

########## Card_Category
data %>%
  group_by(Card_Category,Attrition_Flag) %>%
  summarize(Q=n()) %>%
  mutate(Participation=Q/sum(Q)) %>%
  mutate(Participation=format(round(Participation,digits = 4),digits = 4)) %>%
  ggplot(aes(x=Card_Category,y=Participation,fill=Attrition_Flag))+
  geom_col()+
  ggtitle("Card_Category by Attrition_Flag")+
  geom_text(aes(x=Card_Category, y = Participation,label=Participation,vjust=-0.45))+
  facet_wrap(~ Card_Category)

data %>%
  group_by(Card_Category) %>%
  summarize(Q=n()) %>%
  mutate(Participation=Q/sum(Q)) %>%
  kable()

########## Months_on_book
data %>%
  group_by(Months_on_book) %>%
  summarize(n=n()) %>%
  arrange(desc(Months_on_book)) %>%
  ggplot(aes(x=Months_on_book,y=n)) +
  geom_line()+
  geom_col(alpha=0.5)+
  ggtitle("Months on book")

########## Total_Relationship_Count
data %>%
  group_by(Total_Relationship_Count) %>%
  summarize(n=n()) %>%
  ggplot(aes(Total_Relationship_Count,n))+
  geom_point()+
  geom_line()+
  ggtitle("Total Relationship Count")

########## Months_Inactive_12_mon
data %>%
  group_by(Months_Inactive_12_mon) %>%
  summarize(n=n()) %>%
  kable()


data %>%
  group_by(Months_Inactive_12_mon,Attrition_Flag) %>%
  summarize(n=n()) %>%
  ggplot(aes(Months_Inactive_12_mon,n,fill=Attrition_Flag))+
  geom_col(position="fill")+
  geom_line()

########## Contacts_Count_12_mon

data %>%
  group_by(Contacts_Count_12_mon) %>%
  summarize(n=n()) %>%
  mutate(Participation=n/sum(n)) %>%
  arrange(Contacts_Count_12_mon) 

########## Credit_Limit               

data %>%
  group_by(Attrition_Flag,Credit_Limit) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=Credit_Limit,y=n,fill=Attrition_Flag)) +
  geom_col(width = 2500)+
  ylim(0,25)+
  ggtitle("Composition of Credit_Limit by Attrition_Flag")

########## Total_Revolving_Bal
summary(data$Total_Revolving_Bal)

data %>%
  group_by(Total_Revolving_Bal) %>%
  summarize(n=n()) %>%
  mutate(porc=n/sum(n)) %>%
  arrange(desc(porc))

data %>%
  group_by(Total_Revolving_Bal) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=Total_Revolving_Bal,y=n))+
  geom_col(width = 100)+
  ggtitle("Distribution of Customers by Total Revolving Limit")

########## Avg_Open_To_Buy
data %>%
  group_by(Avg_Open_To_Buy) %>%
  summarize(n=n()) %>%
  mutate(porc=n/sum(n)) %>%
  ggplot(aes(Avg_Open_To_Buy,n)) +
  geom_col(width = 1000)+
  ggtitle("Average Open to Buy")

########## Total_Amt_Chng_Q4_Q1 
data %>%
  group_by(Total_Amt_Chng_Q4_Q1) %>%
  summarize(n=n()) %>%
  mutate(porc=n/sum(n)) %>%
  ggplot(aes(Total_Amt_Chng_Q4_Q1,n))+
  geom_col(width = 0.1)+
  ggtitle("Total_Amt_Chng_Q4_Q1")

########## Total_Trans_Amt 
data %>%
  group_by(Total_Trans_Amt,Attrition_Flag) %>%
  summarize(Q=n()) %>%
  mutate(porc=Q/sum(Q)) %>%
  arrange(desc(porc)) %>%
  ggplot(aes(Total_Trans_Amt,Q,fill=Attrition_Flag))+
  geom_col(width = 500)+
  ggtitle("Total_Trans_Amt by Attrition_Flag")

########## Total_Trans_Ct
data %>%
  group_by(Total_Trans_Ct,Attrition_Flag) %>%
  summarize(Q=n()) %>%
  mutate(porc=Q/sum(Q)) %>%
  arrange(desc(porc)) %>%
  ggplot(aes(Total_Trans_Ct,Q,fill=Attrition_Flag))+
  geom_col()+
  ggtitle("Total_Trans_Ct by Attrition_Flag")

########## Total_Ct_Chng_Q4_Q1
data %>%
  group_by(Total_Ct_Chng_Q4_Q1,Attrition_Flag) %>%
  summarize(Q=n()) %>%
  mutate(porc=Q/sum(Q)) %>%
  arrange(desc(porc)) %>%
  ggplot(aes(Total_Ct_Chng_Q4_Q1,Q,fill=Attrition_Flag))+
  geom_col()+
  ylim(0,100)+
  xlim(0,2)+
  ggtitle("Total_Ct_Chng_Q4_Q1 by Attrition_Flag")

########## Avg_Utilization_Ratio
data %>%
  group_by(Avg_Utilization_Ratio,Attrition_Flag) %>%
  summarize(Q=n()) %>%
  mutate(porc=Q/sum(Q)) %>%
  arrange(desc(porc)) %>%
  ggplot(aes(Avg_Utilization_Ratio,Q,fill=Attrition_Flag))+
  geom_col(width = 0.2)+
  ggtitle("Distribution of Avg_Utilization_Ratio by Attrition_Flag")


############################################################
############# | Pre-processing the dataset  | ##############
############################################################
#Making the dummy's features
data_flags <- dummy_cols(data,remove_selected_columns = TRUE,select_columns = c("Gender","Education_Level","Marital_Status","Income_Category","Card_Category"))

#Factor variables that are mutually exclusive can be separated into n-1 dummy variables. 
#This means, if it is not any of the above, it is by default the one that remains, being one of the dispensable #dummys variables
data_flags <- data_flags %>%
  mutate(Churn_Customers= ifelse(Attrition_Flag =="Attrited Customer",1,0)) %>%
  select(-Attrition_Flag,
         -Gender_M,
         -Education_Level_Doctorate,
         -Marital_Status_Unknown,
         -`Income_Category_$120K +`,
         -Card_Category_Platinum,
         -CLIENTNUM #Does not have any information apart fromthe ID
  )

str(data_flags)

#### Correlations
correlationMatrix <- cor(data_flags[,1:32],method="pearson")

highlyCorrelatedNames <- findCorrelation(correlationMatrix, cutoff=0.5,names=TRUE)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5,names=FALSE)

data_model <- data_flags[,-highlyCorrelated]
kable(highlyCorrelatedNames,col.names = "This are the variables highly correlated")

str(data_flags)
############################################################
####################### | Models  | #########################
############################################################

#Using a 50/50 division of the data
index_test <- createDataPartition(data_model$Churn_Customers,p=0.5,list=FALSE) 

test_set <- data_flags[index_test,]
train_set <- data_flags[-index_test,]

################### Knn
#Creating the function that normalizes features
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))} 

#Creating both datasets with the new scale
train_features_n <- as.data.frame(lapply(train_set[1:32],normalize))
test_features_n <- as.data.frame(lapply(test_set[1:32],normalize))

#Making the model
model_knn_n <- train(train_features_n,as.factor(train_set[,33]),method = "knn") 

#Using this model to predict
y_hat_knn_n_model <- predict(model_knn_n,test_features_n) 

#Checking the proportion of right cases
Mean_knn_n <- mean(test_set$Churn_Customers==y_hat_knn_n_model) 

#Creating a table to fill it with the results
Results_table <- data.frame(Model=character(),Mean=numeric(),stringsAsFactors = FALSE) 

#Creating the list for this model
Results_knn_n <- list("Knn normalized",Mean_knn_n)

# Adding this model's results to the table
Results_table[1,] <- Results_knn_n 
Results_knn_n

###z-score
# Scaling the data
train_features_s <- as.data.frame(scale(train_set[,1:32]))
test_features_s <- as.data.frame(scale(test_set[,1:32]))

#Making the model
model_knn_s <- train(train_features_s[,1:32],as.factor(train_set[,33]),method = "knn")

#Using the model to predict
y_hat_knn_s_model <- predict(model_knn_s,test_features_s)

#Checking the proportion of right cases
Mean_kss_s <- mean(test_set$Churn_Customers==y_hat_knn_s_model) 

#Creating the list for this model
Results_knn_s <- list("Knn with scale z-score",Mean_kss_s)

# Adding this model's results to the table
Results_table[2,] <-Results_knn_s

Results_knn_s

###### Decision Tree C5.0
# Knn needed numerical features. Now that that is not needed, some of them should be factors

#Transforming into factorial some features in the train set
train_set_C5.0 <- train_set %>% mutate(
  Gender_F = as.factor(Gender_F),
  Education_Level_College= as.factor(Education_Level_College),
  Education_Level_Graduate=as.factor(Education_Level_Graduate),
  Education_Level_High_school = as.factor(`Education_Level_High School`),
  Education_Level_Post_Graduate = as.factor(`Education_Level_Post-Graduate`),
  Education_Level_Uneducated = as.factor(Education_Level_Uneducated),
  Education_Level_Unknown=as.factor(Education_Level_Unknown),
  Marital_Status_Divorced=as.factor(Marital_Status_Divorced),
  Marital_Status_Married = as.factor(Marital_Status_Married),
  Marital_Status_Single=as.factor(Marital_Status_Single),
  Income_Category_40K_60K= as.factor(`Income_Category_$40K - $60K`),
  Income_Category_60K_80K = as.factor(`Income_Category_$60K - $80K`),
  Income_Category_80K_120K=as.factor(`Income_Category_$80K - $120K`),
  Income_Category_Less_than_40K = as.factor(`Income_Category_Less than $40K`),
  Income_Category_Unknown = as.factor(Income_Category_Unknown),
  Card_Category_Blue = as.factor(Card_Category_Blue),
  Card_Category_Gold = as.factor(Card_Category_Gold),
  Card_Category_Silver = as.factor(Card_Category_Silver),
  Churn_Customers = as.factor(Churn_Customers)
) %>%
  select(
    -`Education_Level_High School`,
    -`Education_Level_Post-Graduate`,
    -`Income_Category_$40K - $60K`,
    -`Income_Category_$60K - $80K`,
    -`Income_Category_$80K - $120K`,
    -`Income_Category_Less than $40K`
  )

#Transforming into factorial some features in the test set
test_set_C5.0 <- test_set %>% mutate(
  Gender_F = as.factor(Gender_F),
  Education_Level_College= as.factor(Education_Level_College),
  Education_Level_Graduate=as.factor(Education_Level_Graduate),
  Education_Level_High_school = as.factor(`Education_Level_High School`),
  Education_Level_Post_Graduate = as.factor(`Education_Level_Post-Graduate`),
  Education_Level_Uneducated = as.factor(Education_Level_Uneducated),
  Education_Level_Unknown=as.factor(Education_Level_Unknown),
  Marital_Status_Divorced=as.factor(Marital_Status_Divorced),
  Marital_Status_Married = as.factor(Marital_Status_Married),
  Marital_Status_Single=as.factor(Marital_Status_Single),
  Income_Category_40K_60K= as.factor(`Income_Category_$40K - $60K`),
  Income_Category_60K_80K = as.factor(`Income_Category_$60K - $80K`),
  Income_Category_80K_120K=as.factor(`Income_Category_$80K - $120K`),
  Income_Category_Less_than_40K = as.factor(`Income_Category_Less than $40K`),
  Income_Category_Unknown = as.factor(Income_Category_Unknown),
  Card_Category_Blue = as.factor(Card_Category_Blue),
  Card_Category_Gold = as.factor(Card_Category_Gold),
  Card_Category_Silver = as.factor(Card_Category_Silver),
  Churn_Customers = as.factor(Churn_Customers)
) %>%
  select(
    -`Education_Level_High School`,
    -`Education_Level_Post-Graduate`,
    -`Income_Category_$40K - $60K`,
    -`Income_Category_$60K - $80K`,
    -`Income_Category_$80K - $120K`,
    -`Income_Category_Less than $40K`
  )


#Making the model
model_c5o <- C5.0(Churn_Customers ~.,data=train_set_C5.0)

#Using the model to predict
y_hat_c50 <- predict(model_c5o,test_set_C5.0)

# Summary of the model
summ_c50 <- summary(model_c5o) 

#Checking the proportion of right cases
mean_c50 <- mean(y_hat_c50==test_set_C5.0$Churn_Customers)

#Creating the list for this model
Results_c50 <- list("C5.0 Decision Tree",mean_c50)

# Adding this model's results to the table
Results_table[3,] <- Results_c50

Results_c50

# Boosting
#Making the model
model_c5o_boosted <- C5.0(Churn_Customers ~.,data=train_set_C5.0,trials = 20) 

#Using the model to predict
y_hat_c50_boosted <- predict(model_c5o_boosted,test_set_C5.0) 

# Summary of the model
summ_c50_boosted <- summary(model_c5o_boosted) 

#Checking the proportion of right cases
mean_c50_boosted <- mean(y_hat_c50_boosted==test_set_C5.0$Churn_Customers)

#Creating the list for this model
Results_c50_boosted <- list("C5.0 Decision Tree Boosted",mean_c50_boosted)

# Adding this model's results to the table
Results_table[4,] <- Results_c50_boosted

Results_c50_boosted

############################################################
####################### | Results  | #########################
############################################################
Results_table %>% 
  mutate("Correct predictions" = round(Results_table$Mean,digits = 4)) %>%
  select(-Mean) %>%
  kable()
