library(ggplot2)
library(DescTools)
library(dplyr)
library(GGally)
library(plotROC) 
library(caTools)
library(class)
library(caret)
library(psych)
library(readxl)

# Load the dataset in R
df <- read_excel("hajdataset.xlsx")
View(df) # View the data
attach(df) # Attach the dataframe for direct access to column names

# Basic exploration of the dataset

dim(df) # Get the number of rows and columns
names(df) # View column names
head(df) # Preview the first few rows
summary(df) # Summarize data (numeric summaries, factor levels)
str(df) # Check the structure of the dataframe
describe(df) #to know the descreptive measures for all numerical variables

# Identify character columns
char_col <- which(sapply(df, is.character))

# Convert specified columns to factors
cat_columns <- c(4, 6, 7, 10, 11, 12, 14, 15, 17, 20, 21, 22, 23, 26, 28)
df[cat_columns] <- lapply(df[cat_columns], as.factor)

summary(df) # Summarize data after after redefining the variables
str(df) # Check the structure of the dataframe after redefining the variables 

# Extract factor levels from the dataframe
factor_levels <- sapply(df, function(col) {
  if (is.factor(col)) levels(col) else NULL
})
factor_levels <- factor_levels[!sapply(factor_levels, is.null)] # Remove non-factor entries

# Check for missing values
any(is.na(df)) 

# Generate box plots for numeric columns
num_col <- which(sapply(df, is.numeric)) # Identify numeric columns
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
for (col in names(df)[num_col]) {
  boxplot(df[[col]], main = paste("Boxplot of", col), col = "skyblue", border = "darkblue") # Plot each numeric column
}

#Descripe the data
#To descripe some categorical variables in data and know the count for each category

table(df$Crowd_Density)
table(df$Weather_Conditions)
table(df$Fatigue_Level)
table(df$Transport_Mode)

# two-way contingecy tables for some variables

table(df$Health_Condition,df$Age_Group)
table(df$Pilgrim_Experience,df$Stress_Level)
table(df$Crowd_Morale,df$Activity_Type)


# Crowd_Density  & Weather_Conditions pie charts
my_palette_default <- scales::hue_pal()(length(levels(df$Crowd_Density)))

crowed_pie = df %>%
  count(Crowd_Density) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = "", y = percent, fill = Crowd_Density)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = my_palette_default,
                    breaks = levels(df$Crowd_Density),
                    labels = c('high' , 'low' , 'medium')) +
  coord_polar(theta = "y") +
  labs(title = "Crowd_Density Distribution") +
  theme_void() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))

Weather_pie = df %>%
  count(Weather_Conditions) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = "", y = percent, fill = Weather_Conditions)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = my_palette_default,
                    breaks = levels(df$Weather_Conditions),
                    labels = c('clear' , 'Cloudy' , 'Rainy')) +
  coord_polar(theta = "y") +
  labs(title = "Weather_Conditions Distribution") +
  theme_void() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(crowed_pie, Weather_pie, ncol = 2)

####################
## bar chart for Transport_Mode & Fatigue_Level
Fatigue_Level_colors <- c("#044", "#66c", "#fd1")

Fatigue_Level_barchart <- df %>%
  count(Fatigue_Level) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = c("high", " low",
                   "medium"), y = percent, fill = Fatigue_Level)) +
  geom_bar(stat = "identity", width = 0.7, color = 'black') +
  scale_fill_manual(values = Fatigue_Level_colors ,
                    labels = c("high", " low",
                               "medium")) +
  labs(title = "Fatigue_Level bar chart" , x = 'Fatigue_Level') +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim = c(0, 0.45))   #Fatigue_Level bar chart



Transport_Mode_colors <- c("#a11", "#b66", "#c50" , "#d00") 

Transport_barchart <- df %>%
  count(Transport_Mode) %>%
  mutate(percent = n/sum(n)) %>%
  ggplot(aes(x = c("Walking", "car" , "bus" , "train"), y = percent, fill = Transport_Mode)) +
  geom_bar(stat = "identity", width = 0.7, color = 'black') +
  scale_fill_manual(values = Transport_Mode_colors ,
                    labels = c("Walking", "car" , "bus" , "train")) +
  labs(title = "Transport_barchart" , x = 'Transport_Mode') +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percent * 100), "%")),
            position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim = c(0, 0.6)) # Transport_Mode barchart

grid.arrange(Fatigue_Level_barchart, Transport_barchart, ncol = 2) # combine the 2 histograms in 1 plot 

#Bar Chart for Age_Group vs Health_Condition
p <- ggplot(df, aes(x=as.factor(Age_Group))) +
  geom_bar(aes(fill=as.factor(Health_Condition)), position="dodge") +  
  labs(
    title="Bar Chart for Age_Group vs Health_Condition",  
    x="Age_Group",   
    y="Count",   
    fill="Health_Condition"   
  ) +
  scale_fill_manual(values=c("#a22", "black", "#34a","#a56","#08f")) +  
  theme_minimal()  
print(p)

#Bar Chart for Stress_Level vs Pilgrim_Experience

p <- ggplot(df, aes(x=as.factor(Stress_Level))) +
  geom_bar(aes(fill=as.factor(Pilgrim_Experience)), position="dodge") +  
  labs(
    title="Bar Chart for Stress_Level vs Pilgrim_Experience",  
    x="Stress_Level",   
    y="Count",   
    fill="Pilgrim_Experience"   
  ) +
  scale_fill_manual(values=c("#a22", "black", "#34a")) +  
  theme_minimal()  
print(p)
#Bar Chart for Activity_Type vs Crowd_Morale
p <- ggplot(df, aes(x=as.factor(Activity_Type))) +
  geom_bar(aes(fill=as.factor(Crowd_Morale)), position="dodge") +  
  labs(
    title="Bar Chart for Activity_Type vs Crowd_Morale",  
    x="Activity_Type",   
    y="Count",   
    fill="Crowd_Morale"   
  ) +
  scale_fill_manual(values=c( "#08d" , "#12c" , "black")) +  
  theme_minimal()  
print(p)

# correlation matrix for quantitative variables

num_col <- which(sapply(df, is.numeric)) 
quan_data <- df[, num_col] 

library(GGally)
ggcorr(
  data = quan_data, 
  label = TRUE,             
  label_round = 2,            
  label_alpha = TRUE,         
  legend.size = 12,           
  legend.title = "Correlation Coefficient"
) +
  theme_bw() +
  ggtitle("Correlation between quantitative variable")

#############################
# Histograms for Location_Lat , Movement_Speed,Distance_Between_People_m , Temperature 
Location_Lat_histogram <- ggplot(df, aes(x = Location_Lat)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "#a12", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve for Location_Lat", x = "Location_Lat", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) #histogram for Location_Lat

Movement_Speed_histogram <- ggplot(df, aes(x = Movement_Speed)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "#a12", color = "black") +
  geom_density(color = "white") +
  labs(title = "Histogram with Density Curve for Movement_Speed", x = "Movement_Speed", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

Distance_Between_People_m_histogram <- ggplot(df, aes(x = Distance_Between_People_m)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "#a12", color = "black") +
  geom_density(color = "white") +
  labs(title = "Histogram with Density Curve for Distance_Between_People_m", x = "Distance_Between_People_m", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

Temperature_histogram <- ggplot(df, aes(x = Temperature)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#a12", color = "black") +
  geom_density(color = "white") +
  labs(title = "Histogram with Density Curve for Temperature", x = "Temperature", y = "Density") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(Location_Lat_histogram, Movement_Speed_histogram, ncol = 2) # combine the 2 histograms together 
grid.arrange(Distance_Between_People_m_histogram, Temperature_histogram, ncol = 2) # combine the 2 histograms together 


##########function#################
#average_temperature for  Weather_Conditions
weather_conditions <- unique(df$Weather_Conditions)
average_temperature <-c()
condition<-"Rainy"
for (condition in weather_conditions) {
  subset_data <- df[df$Weather_Conditions == condition, ]
  avg_temp <- mean(subset_data$Temperature)
  average_temperature[[condition]] <- avg_temp
}
print(average_temperature)

################################################################################
#Logistic Model :
#we want to make logistic model for the ordinal variable Fatigue_Level :
############################################
colnames(df)
summary(df)

library(MASS)
library(ordinal)

#binary logestic model:
levels(df$Fatigue_Level)
levels(df$Fatigue_Level)[1]<- "1"
levels(df$Fatigue_Level)[2]<- "0"
levels(df$Fatigue_Level)[3]<-"1"
levels(df$Fatigue_Level)

df$Fatigue_Level<-as.factor(df$Fatigue_Level)

library(dplyr)
df <- df %>%
  mutate(Row_ID = row_number())


set.seed(1234)
train_df<-sample_frac(df,0.7)
test_df<- anti_join(df,train_df,by="Row_ID")
#now i will remove the vriable ID that we added above:
train_df<-train_df[,-c(31)]
test_df<-test_df[,-c(31)]

#the binary model: (full model):
#we put all the possible combinations that we think it may have interaction:
logmodel<-glm(Fatigue_Level ~ Perceived_Safety_Rating + Crowd_Density +
                Movement_Speed+Activity_Type+Weather_Conditions+Temperature+Sound_Level_dB +
                AR_System_Interaction+ Satisfaction_Rating+ Stress_Level+ Queue_Time_minutes+
                Health_Condition+Age_Group+Transport_Mode +Waiting_Time_for_Transport+
                Security_Checkpoint_Wait_Time+Emergency_Event+Incident_Type+Crowd_Morale+
                Pilgrim_Experience+ Interaction_Frequency+Distance_Between_People_m+
                Event_Type+Time_Spent_at_Location_minutes+AR_Navigation_Success+
                Age_Group*Health_Condition+ Weather_Conditions*Temperature+Stress_Level*Temperature+
                Crowd_Density*Stress_Level+Crowd_Density*Health_Condition+Queue_Time_minutes*Crowd_Density
              +Waiting_Time_for_Transport*Transport_Mode+Perceived_Safety_Rating*Incident_Type+
                Incident_Type*Crowd_Density+Movement_Speed*Activity_Type+Age_Group*Satisfaction_Rating
              +Pilgrim_Experience*Perceived_Safety_Rating+Incident_Type*Emergency_Event
              ,binomial(link="logit"),train_df)

summary(logmodel) #AIC=9091.4

#it shows not all the variables is sign. (alot of them is insign)

# Check for multicollinearity
library(car)
car::vif(logmodel) #we compare vif with 10.and alot of them have gvif>10 wich mean the existance of multi.


### now we will build the model using step wise then we will compare it with the full model

final_model_back <- step(logmodel, direction = "back")
summary(final_model_back)  #AIC=8956.3

car::vif(final_model_back) #there is multicollinearity fro both Crowd_Density &Crowd_Density:Queue_Time_minutes
#we tried to remove only the interaction term and then check the vif again
#############
real_final_model_back<-glm(Fatigue_Level ~ 
                             Movement_Speed+Weather_Conditions+Sound_Level_dB +
                             + Queue_Time_minutes+AR_Navigation_Success+Crowd_Density,binomial(link="logit"),train_df)

summary(real_final_model_back)  #AIC=8957.5

car::vif(real_final_model_back) #there is no multicollinearity
#important note: i just tired this but at the end i have used the final_model_back
# as when i used the model without the interaction alot of terms turned to be insign
#so we chose to keep it as control, as we have learnt in CDA course

######
##the goodness of fit & prediction power:

#1
#to compare the two nested models (full model-logmodel- and the real_final_model_back)
#the likelihood ratio test (Deviance test):
anova(final_model_back, logmodel, test = "Chisq")

#as the p-value is high (greater than 0.05) we suggest that the real_final_model_back is better than the full model (logmodel)

#2
#hosmer test as we have cont.predictors:
library(ResourceSelection)
train_df$Fatigue_Level #this gives the real values of the response

predicted_prob<-predict(final_model_back, train_df,type="response") #gives the predicted probabilites

numeric_actual<-as.numeric(as.character(train_df$Fatigue_Level ))
#as i warning said that i can't apply it at factor variable:
hosmer_test <- hoslem.test(numeric_actual, predicted_prob, g = 10)  
print(hosmer_test)

#as the p-value (2.2e-16) is very small, indicating strong evidence against the null hypothesis. means that the model doesn't fits the train data well.
#comment: however we will continue the other tests of goodness of fit to make the possible conclusion about the model


#3. 
#we will find the best cutoff point to continue evaluating the model: (using test data)
predicted_probs <- predict(final_model_back,test_df, type = "response")

library(vcd)
library(pROC)

sensitivity <- c()
specificity <- c()
youden_index <- c()
area <- c()
overall_correctly <- c()

cutoff_point<-c()

#i made a loop on different cutoff points and calculated the confusion matrix and sens. , spec. also the accuracy for each one
for (i in seq(0,1,0.01)) {
  
  predicted_class <- ifelse(predicted_probs > i, 1, 0)
  
  #confusion matrix
  cm <- table(Predicted = factor(predicted_class, levels = c(1, 0)),Actual = factor(test_df$Fatigue_Level,levels=c(1,0)))
  
  sens <- cm[1, 1] / (cm[1, 1] + cm[2, 1])  # True Positives / (True Positives + False Negatives)
  
  spec <- cm[2, 2] / (cm[2, 2] + cm[1, 2])  # True Negatives / (True Negatives + False Positives)
  
  sensitivity <- c(sensitivity, sens)
  specificity <- c(specificity, spec)
  
  #Youden's Index = Sensitivity + Specificity - 1
  youden <- sens + spec - 1
  youden_index <- c(youden_index, youden)
  
  #area under the curve (AUC)
  roc_score <- roc(test_df$Fatigue_Level, predicted_class)
  plot(roc_score, main = "ROC curve – Logistic Regression")
  au_c <- auc(roc_score)
  area <- c(area, au_c)
  
  #overall accuracy
  overall <- sum(diag(cm)) / sum(cm)
  overall_correctly <- c(overall_correctly, overall)
  cutoff_point<-c(cutoff_point, i)}



#based on the maximum overall correctly specified:
max_overall<-which.max(overall_correctly) #the location of the maximum

optimal_cutoff_piont1<-cutoff_point[max_overall]
cat("The optimal cutoff point based on maximum overall correctly specified is:", optimal_cutoff_piont1, "\n")
#0.27

#based on the maximum area under the curve:
max_area<-which.max(area)
optimal_cutoff_piont2<-cutoff_point[max_area]
cat("The optimal cutoff point based on maximum area under the curve is:", optimal_cutoff_piont2, "\n")
#0.35 

#based on yuden index :
location<-which.max(youden_index)
optimal_cutoff_piont3<-cutoff_point[location]
cat("The optimal cutoff point based on the Youden Index is:", optimal_cutoff_piont3, "\n")
#0.29  
################################
#based on the cutoff 0.29

predicted_probs_test <- predict(final_model_back, test_df, type = "response")
#predicted Satisfaction_Rating from the test data
predicted_class_test <- ifelse(predicted_probs_test > 0.29, 1, 0)

library(caret)
CM<- confusionMatrix(factor(predicted_class_test, levels = c(1, 0)), factor(test_df$Fatigue_Level, levels = c(1, 0)))
print(CM)

#####
#based on the cutoff 0.27

predicted_class_test2 <- ifelse(predicted_probs_test > 0.27, 1, 0)

library(caret)
CM2<- confusionMatrix(factor(predicted_class_test2, levels = c(1, 0)), factor(test_df$Fatigue_Level, levels = c(1, 0)))
print(CM2)

#####
#based on the cutoff 0.3480784 ~0.35
predicted_class_test3 <- ifelse(predicted_probs_test > 0.3480784, 1, 0)

library(caret)
CM3<- confusionMatrix(factor(predicted_class_test3, levels = c(1, 0)), factor(test_df$Fatigue_Level, levels = c(1, 0)))
print(CM3)

#################
test_df$Fatigue_Level<-as.factor(test_df$Fatigue_Level)
#4.roc curve
library(pROC)
roc_score<-roc(test_df$Fatigue_Level, predicted_class_test3)
plot(roc_score ,main ="ROC curve – Logistic Regression")

auc_area <- auc(roc_score)
print(auc_area) #acctualy it's very bad we have studied it will be accepted if it's at least 0.6

##################################
##note: another way to determine the optimal cutoff by already exist package

predicted_probs_test <- predict(final_model_back, test_df, type = "response")
levels(test_df$Fatigue_Level)
test_df$Fatigue_Level <- as.numeric(as.character(test_df$Fatigue_Level))
#there is also a package called cutpointr that calculates the cutoff
library(cutpointr)
optimal_cut<-cutpointr(data = test_df,
                       x=predicted_probs_test,
                       class = Fatigue_Level)

optimal_cut |> t()

#it shows that 0.3480784 ~0.35 is the optimal cutoff point as the maximum area under the curve see
#############
#5.Macfaden test:
pscl::pR2(final_model_back)["McFadden"]

################################################################################

datahealth <- data[, c("Crowd_Density", "Activity_Type", "Weather_Conditions", "Fatigue_Level", "Stress_Level", "Health_Condition")]
datahealth<-apply(datahealth,2,as.factor)
datahealth <- as.data.frame(datahealth)

data$Satisfaction_Rating <- as.factor(data$Satisfaction_Rating)
dataSatisfaction_Rating <- data[,c("Satisfaction_Rating", "Distance_Between_People_m", "Time_Spent_at_Location_minutes", 
                                   "Security_Checkpoint_Wait_Time", "Waiting_Time_for_Transport")]

set.seed(123) 
split <- sample.split(datahealth$Health_Condition, SplitRatio = 0.7)
train_data <- subset(datahealth, split == "TRUE")
test_data <- subset(datahealth, split == "FALSE")
set.seed(123) 
split <- sample.split(dataSatisfaction_Rating$Satisfaction_Rating, SplitRatio = 0.7)
train_data_st <- subset(dataSatisfaction_Rating, split == "TRUE")
test_data_st <- subset(dataSatisfaction_Rating, split == "FALSE")

################knn for Satisfaction_Rating
scaled_train <- scale(train_data_st[, c("Distance_Between_People_m", "Time_Spent_at_Location_minutes", 
                                        "Security_Checkpoint_Wait_Time", "Waiting_Time_for_Transport")])
scaled_test <- scale(test_data_st[, c("Distance_Between_People_m", "Time_Spent_at_Location_minutes", 
                                      "Security_Checkpoint_Wait_Time", "Waiting_Time_for_Transport")])
misClassError <- c()
# Loop over k values from 1 to 15
for (i in 1:15) {
  classifier_knn_satis <- knn(train = scaled_train,
                              test = scaled_test,
                              cl = train_data_st$Satisfaction_Rating,
                              k = i)
  misClassError[i] <- mean(classifier_knn_satis != test_data_st$Satisfaction_Rating)
  cat("At k =", i, "| Accuracy =", 1 - misClassError[i], "\n")}

classifier_knn_satis_k <- knn(train = scaled_train, 
                              test = scaled_test, 
                              cl = train_data_st$Satisfaction_Rating, 
                              k = 3)
cm <- table(Predicted = classifier_knn_satis_k, Actual = test_data_st$Satisfaction_Rating)
print(cm)
###############################knn for Health_Condition

# One-hot encoding categorical variables in train and test datasets that change format  of catgorical 
#variable catgories convert into 1 and 0(not becmoing binary or we can say that every catgory in each categorical variable become binary varaible it self)
train_data_encoded <- model.matrix(~ . - 1, data = train_data)  
test_data_encoded <- model.matrix(~ . - 1, data = test_data)  


levels(test_data$Health_Condition) <- levels(train_data$Health_Condition)
train_data$Health_Condition <- as.factor(train_data$Health_Condition)
test_data$Health_Condition <- as.factor(test_data$Health_Condition)
misClassError <- c()

# Loop over k values from 1 to 15
for (i in 1:15) {
  classifier_knn_health <- knn(train = train_data_encoded,
                               test = test_data_encoded,
                               cl = train_data$Health_Condition,
                               k = i)
  misClassError[i] <- mean(classifier_knn_health != test_data$Health_Condition)
  cat("At k =", i, "| Accuracy =", 1 - misClassError[i], "\n")}

classifier_knn_healt_k <- knn(train = train_data_encoded, 
                              test = test_data_encoded, 
                              cl = train_data$Health_Condition,
                              k = 3)
cm <- table(Predicted = classifier_knn_healt_k, Actual = test_data$Health_Condition)
cm

confusion_Matrix <- confusionMatrix( classifier_knn_healt_k,  test_data$Health_Condition)
confusion_Matrix

