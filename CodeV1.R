#install required packages for this report
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(plotly)
library(tidyverse)
library(car)
library(pscl)
library(Boruta)
library(factoextra)
library(pdfCluster)
library(flextable)
library(pROC)
library(DT)

citation("pROC") #citation for package used


df1 <- read.csv("m:\\pc\\desktop\\Modelling\\project data.csv") #read in data file
df1 <- na.omit(df1) #drop missing values
df1$M.F <- ifelse(df1$M.F == "M", 1, 2) #change M to 1 and F to 2
df1 = filter(df1, Group != "Converted") #remove rows containing "converted" from the group column
df1$Group_Num <- ifelse(df1$Group == "Demented", 1, 0) #creates a new column with 0 = nondemented, and 1 = demented
head(df1)
str(df1)
unique(df1$M.F)
unique(df1$Group)#these are used to check the data cleaning is correctly done
unique(df1$CDR)
unique(df1$MMSE) 
unique(df1$Age)


df1 <- df1 %>% mutate(MMSE_score = case_when(
  MMSE <= 9 ~ "severe",
  MMSE >= 10 & MMSE <= 18 ~ "moderate",
  MMSE >= 19 & MMSE <= 23 ~ "mild",
  MMSE >= 24 ~ "normal"
)) #creates a new column grouping MMSE scores into their diagnosis range to make visualisation easier

unique_count <- function(column) { #take the number of unique entries for each column and add them
  length(unique(column))
}
col_unique <- sapply(df1, unique_count)
print(col_unique) #this is used to get a better understanding of each column


##### DATA VISUALISATION

#Pie chart showing % of people diagnosed (Figure 1)
ggplot(df1, aes(x = "", fill = Group)) + #create a plot using ggplot2 from df1, leave x-axis empty
  geom_bar(width = 1, position = "fill") + 
  scale_fill_manual(values = c("brown", "#E69F00")) + #set colours manually
  geom_text(stat = "count", aes(label = paste0(scales::percent(stat(count) / sum(stat(count))), "" , "")),
            position = position_fill(vjust = 0.5),color = "white") + #Add text labels to the plot displaying the percentage count of each Group
  coord_polar("y", start = 0) +
  theme_void() + ## Remove all unimportant elements from the plot
  ggtitle("Percentage of People Diagnosed") +
  guides(fill = guide_legend(title = "Diagnosis")) #set custom title and legend title
#40% diagnosed, 60% not.

#Correlation Matrix of all variables (Figure 2)
corrM <- cor(df1[, c("Group_Num", "M.F", "Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")]) #correlation matrix of all variables
ggcorrplot(corrM, type = "lower", outline.col = "white", lab = TRUE, lab_size = 3, colors = c("#6D9EC1", "white", "#E46726")) #plot corr matrix
#a correlation analysis is used to help identify potentially most useful predictor values
#and to see if there are any predictors which are highly correlated and could probably be removed from the model
#eTIV and ASF have effectively no correlation with Group, thus probably don't need to be in the model

## Density plots of all continuous variables

#for explanation of code for figures 3-8 see figure 3
#Density plot of age (Figure 3)
ggplot(data = df1, aes(x = Age)) + #using ggplot2 and df1 create density plots
  geom_density(fill = "lightblue") + #create density plot with lightblue fill colour
  labs(x = "Age", y = "Density") +
  ggtitle("Density Plot of Subject Ages") #custom title and axis labels
#Looks close to a normal distribution of ages between 60 and 98
shapiro.test(df1$Age)
#A quick test shows that Age is not normally distributed at p<0.05

#Density plot of Years in Education (Multiple peaks) (Figure 4)
ggplot(data = df1, aes(x = EDUC)) +
  geom_density(fill = "lightblue") +
  labs(x = "EDUC", y = "Density") +
  ggtitle("Density Plot of Subject Years in Education")

#Density plot of MMSE score (Highly skewed) (Figure 5)
ggplot(data = df1, aes(x = MMSE)) +
  geom_density(fill = "lightblue") +
  labs(x = "MMSE", y = "Density") +
  ggtitle("Density Plot of Subject MMSE Score")

#Density plot of subject eTIV (Figure 6)
ggplot(data = df1, aes(x = eTIV)) +
  geom_density(fill = "lightblue") +
  labs(x = "eTIV", y = "Density") +
  ggtitle("Density Plot of Subject eTIV")

#Density plot of subject nWBV (Figure 7)
ggplot(data = df1, aes(x = nWBV)) +
  geom_density(fill = "lightblue") +
  labs(x = "nWBV", y = "Density") +
  ggtitle("Density Plot of Subject nWBV")

#Density plot of subject ASF (Figure 8)
ggplot(data = df1, aes(x = ASF)) +
  geom_density(fill = "lightblue") +
  labs(x = "ASF", y = "Density") +
  ggtitle("Density Plot of Subject ASF")

#Boxplot of Diagnosis against Age (Figure 9)
ggplot(df1, aes(x = Group, y = Age)) + #set x-axis as Group and y-axis as Age
  geom_boxplot(fill = "#0072B2", outlier.shape = NA) + #create boxplot using ggplot2
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) + #jitter the datapoints to make it easier to see where they are located on the plot
  labs(x = "Diagnosis", y = "Age",
       title = "Diagnosis by Age") + #custom title and axis labels
  theme_bw()

#Diagnoses by gender (Figure 10)
gridExtra::grid.arrange(grobs = list( #set to plot multiple graphs
  ggplot(subset(df1, Group == "Demented"), aes(x = M.F)) + #plot 1 only show Demeted subjects
    geom_bar(fill = "Lightblue") + #ggplot2 bar plot with custom fill colour
    labs(title = "Demented Subjects by Gender",
         x = "Male (1) or Female (2)",
         y = "Count (bins = 20)"), #custom labels
  ggplot(subset(df1, Group == "Nondemented"), aes(x = M.F)) + #plot 2 only show Nondemeted subjects
    geom_bar(fill = "Lightblue") + #ggplot2 bar plot with custom fill colour
    labs(title = "Nondemented Subjects by Gender",
         x = "Male (1) or Female (2)",
         y = "Count (bins = 50)")), #custom labels
  nrow = 1) #plot should be arranged on a single row

#Bar graph: Diagnosis by EDUC and SES (Figure 11)
ggplot(df1, aes(x = EDUC, fill = Group)) + #ggplot2 barplot
  geom_bar(position = "stack") + #add another layer of bars stacked on top of each other
  facet_wrap(~ SES, ncol = 1) + #create facet plots arranged on a single column
  labs(title = "Subject Diagnosis by Education and SES",
       x = "Years in Education",
       y = "Count",
       fill = "Diagnosis") #custom labels

#table showing diagnoses by CDR and MMSE score groups (Table 1)
df_demented <- df1 %>% #create new dataframe, grouped by CDR, MMSE, and Gender
  group_by(CDR, MMSE_score, M.F) %>%
  summarize(total_diagnoses = n(), #summarise the data 
            diagnosed = sum(Group == "Demented")) %>% #find total count of diagnoses and total count of Demented diagnoses within that
  ungroup() %>% #remove grouping
  mutate(percentage = round(diagnosed / total_diagnoses * 100,2 )) #calculate percentage of Demented diagnoses
datatable(df_demented, #create datatable visualising the above
          options = list(pageLength = 20, lengthChange = FALSE))
#If the person has a CDR score that is not 0, then they have dementia, and will likely be diagnosed


##### CLUSTERING ALGORITHMS
set.seed(123) # Set seed for reproducibility
df1Norm <- data.frame(df1) #new data frame for clustering
df1Norm <- subset(df1Norm, select = -c(Group, MMSE_score)) #remove redundant columns/duplicate information
str(df1Norm)
contVars <- c("Age", "EDUC", "MMSE", "eTIV", "nWBV", "ASF") #create list of continuous variables for use later to assess average values


#K-Mean Clustering
fviz_nbclust(df1Norm, kmeans, method = "wss")+ #find TWSS per cluster and plot
  geom_vline(xintercept = 3, color = "red") #(Figure 12)

kmeansNorm <- kmeans(df1Norm, centers = 3, nstart = 20) #run k-means function with clusters = 3
clusterPlot <- fviz_cluster(kmeansNorm, geom = "point", data = df1Norm) + ggtitle("Cluster Plot: k = 3")
plot(clusterPlot) #plot k-means clusters (Figure 13)
clusterNorm <- kmeansNorm$cluster #select cluster information
table(clusterNorm, df1Norm$Group_Num) #Distribution of diagnosis in clusters
table(clusterNorm, df1Norm$CDR) #Distribution of CDR in clusters
table(clusterNorm, df1Norm$M.F) #Distribution of Gender in clusters
#Create savable tables that look nice from the above information
ftable1 <- flextable(data.frame(table(clusterNorm, df1Norm$Group_Num))) #create a flextable showing breakdown of variable assignment to clusters
ftable1 #Table 2
save_as_docx(ftable1, path = "table_2.docx") #save flextables to a word doc for easy access
ftable2 <- flextable(data.frame(table(clusterNorm, df1Norm$CDR)))
ftable2 #Table 3
save_as_docx(ftable2, path = "table_3.docx")
ftable3 <- flextable(data.frame(table(clusterNorm, df1Norm$M.F)))
ftable3 #Table 4
save_as_docx(ftable3, path = "table_4.docx")

clusterAvg <- data.frame(Cluster = unique(clusterNorm)) #Average of each contVar in the clusters
for (var in contVars) { #Iterate over each continuous variable
  avg_values <- tapply(df1Norm[[var]], clusterNorm, mean) #Compute the average value of the variables in each cluster
  clusterAvg[[var]] <- avg_values #Add the average values to the new data frame
}
clusterAvg
ftableAvg <- flextable(clusterAvg) #use average values to plot to a table
ftableAvg #Table 5
save_as_docx(ftableAvg, path = "table_5.docx") #save to word

#Showing how effective the clustering is: <0 = worse than chance, 0= same as chance, 1= Perfect alignment with truth
ARI1 <- adj.rand.index(df1Norm$Group_Num, clusterNorm) #function identifying how effective the clustering is, these cover 3 variables
ARI1
ARI2 <- adj.rand.index(df1Norm$CDR, clusterNorm)
ARI2
ARI3 <- adj.rand.index(df1Norm$M.F, clusterNorm)
ARI3

#repeating the above but using 5 clusters instead of 3
df1Norm1 <- data.frame(df1) 
df1Norm1 <- subset(df1Norm1, select = -c(Group, MMSE_score))
kmeansNorm1 <- kmeans(df1Norm1, centers = 5, nstart = 20) #clusters set to 5
clusterNorm1 <- kmeansNorm1$cluster
ARI15 <- adj.rand.index(df1Norm1$Group_Num, clusterNorm1) 
ARI15
ARI25 <- adj.rand.index(df1Norm1$CDR, clusterNorm1)
ARI25
ARI35 <- adj.rand.index(df1Norm1$M.F, clusterNorm1)
ARI35

##### LOGISTIC REGRESSION

#Logistic regression model for all variables 
#Code and functions will work the same for all "Model#" thus comments on Model 1 will not be repeated on following Model# 
model1 <- glm(Group_Num~M.F+Age+EDUC+SES+MMSE+eTIV+nWBV+ASF+CDR, family="binomial", data=df1) #create logistic regression model using all variables
summary(model1) #show output of above regression model
vif(model1) #check for colinearity
pR2(model1)["McFadden"] #high McFadden rsqr val shows the usefulness of the model.psuedo R-Squared value
#This model does not work
#as previous analysis showed, if someone has a CDR rating above 0 then they were also demented (4 anomalies), hence the populations of Group and CDR are the same -chance


model2 <- glm(Group_Num~M.F+Age+EDUC+SES+MMSE+eTIV+nWBV+ASF, family="binomial", data=df1) #CDR removed from predictors
summary(model2)
vif(model2) 
pR2(model2)["McFadden"]
#This model, while not perfect, does work using all variables (minus CDR)
#Although it can be seen that some variables have high levels of colinearity.



##### FEATURE SELECTION (Maybe come back and drop in a few anova tests)

#New model with ASF removed due to high multicollinearity.
model3 <- glm(Group_Num~M.F+Age+EDUC+SES+MMSE+eTIV+nWBV, family="binomial", data=df1)
summary(model3)
vif(model3)
pR2(model3)["McFadden"]
#This model has much lower levels of colinearity, and has a lower AIC score, without majorly affecting the pseudo-R2 val.
#AIC 197.3 and psudeo-R2 val 0.5753

#Now through feature selection functions, attempts will be made to improve the model
bkwrdstep <- step(model3, method = "backward") #uses all predictors then drops the predictor with the lowest SS score, reducing AIC
#Group_Num ~ M.F + Age + MMSE + eTIV + nWBV appears to be the best model according to the bkwrdstep algorithm
#a forward step on the same model to ensure the results are correct.
fwrdstep <- step(model3, method = "forward")

#Using another feature selection model lets see if the 'best' models align
#Boruta applies a random forest to rate features by their importance in the model
boruta1 <- Boruta(Group_Num~M.F+Age+EDUC+SES+MMSE+eTIV+nWBV, family="binomial", data = df1, doTrace = 1) #run boruta feature selection on Model 3
decision <- boruta1$finalDecision
signif <- decision[boruta1$finalDecision %in% c("Confirmed")] #select significant predictors according to boruta algorithm
print(signif) #print significant predictors
plot(boruta1, xlab="", main="Variable Importance") #(Figure 14) create plot showing figure importance
attStats(boruta1) #(Table 6) create table showing feature importance

#test the bkwrdstep and frwrdstep model
model4 <- glm(Group_Num~M.F+Age+MMSE+eTIV+nWBV, family="binomial", data=df1)
summary(model4)
pR2(model4)["McFadden"]
#AIC 196.99 and psudeo-R2 val 0.5666

#test the Boruta model (dropping lowest values Age and SES)
model5 <- glm(Group_Num~M.F+EDUC+MMSE+eTIV+nWBV, family="binomial", data=df1)
summary(model5)
pR2(model5)["McFadden"]
#AIC 215.88 and psudeo-R2 val 0.5224 (dropping anything else makes the model worse)

#the result of this analysis has created model4, the best from this testing

#Evalute model4's performance using several metrics
#building the #train/test split prediction model based off our previous logistic regression model
TTmodelsplit <- function(df) { #creates a function that runs the train and test split model 50 times to get an avg model accuracy
  results <- numeric(50)  #vector to store results
  
  for (i in 1:50) {
    sample <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(0.7, 0.3)) #assigns 70/30 split of T/F vals to rows
    train <- df[sample, ]
    test <- df[!sample, ] #these check for T/F vals, then creates samples from df1 split into train and test sets based on that val
    
    modeltt <- glm(Group_Num~M.F+Age+MMSE+eTIV+nWBV, family = "binomial", data = train) #based on model4 but using the Train split
    new.probs <- predict(modeltt, test, type = "response") #uses the trained regression model to make predictions for test set
    
    test.length <- nrow(test) #counters the problem of the variable length of "test" from the way df1 is split
    new.pred <- rep("Nondemented", test.length) #new vector filled with Nondemented values
    new.pred[new.probs > 0.5] <- "Demented" #updates the vector, changing values to Demented if the prediction thinks its a more likely result
    
    results[i] <- mean(new.pred == test$Group) #store the results of each iteration, and proportion of correct predictions
  }
  
  avg_result <- mean(results) #averages the proportion of correct predictions across all iterations, to get an overall average accuracy
  return(avg_result)
}
avg_accuracy <- TTmodelsplit(df1) 
print(avg_accuracy) #takes output of the function and prints it


#Area Under Curve (AUC) of ROC curve for test set
rocm4 <- roc(test$Group, new.probs)
rocm4

#Confusion Matrix for the test set
cMatrix <- table(new.pred, test$Group)
cMatrix
ftabCM <- flextable(data.frame(cMatrix)) #Table 7
save_as_docx(ftabCM, path = "table_7.docx") #save table as a word doc

#Compare the optimal model with the original model to assess how much it has improved
TTmodelsplitOG <- function(df) { #creates a function that runs the train and test split model 50 times to get an avg model accuracy
  resultsOG <- numeric(50)  #vector to store results
  
  for (i in 1:50) {
    sample <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(0.7, 0.3)) #assigns 70/30 split of T/F vals to rows
    train <- df[sample, ]
    test <- df[!sample, ] #these check for T/F vals, then creates samples from df1 split into train and test sets based on that val
    
    modelOG <- glm(Group_Num~M.F+Age+EDUC+SES+MMSE+eTIV+nWBV+ASF, family = "binomial", data = train) #based on model2 but using the Train split
    OG.probs <- predict(modelOG, test, type = "response") #uses the trained regression model to make predictions for test set
    
    test.length <- nrow(test) #counters the problem of the variable length of "test" from the way df1 is split
    OG.pred <- rep("Nondemented", test.length) #new vector filled with Nondemented values
    OG.pred[OG.probs > 0.5] <- "Demented" #updates the vector, changing values to Demented if the prediction thinks its a more likely result
    
    resultsOG[i] <- mean(OG.pred == test$Group) #store the results of each iteration, and proportion of correct predictions
  }
  
  avg_resultOG <- mean(resultsOG) #averages the proportion of correct predictions across all iterations, to get an overall average accuracy
  return(avg_resultOG)
}
avg_accuracyOG <- TTmodelsplitOG(df1)
print(avg_accuracyOG) #takes output of the function and prints it


##### CODE DUMP

#building the predicting model
probability <- predict(model2,type="response") #Pr(Y=1|X)
predicted <- rep("Nondemented",317)
predicted[probability>0.5]="Demented"
table(predicted, df1$Group)
mean(predicted==df1$Group)
#an average accuracy rating of 85.8% is a good result for this model, however this was done purely from the dataset

#train/test split prediction model
sample <- sample(c(TRUE, FALSE), nrow(df1), replace=TRUE, prob=c(0.7,0.3)) #70% of dataset for training and 30% for test
train <- df1[sample, ]
test <- df1[!sample, ]

modeltt <- glm(Group_Num~M.F+Age+MMSE+nWBV+ASF, family="binomial", data=train)
new.probs <- predict(modeltt, test, type="response")

test.length <- nrow(test) #due to random length from the train/test split, this allows the following code to run without edits
new.pred = rep("Nondemented", test.length) 
new.pred[new.probs>0.5]="Demented"
table(new.pred, test$Group)
mean(new.pred== test$Group)

modelfs1 <- glm(Group_Num~M.F+Age+EDUC+SES+MMSE+nWBV+ASF, data = df1)
summary(modelfs1)
step1 <- step(modelfs1, method = "backward") #uses everything then drops the predictor with the lowest SS score, reducing AIC

#(Figure 13)
#nWBV amd MMSE had the highest correlation, boxplot it to see their relationship
ggplot(df1, aes(x = MMSE_score, y = nWBV)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "MMSE Rating", y = "Normalised Whole Brain Volume") +
  ggtitle("MMSE Ratings by Brain Volume")
#Large amounts of variation, however we can see that those with worse scores tend to have lower WBV medians
#Although normal scores show massive variation, they also have the highest quartiles. 

#Scatterplot for age and diagnosis (Figure 12)
df_age <- df1 %>%
  group_by(Age) %>%
  summarize(percent_diagnosis = sum(Group == "Demented") / n())
ggplot(df_age, aes(x = Age, y = percent_diagnosis)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Age", y = "Percent of subjects Diagnosed")
#A quick look at the scatter plot confirms what the correlation matrix revealed,
#that Age is not a useful indicator for diagnosis, with a large no. of outliers. 
