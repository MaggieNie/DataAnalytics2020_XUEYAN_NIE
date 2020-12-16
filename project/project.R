rm(list=ls())

library(plyr)
library(dplyr)
library(reshape2)
library(tidyverse)
library(stringr)
library(naniar)

# read file
dataset <- read.csv(file.choose( "NATMON_DS_12102020204211879.cvs"),header = T)

#the information of original file
head(dataset) 
colnames(dataset)
str(dataset)
sapply(dataset, class)
dim(dataset)

# drop unused columns
dataset$TIME <- NULL
dataset$Flag.Codes <- NULL
dataset$Flags <- NULL
dataset$ï..NATMON_IND <- NULL
dataset$LOCATION <- NULL

df <- dataset %>%
  group_by(dataset$Indicator)

group_split(df)
group_keys(df)

# reshape the dataset using pivot table
df1 <- dcast(dataset, Country + Time ~ Indicator, value.var="Value", fun.aggregate=sum)
dim(df1)
colnames(df1)
summary(df1)

# PROBLEM 1: MISSING VALUE
# PROBLEM 2: TOO MANY ATTRIBUTES
# deal with the missing value
# too many 0 in the dataset, replace all 0 with NA
df1[df1 == 0] <- NA

#PERCENTAGE OF MISSING DATA
missing_p<-colMeans(is.na(df1))
missing_p

#Patterns of missingness and combinations of missingness across cases.
missing_data <- colSums(is.na(df1))[colSums(is.na(df1)) > 0] %>% sort(decreasing=T)
gg_miss_upset(df1)

# drop the columns with over 50% missing value
df1[,c(3:36, 38:45)] <- NULL
df1$`Gross enrolment ratio, post-secondary non-tertiary, both sexes (%)` <- NULL
df1[,c(11:23)] <- NULL
df1[,c(18:34)] <- NULL

df1[14]
df1 <- df1[, c(14, 1:13, 15:20)]

# replace the missing value with mean
df1$`Government expenditure on education as a percentage of GDP (%)`[is.na(df1$`Government expenditure on education as a percentage of GDP (%)`)] <- mean(df1$`Government expenditure on education as a percentage of GDP (%)`, na.rm = TRUE)
df1[,c(1)][is.na(df1[,c(1)])] <- mean(df1[,c(1)], na.rm = TRUE)
df1[,c(4)][is.na(df1[,c(4)])] <- mean(df1[,c(4)], na.rm = TRUE)
df1[,c(5)][is.na(df1[,c(5)])] <- mean(df1[,c(5)], na.rm = TRUE)
df1[,c(6)][is.na(df1[,c(6)])] <- mean(df1[,c(6)], na.rm = TRUE)
df1[,c(7)][is.na(df1[,c(7)])] <- mean(df1[,c(7)], na.rm = TRUE)
df1[,c(8)][is.na(df1[,c(8)])] <- mean(df1[,c(8)], na.rm = TRUE)
df1[,c(9)][is.na(df1[,c(9)])] <- mean(df1[,c(9)], na.rm = TRUE)
df1[,c(10)][is.na(df1[,c(10)])] <- mean(df1[,c(10)], na.rm = TRUE)
df1[,c(11)][is.na(df1[,c(11)])] <- mean(df1[,c(11)], na.rm = TRUE)
df1[,c(12)][is.na(df1[,c(12)])] <- mean(df1[,c(12)], na.rm = TRUE)
df1[,c(13)][is.na(df1[,c(13)])] <- mean(df1[,c(13)], na.rm = TRUE)
df1[,c(14)][is.na(df1[,c(14)])] <- mean(df1[,c(14)], na.rm = TRUE)
df1[,c(15)][is.na(df1[,c(15)])] <- mean(df1[,c(15)], na.rm = TRUE)
df1[,c(16)][is.na(df1[,c(16)])] <- mean(df1[,c(16)], na.rm = TRUE)
df1[,c(17)][is.na(df1[,c(17)])] <- mean(df1[,c(17)], na.rm = TRUE)
df1[,c(18)][is.na(df1[,c(18)])] <- mean(df1[,c(18)], na.rm = TRUE)
df1[,c(19)][is.na(df1[,c(19)])] <- mean(df1[,c(19)], na.rm = TRUE)
df1[,c(20)][is.na(df1[,c(20)])] <- mean(df1[,c(20)], na.rm = TRUE)


stat(df1)
summary(df1)

# PCA
df1.pca <- prcomp(df1[,c(1,4:20)])
# df1.pca
summary(df1.pca)
str(df1.pca)

# access to PCA results
library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(df1.pca)
eig.val
# Results for Variables
df1.var <- get_pca_var(df1.pca)
df1.var$coord          # Coordinates
df1.var$contrib        # Contributions to the PCs
df1.var$cos2           # Quality of representation 
# Results for individuals
df1.ind <- get_pca_ind(df1.pca)
df1.ind$coord          # Coordinates
df1.ind$contrib        # Contributions to the PCs
df1.ind$cos2           # Quality of representation

# The ggfortify package provides a handy method for plotting the first two principal components with autoplot().
library(ggfortify)
pca.plot <- autoplot(df1.pca, data = df1, colour = 'blue')
pca.plot

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
library(factoextra)
fviz_eig(df1.pca)

#rename columns
names(df1)[names(df1) == "School life expectancy, primary to tertiary, both sexes (years)"] <- "school_life_expectance_p2t"
names(df1)[names(df1) == "Government expenditure on education as a percentage of GDP (%)"] <- "government_expenditure_on_education"
names(df1)[names(df1) == "Gross enrolment ratio, lower secondary, both sexes (%)"] <- "Gross_enrolment_ratio_lower_secondary"
names(df1)[names(df1) == "Gross enrolment ratio, primary and lower secondary, both sexes (%)"] <- "Gross_enrolment_ratio_primary_and_lower_secondary"
names(df1)[names(df1) == "Gross enrolment ratio, primary and secondary, both sexes (%)"] <- "Gross_enrolment_ratio_primary_and_secondary"
names(df1)[names(df1) == "Gross enrolment ratio, primary to tertiary, both sexes (%)"] <- "Gross_enrolment_ratio_primary_to_tertiary"
names(df1)[names(df1) == "Gross enrolment ratio, primary, both sexes (%)"] <- "Gross_enrolment_ratio_primary"
names(df1)[names(df1) == "Gross enrolment ratio, secondary, both sexes (%)"] <- "Gross_enrolment_ratio_secondary"
names(df1)[names(df1) == "Gross enrolment ratio, upper secondary, both sexes (%)" ] <- "Gross_enrolment_ratio_upper_secondary" 
names(df1)[names(df1) == "School life expectancy, pre-primary, both sexes (years)"] <- "School_life_expectancy_pre-primary"
names(df1)[names(df1) == "School life expectancy, primary and lower secondary, both sexes (years)"] <- "School_life_expectancy_primary_and_lower_secondary"
names(df1)[names(df1) == "School life expectancy, primary and secondary, both sexes (years)"] <- "School_life_expectancy_primary_and_secondary"
names(df1)[names(df1) == "School life expectancy, primary, both sexes (years)"] <- "School_life_expectancy_primary"
names(df1)[names(df1) == "School life expectancy, secondary, both sexes (years)"] <- "School_life_expectancy_secondary"
names(df1)[names(df1) == "School life expectancy, tertiary, both sexes (years)"] <- "School_life_expectancy_tertiary"
names(df1)[names(df1) == "Total net enrolment rate, lower secondary, both sexes (%)"] <- "Total_net_enrolment_rate_lower_secondary"
names(df1)[names(df1) == "Total net enrolment rate, primary, both sexes (%)"] <- "Total_net_enrolment_rate_primary"
names(df1)[names(df1) == "Total net enrolment rate, upper secondary, both sexes (%)"] <- "Total_net_enrolment_rate_upper_secondary"
names(df1)[names(df1) == "School_life_expectancy_pre-primary"] <- "School_life_expectancy_preprimary"


# DATA VISUALIZATION
library("PerformanceAnalytics")
my_data <- df1[,c(1,4:20)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# heatmap
res <- cor(my_data)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)


# MODELING

####
######## MODEL 1: KNN REGRESSION########
####
library(FNN)

# test & train
smp_size <- floor(0.75 * nrow(df1))

## set the seed to make the partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df1)), size = smp_size)

train <- df1[train_ind, ]
test <- df1[-train_ind, ]

train$Country <- NULL
test$Country <- NULL

dim(train)
dim(test)

knn_predictions <- knn.reg(train = train, 
                           test = test, 
                           y = train$school_life_expectance_p2t, 
                           k = 5)

# r square
actual_knn <- test$school_life_expectance_p2t
preds_knn <- knn_predictions$pred
rss_knn <- sum((preds_knn - actual_knn) ^ 2)
tss_knn <- sum((actual_knn - mean(actual_knn)) ^ 2)
rsq_knn <- 1 - rss_knn/tss_knn
rsq_knn

####
######## MODEL 2: RANDOM FORESTS ########
####

# test & train
smp_size <- floor(0.75 * nrow(df1))

## set the seed to make the partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df1)), size = smp_size)

train <- df1[train_ind, ]
test <- df1[-train_ind, ]

dim(train)
dim(test)

library(randomForest)
library(caret)

rf <- randomForest(school_life_expectance_p2t ~ ., 
                   data=train,
                   metric = "Accuracy",
                   mtry = 4, 
                   importance = TRUE,
                   trControl = trainControl(method = 'cv', number = 5))
plot(rf) 

pred_rf <- predict(rf, test)

# r square
actual_rf <- test$school_life_expectance_p2t
preds_rf <- pred_rf
rss_rf <- sum((preds_rf - actual_rf) ^ 2)
tss_rf <- sum((actual_rf - mean(actual_rf)) ^ 2)
rsq_rf <- 1 - rss_rf/tss_rf
rsq_rf

####
######## MODEL 3: LASSO REGRESSION########
####
# Loading the library
library(glmnet)

x_vars <- model.matrix(school_life_expectance_p2t~. , df1)[,-1]
y_var <- df1$school_life_expectance_p2t
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test,])

# r square
actual_lr <- y_test
preds_lr <- pred
rss_lr <- sum((preds_lr - actual_lr) ^ 2)
tss_lr <- sum((actual_lr - mean(actual_lr)) ^ 2)
rsq_lr <- 1 - rss_lr/tss_lr
rsq_lr

# find best correlated parameters
coef(lasso_best)