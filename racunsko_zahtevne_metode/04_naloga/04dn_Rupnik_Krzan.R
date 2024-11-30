# neki predlogov za podatke

# 1. opacija: teh je pomojem precej premalo...
library(mice)
data = nhanes 
summary(data)
nrow(data)
missing_data = (colSums(is.na(data)) / nrow(data)) * 100 # koliko % podatkov manka po stolpcih
missing_data

# 2. opcija: Pima Indians Diabetes Database - ta se mi zdi kr okej...
data = read.csv("pima_indians_diabetes_dataset.csv", header = TRUE, sep = ",", na.strings = c(""))
summary(data)
nrow(data)
missing_data = (colSums(is.na(data)) / nrow(data)) * 100 # koliko % podatkov manka po stolpcih
missing_data
# https://github.com/ashishpatel26/Pima-Indians-Diabetes-Dataset-Missing-Value-Imputation/blob/master/Readme.md
# https://www.kaggle.com/code/faelk8/diabetes-eda-missing-values
# https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database


complete_df <- data[complete.cases(data), ]
missing_data2 = (colSums(is.na(complete_df)) / nrow(complete_df)) * 100 
missing_data2

nrow(complete_df)/nrow(data)
