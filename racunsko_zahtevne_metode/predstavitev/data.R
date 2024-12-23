data = read.csv("pima_indians_diabetes_dataset.csv", header = TRUE, sep = ",", na.strings = c(""))

View(data)

data$AgeGroup <- cut(data$Age, 
                         breaks = c(20, 30, 40, 50, 60, 70, 80), 
                         labels = c("21-30", "31-40", "41-50", "51-60", "61-70", "71-81"),
                         right = TRUE)

library(dplyr)

# Izračun števila in deleža v vsaki skupini
group_summary <- data %>%
  group_by(AgeGroup) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
View(group_summary)

library(ggplot2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)
library(kableExtra)
library(gridExtra)
library(psych) #describe
library(corrplot)
library(VIM)
library(multiUS)
library(mice)
library(arm)
library(randomForest)
data$Class = as.factor(data$Class)
g1 <- ggplot(data, aes(x=Pregnant)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "density") 
g2 <- ggplot(data, aes(x=Pregnant, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "density") 
g_1 = grid.arrange(g1, g2, ncol=2, top = paste("variable Pregnant"))

g1 <- ggplot(data, aes(x=Glucose)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "density") 
g2 <- ggplot(data, aes(x=Glucose, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "density") 
g_2 = grid.arrange(g1, g2, ncol=2, top = paste("variable Glucose"))

g1 <- ggplot(data, aes(x=BloodPressure)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "density") 
g2 <- ggplot(data, aes(x=BloodPressure, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "density") 
g_3 = grid.arrange(g1, g2, ncol=2, top = paste("variable BloodPressure"))

g1 <- ggplot(data, aes(x=SkinThickness)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "density") 
g2 <- ggplot(data, aes(x=SkinThickness, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "density") 
g_4 = grid.arrange(g1, g2, ncol=2, top = paste("variable SkinThickness"))

g1 <- ggplot(data, aes(x=Insulin)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "density") 
g2 <- ggplot(data, aes(x=Insulin, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "density") 
g_5 = grid.arrange(g1, g2, ncol=2, top = paste("variable Insulin"))

g1 <- ggplot(data, aes(x=BMI)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "density") 
g2 <- ggplot(data, aes(x=BMI, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "density") 
g_6 = grid.arrange(g1, g2, ncol=2, top = paste("variable BMI"))

g1 <- ggplot(data, aes(x=DiabetesPedigree)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "density") 
g2 <- ggplot(data, aes(x=DiabetesPedigree, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "density") 
g_7 = grid.arrange(g1, g2, ncol=2, top = paste("variable DiabetesPedigree"))

g1 <- ggplot(data, aes(x=Age)) + 
  geom_density() + 
  theme_bw()+
  labs(y = "density") 
g2 <- ggplot(data, aes(x=Age, fill=Class)) + 
  geom_density(alpha=0.4) + 
  theme_bw()+
  labs(y = "density") 
g_8 = grid.arrange(g1, g2, ncol=2, top = paste("variable Age"))

grid.arrange(g_1, g_2, g_3, g_4, g_5, g_6, g_7, g_8, nrow=8, ncol = 2)


# Calculate percentage of missing data for each column
missing_percentage <- colSums(is.na(data)) / nrow(data) * 100

# Display results
missing_percentage


plot_missing_data <- function(column_name, data) {
  total_na <- sum(is.na(data[[column_name]])) # Count of missing values
  total_data <- nrow(data)
  
  non_missing_percentage <- (total_data - total_na) / total_data * 100
  missing_percentage <- total_na / total_data * 100
  
  df <- data.frame(
    Category = c("Non-missing", "Missing"),
    Percentage = c(non_missing_percentage, missing_percentage)
  )
  
  ggplot(df, aes(x = "", y = Percentage, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("    variable", column_name)) +
    theme_void() +
    scale_fill_manual(values = c("royalblue4", "slategray2"))
}

g1 = plot_missing_data("Pregnant",data)
g2 = plot_missing_data("Glucose",data)
g3 = plot_missing_data("BloodPressure",data)
g4 = plot_missing_data("SkinThickness",data)
g5 = plot_missing_data("Insulin",data)
g6 = plot_missing_data("BMI",data)
g7 = plot_missing_data("DiabetesPedigree",data)
g8 = plot_missing_data("Age",data)

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, nrow=2, ncol = 4)

data.frame(x = c(2, 4, 6, 8, 10, 12, 14, 16)
           y = c(0, 0.651, 4.557, 29.557, 48.697, 1.432, 0, 0))