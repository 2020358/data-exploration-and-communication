setwd("C:/Users/rober/OneDrive/Desktop/CAs/data exploration and communication")
library(dplyr)

# import data set
data <- read.csv("data.csv", na.strings = "NA")
head(data)

# remove first dose refused, this column contains only NA values 
data <- data[, -which(names(data) == 'FirstDoseRefused')]


print( nrow(data))
# check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# remove missing values 
remove_na <- na.omit(data)
print(nrow(remove_na))
str(remove_na)
missing_values <- colSums(is.na(remove_na))
print(missing_values)
remove_na
