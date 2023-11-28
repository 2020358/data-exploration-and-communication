setwd("C:/Users/rober/OneDrive/Desktop/CAs/data exploration and communication")
library(dplyr)
library(ggplot2)

# import data set
data <- read.csv("data.csv", na.strings = "NA")
head(data)


# a) cleaning 

# remove first dose refused column, this column contains only NA values 
data <- data[, -which(names(data) == 'FirstDoseRefused')]

# check row count
print( ncol(data))


# remove missing values and check row count 
data <- na.omit(data)
print(nrow(data))
head(data)


# distinct countries 
distinct_countries <- unique(data$ReportingCountry)

# distinct region
distinct_regions <- unique(data$Region)

print(length(distinct_countries))
print(length(distinct_regions))

# remove region column
data <- data[, -which(names(data) == 'Region')]



# check for outliers 
# Boxplot for a population variable 
boxplot(data$Denominator , main = "Boxplot of Denominator", ylab = "Denominator")

# Boxplot for a population variable 
boxplot(data$Population , main = "Boxplot of Population", ylab = "Population")

# b) statistical parameters. 
# denominator 
denominator_statistics <- summary(data$Denominator)
denominator_sd <- sd(data$Denominator)
print(denominator_statistics)
print(denominator_sd)
# number of doses received 
number_received_statistics <- summary(data$NumberDosesReceived)
number_received_sd <- sd(data$NumberDosesReceived)
print(number_received_statistics)
print(number_received_sd)
# number of doses exported 
number_exported_stat <- summary(data$NumberDosesExported)
number_exported_sd <- sd(data$NumberDosesExported)
print(number_exported_stat)
print(number_received_sd)
# first dose
first_dose_stat <- summary(data$FirstDose)
first_dose_sd <- sd(data$FirstDose)
print(first_dose_stat)
print(first_dose_sd)
# second dose 
second_dose_stat <- summary(data$SecondDose)
second_dose_sd <- sd(data$SecondDose)
print(second_dose_stat)
print(second_dose_sd)
# additional dose 1
ad1_stat <- summary(data$DoseAdditional1)
ad1_sd <- sd(data$DoseAdditional1)
print(ad1_stat)
print(ad1_sd)
# additional dose 2
ad2_stat <- summary(data$DoseAdditional2)
ad2_sd <- sd(data$DoseAdditional2)
print(ad2_stat)
print(ad2_sd)
# additional dose 3
ad3_stat <- summary(data$DoseAdditional3)
ad3_sd <- sd(data$DoseAdditional3)
print(ad3_stat)
print(ad3_sd)
# additional dose 4
ad4_stat <- summary(data$DoseAdditional4)
ad4_sd <- sd(data$DoseAdditional4)
print(ad4_stat)
print(ad4_sd)
# additional dose 5
ad5_stat <- summary(data$DoseAdditional5)
ad5_sd <- sd(data$DoseAdditional5)
print(ad5_stat)
print(ad5_sd)
# unknown dose
ud_stat <- summary(data$UnknownDose)
ud_sd <- sd(data$UnknownDose)
print(ud_stat)
print(ud_sd)




first_dose <- data$Population

# Create a histogram
ggplot(data, aes(x = FirstDose)) +
  geom_histogram(binwidth = 1e5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of FirstDose", x = "FirstDose", y = "Frequency")

 
# c min-max normalization, z-score and robust scalar 

# denominator 
denominator <- data$Denominator

min_max_deno <- (denominator - min(denominator))
/ (max(denominator)- min(denominator))
z_score_deno <- (denominator - mean(denominator))
/ sd(denominator)
#first quantile
q1_deno <- quantile(denominator, 0.25)
#third quantile 
q3_deno <- quantile(denominator, 0.75)
robust_deno <- (denominator - median(denominator)) / IQR(denominator)

# number of doses received 
dose_rec <- data$NumberDosesReceived

min_max_dose_rec <- (dose_rec - min(dose_rec)) /
  (max(dose_rec) - min(dose_rec))
z_score_dose_rec <- (dose_rec - mean(dose_rec)) /
  sd(dose_rec)

#first quantile
q1_dose_res = quantile(dose_rec, 0.25)
#third quantile 
q3_dose_res = quantile(dose_res, 075)
robust_dose_res <- (dose_rec - median(dose_rec)) /
  IQR(dose_rec)

#number of doses exported 
dose_ex <- data$NumberDosesExported

min_max_dose_ex <- (dose_ec - min(dose_ex)) /
  (max(dose_ex) - min(dose_ex))
z_score_dose_ex <- (dose_ex - sd(dose_ex)) /
  sd(dose_ex)

#first quantile
q1_dose_ex <- quantile(dose_ex, 0.25)
#third quantile
q3_dose_ex <- quantile(dose_ex, 0.75)
robust_dose_ex <- (dose_ex - median(dose_ex))/
  IQR(dose_ex)


#first dose 
first_dose <- data$FirstDose

min_max_fd <- (first_dose - min(first_dose)) / (max(first_dose) - min(first_dose))
z_score_fd <- (first_dose - mean(first_dose)) / sd(first_dose)

#first quantile 
q1_fd <- quantile(first_dose, 0.25)
#third quantile 
q3_fd <- quantile(first_dose, 0.75)
robust_fd <- (first_dose - median(first_dose)) / IQR(first_dose)

# second dose
second_dose <- data$SecondDose

min_max_second_dose <- (second_dose - min(second_dose)) / (max(second_dose) - min(second_dose))
z_score_second_dose <- (second_dose - mean(second_dose)) / sd(second_dose)

#first quantile 
q1_sd <- quantile(second_dose, 0.25)
#third quantile 
q3_sd <- quantile(second_dose, 0.75)
robust_scaled_second_dose <- (second_dose - median(second_dose)) / IQR(second_dose)

# EDA

# histogram of polulation by frequency (fig 3) 
ggplot(data, aes(x = Population)) +
  geom_histogram( fill = "lightblue", color = "black") +
  labs(title = "Histogram of Population", x = "Population", y = "Frequency") +
  theme_minimal()

# bar chart of number of different vaccines used by country reporting 
# number of different vaccines used per country
different_vaccines_per_country <- data %>%
  group_by(ReportingCountry) %>%
  summarize(DifferentVaccinesNumber = n_distinct(Vaccine))


# bar chart of different vaccine types used by reporting country (fig 4) 
ggplot(total_vaccine_types, aes(x = ReportingCountry, y = TotalVaccineTypes, fill = ReportingCountry)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Different Vaccine Types Used vs Reporting Country", x = "Reporting Country", y = "Different Vaccine Used") +
  theme_minimal()


# bar chart of population by reporting country (fig 5)
max_population_by_country <- data %>%
  group_by(ReportingCountry) %>%
  summarize(Population= max(Population))

ggplot(max_population_by_country, aes(x = ReportingCountry, y = Population, fill = ReportingCountry)) +
  geom_bar(stat = "identity") +
  labs(title = "Population by Reporting Country", x = "Reporting Country", y = "Max Population") +
  theme_minimal()

# bar chart of total number of vaccines per target group 

# add total doses as a column in dataset, it will summ up all the doses for each observation 
data$TotalDoses <- rowSums(data[,c("FirstDose","SecondDose","DoseAdditional1"
                                   ,"DoseAdditional2","DoseAdditional3","DoseAdditional4",
                                   "DoseAdditional5","UnknownDose")])

# calcuate total doses per group 
total_doses_per_group <- data %>%
  group_by(TargetGroup) %>%
  summarize(TotalDoses = sum(TotalDoses))

# remove row  'ALL',"AgeUNK', 'HCW', 'LTCF', 'Age<18'
total_doses_per_group <- total_doses_per_group %>%
  filter(TargetGroup != 'ALL')
total_doses_per_group <- total_doses_per_group %>%
  filter(TargetGroup != 'HCW')
total_doses_per_group <- total_doses_per_group %>%
  filter(TargetGroup != 'LTCF')
total_doses_per_group <- total_doses_per_group %>%
  filter(TargetGroup != 'AgeUNK')
total_doses_per_group <- total_doses_per_group %>%
  filter(TargetGroup != 'Age<18')

# display bar chart of total doses per age group  (fig 6)
ggplot(total_doses_per_group, aes(x = TargetGroup, y = TotalDoses, fill = TargetGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Chart of Total Doses per Target Group", x = "Target Group", y = "Total Doses") +
  theme_minimal()

// to do 

numerical_columns <- data[, c("FirstDose", "SecondDose","NumberDosesReceived","DoseAdditional1","DoseAdditional2","UnknownDose","Denominator", "Population")]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_columns)

# Create a heatmap
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Heatmap")














