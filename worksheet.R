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
ggplot(different_vaccines_per_country, aes(x = ReportingCountry, y = DifferentVaccinesNumber, fill = ReportingCountry)) +
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






# correlation between numerical columns (fig 6)
numerical_columns <- data[, c("FirstDose", "SecondDose","NumberDosesReceived","DoseAdditional1","DoseAdditional2","UnknownDose","Denominator", "Population")]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_columns)

# Create a heatmap
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Heatmap")






# add total doses as a column in dataset, it will sum up all the doses for each observation 
data$TotalDoses <- rowSums(data[,c("FirstDose","SecondDose","DoseAdditional1"
                                   ,"DoseAdditional2","DoseAdditional3","DoseAdditional4",
                                   "DoseAdditional5","UnknownDose")])

# calculate total doses per group 
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

# display bar chart of total doses per age group  (fig 7)
ggplot(total_doses_per_group, aes(x = TargetGroup, y = TotalDoses, fill = TargetGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Chart of Total Doses per Target Group", x = "Target Group", y = "Total Doses") +
  theme_minimal()



# fig 8 - Pie chart of distribution of vaccine type across dataset 

vaccine_type_counts <- table(data$Vaccine)


vaccine_df <- data.frame(Vaccine = names(vaccine_type_counts), Count = as.numeric(vaccine_type_counts))

# Create a pie chart
ggplot(vaccine_df, aes(x = "", y = Count, fill = Vaccine)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Different Types of Vaccines", fill = "Vaccine") +
  theme_minimal() +
  theme(legend.position = "right")



### ONE-HOT ENCODING

one_hot_encoding_vaccine <- as.data.frame(model.matrix(~ Vaccine - 1, data = data))
data_one_hot <- cbind(data, one_hot_encoding_vaccine)
head(data)


### PCA 




library(FactoMineR)

# pca calculations 

pca1 <- data[,c("NumberDosesReceived","FirstDose","SecondDose")]

pca2 <- data[,c("NumberDosesReceived",
               "FirstDose","SecondDose", 
               "DoseAdditional1",
               "DoseAdditional2",
               "DoseAdditional3",
               "DoseAdditional4",
               "DoseAdditional5" )]

## pca function from FactoMiner library 
data.pca1 <- PCA(pca1, scale.unit = TRUE, ncp=5,graph = FALSE)
data.pca2 <- PCA(pca2, scale.unit = TRUE, ncp=5,graph = FALSE)

summary(data.pca1)


#fig 9 
barplot(data.pca1$eig[,2], names.arg = 1:nrow(data.pca$eig),
        main = "Variance by Component",
        xlab = "Principal Component",
        ylab = "Percentage of Variance",
        col = "lightblue"
        )
lines(x=1:nrow(data.pca$eig), data.pca$eig[,2],type = "b", pch=19, col = "green")


## fig 10 
plot.PCA(data.pca1, axes = c(1,2), choix = "var", new.plot = TRUE, col.var = "red",
         col.quanti.sup = "blue", label = c("var","qanti.sup"), lim.cos2.var = 0
         )

## fig 11 
plot.PCA(data.pca2, axes = c(1,2,3,4), choix = "var", new.plot = TRUE, col.var = "red",
         col.quanti.sup = "blue", label = c("var","qanti.sup"), lim.cos2.var = 0
)


plot.PCA(data.pca1, axes = c(1,2), choix = "ind", habillage = "none",
         col.ind = "red", col.ind.sup = "blue", col.quali = "magenta", label = c("ind","ind.sup"
                                                                                 ,"quali"),
         new.plot = TRUE,
         title = "Factor Map"
         )






