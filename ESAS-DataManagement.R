# ESAS Data management
# Load useful libraries
library(tidyverse)
library(readxl)
library(gt)
library(gtsummary)
library(ggplot2)
library(lubridate)
library(lme4)
# Load data - most recent version saved 3/13/2024
setwd("J:/Jim_Gonzalez_Research/DATA/Jim/ESAS_datasets/")

ESAS_raw<-read_excel("MortonPlant/2023_03_11_ESAS Data.xlsx", na = "N/A")
ESAS_raw<-read_excel("/Volumes/jim_gonzalez_research/DATA/Jim/ESAS_datasets/MortonPlant/2023_03_11_ESAS Data.xlsx", na = "N/A")
ESAS <- ESAS_raw %>%
  mutate(patient_id = as.factor(`Patient ID`)) %>%
  arrange(`Patient ID`, `Date of Visit`) %>%
  group_by(`Patient ID`) %>%
  mutate(week = interval( first(`Date of Visit`), `Date of Visit`) %/% weeks(1),
         reading_num = row_number(), 
         visit = ifelse(`Number of Hospitalizations or ED Visits`>= 1, 1, 0))

test<- ESAS %>%
  filter(week == 0) %>%
  group_by(patient_id) %>%
  count()
# Plot spaghetti plot
ESAS %>%
  mutate(patient_id = as.factor(`Patient ID`)) %>%
  ggplot(aes(x = week, y = `Total Score`, group = patient_id, color = patient_id)) +
  geom_line() +
#  geom_point() +
  labs(title = "Spaghetti Plot of ESAS Data", x = "Week", y = "ESAS Total Score") +
  theme_classic()

# Create histogram with blocks of 4 weeks
ESAS %>%
  ggplot(aes(x = week)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(title = "Histogram with Blocks of 4 Weeks", x = "Week", y = "Frequency") +
  theme_minimal()

ESAS %>%
  filter(patient_id == 50) %>%
  mutate(patient_id = as.factor(`Patient ID`)) %>%
  ggplot(aes(x = week, y = `Total Score`, group = patient_id, color = patient_id)) +
  geom_line() +
  #  geom_point() +
  labs(title = "Spaghetti Plot of ESAS Data", x = "Week", y = "ESAS Total Score") +
  theme_classic()

ESAS_n <- ESAS %>%
  group_by(patient_id) %>%
  mutate(n_weeks = n())


data<-ESAS[!is.na(ESAS$`Total Score`),]
# Using by() function
patient_coefficients <- by(data, data$patient_id, function(subset) {
  lm_model <- lm(`Total Score` ~ week, data = subset)
  intercept <- coef(lm_model)[1]
  slope <- coef(lm_model)[2]
  return(data.frame(patient_id = unique(subset$patient_id),
                    intercept = intercept,
                    slope = slope))
})

# Combine the results into a single data frame
patient_coefficients <- do.call(rbind, patient_coefficients)

# View the results
print(patient_coefficients)
model_1 <- lmer(`Total Score` ~ week + (1 + week| patient_id), data = ESAS, 
                control = lmerControl(optimizer = "Nelder_Mead"))
estimates_1 <- coef(model_1)$patient_id %>%
  rownames_to_column(var = "patient_id") %>%
  rename(intercept = `(Intercept)`, 
         slope = week) %>%
  merge(ESAS_n, by = "patient_id", all = T)
write.csv(estimates_1, 
          "/Volumes/jim_gonzalez_research/DATA/Jim/ESAS_datasets/MortonPlant/estimate_all.csv", row.names = T)

model_2 <- lmer(`Total Score` ~ week + (1 + week| patient_id), data = ESAS_n[ESAS_n$n_weeks > 1,], 
                control = lmerControl(optimizer = "Nelder_Mead")) 
estimates_2 <- coef(model_2)$patient_id %>%
  rownames_to_column(var = "patient_id") %>%
  rename(intercept = `(Intercept)`, 
         slope = week) %>%
  merge(ESAS_n, by = "patient_id", all = T) %>%
  mutate(intercept = ifelse(n_weeks == 1, `Total Score`, intercept), 
         slope = ifelse(n_weeks == 1, 0, slope))
  
write.csv(estimates_2, 
          "/Volumes/jim_gonzalez_research/DATA/Jim/ESAS_datasets/MortonPlant/estimate_MoreTime.csv", row.names = T)