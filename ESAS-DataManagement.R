# ESAS Data management
# Load useful libraries
library(tidyverse)
library(readxl)
library(gt)
library(gtsummary)
library(ggplot2)
library(lubridate)
# Load data - most recent version saved 3/13/2024
setwd("J:/Jim_Gonzalez_Research/DATA/Jim/ESAS_datasets/")
ESAS_raw<-read_excel("MortonPlant/2023_03_11_ESAS Data.xlsx", na = "N/A")

ESAS <- ESAS_raw %>%
  arrange(`Patient ID`, `Date of Visit`) %>%
  group_by(`Patient ID`) %>%
  mutate(week = interval( first(`Date of Visit`), `Date of Visit`) %/% weeks(1),
         reading_num = row_number())

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
  geom_histogram(binwidth = 4, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(title = "Histogram with Blocks of 4 Weeks", x = "Week", y = "Frequency") +
  theme_minimal()

ESAS %>%
  filter(week <= )
  mutate(patient_id = as.factor(`Patient ID`)) %>%
  ggplot(aes(x = week, y = `Total Score`, group = patient_id, color = patient_id)) +
  geom_line() +
  #  geom_point() +
  labs(title = "Spaghetti Plot of ESAS Data", x = "Week", y = "ESAS Total Score") +
  theme_classic()