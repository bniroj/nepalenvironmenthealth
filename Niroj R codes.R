if(!require("tidyverse")) install.packages("tidyverse")
if(!require("haven")) install.packages("haven")
if(!require("survey")) install.packages("survey")
if(!require("ggplot2")) install.packages("ggplot2")

library(survey)
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)

core16 <- read_dta("Y:\\CSTOR_Research\\Projects\\You\\Niroj_Bhandari-NIS\\CORE_16_21.dta")
dx <- read_dta("Y:\\CSTOR_Research\\Projects\\You\\Niroj_Bhandari-NIS\\DX_PR_GRPS_18_21.dta")
hosp <- read_dta("Y:\\CSTOR_Research\\Projects\\You\\Niroj_Bhandari-NIS\\Hospital_16_21.dta")
severity <- read_dta("Y:\\CSTOR_Research\\Projects\\You\\Niroj_Bhandari-NIS\\SEVERITY_16_21.dta")

# Check for duplicates in each data frame
sum(duplicated(core16$KEY_NIS))   # Count of duplicate rows in core16
sum(duplicated(dx$KEY_NIS))      # Count of duplicate rows in dx
#sum(duplicated(hosp$KEY_NIS))    # Count of duplicate rows in hosp
sum(duplicated(severity$KEY_NIS)) # Count of duplicate rows in severity

#remove duplicates
core16 <- core16 %>% 
  distinct(KEY_NIS, .keep_all = TRUE)

nis_merged <- core16 %>% 
  left_join(dx, by ="KEY_NIS") %>%
  left_join(severity, by ="KEY_NIS")

axilobifem <- c("031598", "03150A8", "03150J8","03150K8", "03150Z8", "031698", "03160A8", "03160J8","03160K8", "03160Z8" )
aortobifem <- c("041009K", "04100AK", "04100JK", "04100KK", "04100ZK")

nis_filtered <- nis_merged %>% 
  filter(if_any(starts_with("I10_PR"),~ . %in% c(axilobifem, aortobifem)))

#filter relevant ICD codes and create 0/1 table for aorto and axilbifem

nis_merged <- nis_merged %>%
  mutate(axbifem = if_any(starts_with("I10_PR"),~ . %in% axilobifem ), 
         arbifem = if_any(starts_with("I10_PR"),~ . %in% aortobifem)) %>% 
  mutate(axbifem = as.integer(axbifem), 
         arbifem = as.integer(arbifem))

#drop observations if age < 18
nis_merg <- nis_merged %>%
  filter(AGE >=18)

#count of observations before and after dropping
nrow(nis_merged)
nrow(nis_merg)

#descriptive analysis

sum(nis_merg$arbifem, na.rm = TRUE)
sum(nis_merg$axbifem, na.rm = TRUE)

table(nis_merg$FEMALE, nis_merg$arbifem)

sex_summary <- nis_merg %>%
  group_by(YEAR, FEMALE, axbifem) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count/sum(Count)*100)
print(sex_summary, n = Inf)
sex_summary_1 <- nis_merg %>%
  group_by(YEAR, axbifem) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count/sum(Count)*100)
print(sex_summary_1, n = Inf)

#Age among arbifem and axbifem
nis_merg %>%
  filter(arbifem == 1) %>%
  summarise(
    Mean_Age = mean(AGE, na.rm = TRUE),
    SD_Age = sd(AGE, na.rm = TRUE)
  )

nis_merg %>%
  filter(axbifem == 1) %>%
  summarise(
    Mean_Age = mean(AGE, na.rm = TRUE),
    SD_Age = sd(AGE, na.rm = TRUE)
  )

#race by procedure
nis_merg %>%
  group_by(RACE) %>%
  summarise(arbifemct = sum(arbifem, na.rm = TRUE),
            axbifemct = sum(axbifem, na.rm = TRUE))

#plotting the trend data

trends<- nis_merg %>%
  group_by(YEAR) %>%
  summarise(arbifemct = sum(arbifem, na.rm = TRUE),
            axbifemct = sum(axbifem, na.rm = TRUE)
            )%>%
  pivot_longer(cols = c(arbifemct, axbifemct),
               names_to = "Procedure",
               values_to = "Count")
print(trends)  

ggplot(trends, aes(x = YEAR, y = Count, color = Procedure, group = Procedure)) +
  geom_line(size = 1.2) + 
  #geom_point(size = 3) + 
  labs( title = "Trend of Aortobifemoral vs. Axillobifemoral Bypass Procedures", 
        x = "Year", 
        y = "Number of Procedures", 
        color = "Procedure" 
        ) + theme_minimal() + 
  scale_x_continuous(breaks =seq(min(trends$YEAR), max(trends$YEAR), 1)) +
  theme(text =element_text(size = 14)) 
