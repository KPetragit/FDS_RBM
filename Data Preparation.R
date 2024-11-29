#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven )

#Import "main" and "roster" data file. Here we are using FDS Pakistan dataset. 
library(haven)
#main <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis/FDS_PAK_2024_Allrespondents_complete.dta")

#hhroster <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis/FDS_PAK_2024_Roster_complete.dta"
#)

# Rename _uuid to uuid in both datasets
main <- main %>%
  rename(uuid = `_uuid`)

hhroster <- hhroster %>%
  rename(uuid = `_uuid`)

###Age of the child - Immunization 
# Calculate the age of the child which was asked the questions on immunization
main <- main %>%
  mutate(
    childnametouseAGE = as.numeric(childnametouseAGE),  # Convert to numeric
    child_age_immunization = case_when(
      childnametouseAGE != 99 ~ childnametouseAGE,  # Use childnametouseAGE if it’s not 99
      childnametouseAGE == 99 & AN1_5 == 2 ~ age_child_5,
      childnametouseAGE == 99 & AN1_4 == 2 ~ age_child_4,
      childnametouseAGE == 99 & AN1_3 == 2 ~ age_child_3,
      childnametouseAGE == 99 & AN1_2 == 2 ~ age_child_2,
      childnametouseAGE == 99 & AN1 == 2 ~ age_child_1,
      TRUE ~ NA_real_  # Set to NA (blank) if none of the conditions are met
    )
  )
###Age of the child - Child Labour

# Pull the age of the child about whom he child labour questions are asked 
# Perform a left join and extract the child's age
main <- main %>%
  left_join(
    hhroster %>% select(uuid, rosterposition, HH_04), # Select relevant columns from hhroster
    by = c("uuid" = "uuid", "selected_childla" = "rosterposition") # Match on uuid and positions
  ) %>%
  rename(child_labor_age = HH_04) # Rename HH_04 to child_labor_age


### Household members size
# Create a new variable to count the number of household members in each household 
##Here we are using the variable "member" which assigned a 1 for each person that passes the check for actually being a household member. This check done automatically in the data collection form. 
household_counts <- hhroster %>%
  group_by(uuid) %>%
  summarise(HHmembersize = sum(member == 1, na.rm = TRUE))
##Join with the main data set
main <- main %>%
  left_join(household_counts, by = "uuid")


### Washington Group Index
#Disability status identifiers are calculated based on guidelines by the Washington Group on Disability Statistics for over 5-year-olds. Note that in FDS we have three levels of disability level (some difficulty, a lot of difficulty, cannot do at all).


#Conditions for different disability identifiers:
  
#Category 1: any domain = 1, 2 or 3

#Category 2: at least two domains = 1 or at least one domain = 2 or 3

#Category 3: at least one domain = 2 or 3

#Category 4: at least one domain = 3

#The WG recommends using category as the indicator for disability status.

#Step 1: Sum disability cases across domains
hhroster <- hhroster %>%
  mutate(disabilitySeverity1_count = rowSums(
    select(., all_of(c("Dis_03", "Dis_06", "Dis_09", "Dis_12", "Dis_15", "Dis_18"))) == 1, #looking at disability level 1 = Some difficulty
    na.rm = TRUE))
# "disabilitySeverity1_count" represents the number of domains with disability severity 1 (some difficulty).

hhroster <- hhroster %>%
  mutate(disabilitySeverity2_count = rowSums(
    select(., all_of(c("Dis_03", "Dis_06", "Dis_09", "Dis_12", "Dis_15", "Dis_18"))) == 2, #looking at disability level 2 = A lot of difficulty
    na.rm = TRUE))
# "disabilitySeverity2_count" represents the number of domains with disability severity 2 (A lot of difficulty).

hhroster <- hhroster %>%
  mutate(disabilitySeverity3_count = rowSums(
    select(., all_of(c("Dis_03", "Dis_06", "Dis_09", "Dis_12", "Dis_15", "Dis_18"))) == 3, #looking at disability level 3 = Cannot do at all
    na.rm = TRUE))
# "disabilitySeverity3_count" represents the number of domains with disability severity 3 (Cannot do at all).


#Step 2: Classify disability identifiers 
#WG Disability Status Identifier 1 = Category 1

hhroster <- hhroster %>%
  mutate(
    disability1 = case_when(
      HH_04 >= 5 & (disabilitySeverity1_count > 0 | 
                         disabilitySeverity2_count > 0 | 
                         disabilitySeverity3_count > 0) ~ 1, # Disability exists
      HH_04 >= 5 & (disabilitySeverity1_count == 0 & 
                         disabilitySeverity2_count == 0 & 
                         disabilitySeverity3_count == 0) ~ 2, # No disability
      TRUE ~ NA_real_ # Default for missing or unmatched cases
    )
  )
var_label(hhroster$disability1) <- "WGI Disability Status Identifier 1"

#WG Disability Status Identifier 2 = Category 2
hhroster <- hhroster %>%
  mutate(
    disability2 = case_when(
      HH_04 >= 5 & (disabilitySeverity1_count > 1 | 
                         disabilitySeverity2_count > 0 | 
                         disabilitySeverity3_count > 0) ~ 1, # Disability exists
      HH_04 >= 5 & (disabilitySeverity1_count <= 1 & 
                         disabilitySeverity2_count == 0 & 
                         disabilitySeverity3_count == 0) ~ 2, # No disability
      TRUE ~ NA_real_ # Default case for missing or unmatched conditions
    )
  )
var_label(hhroster$disability2) <- "WGI Disability Status Identifier 2"

#WG Disability Status Identifier 3 = Category 3
hhroster <- hhroster %>%
  mutate(
    disability3 = case_when(
      HH_04 >= 5 & (disabilitySeverity2_count > 0 | 
                         disabilitySeverity3_count > 0) ~ 1, # Disability exists
      HH_04 >= 5 & (disabilitySeverity2_count == 0 & 
                         disabilitySeverity3_count == 0) ~ 2, # No disability
      TRUE ~ NA_real_ # Default case for unmatched conditions
    )
  )
var_label(hhroster$disability3) <- "WGI Disability Status Identifier 3"

#WG Disability Status Identifier 4 = Category 4
hhroster <- hhroster %>%
  mutate(
    disability4 = case_when(
      HH_04 >= 5 & (disabilitySeverity3_count > 0) ~ 1,  # Disability exists
      HH_04 >= 5 & (disabilitySeverity3_count == 0) ~ 2, # No disability
      TRUE ~ NA_real_ # Default case for unmatched conditions
    )
  )
var_label(hhroster$disability4) <- "WGI Disability Status Identifier 4"
