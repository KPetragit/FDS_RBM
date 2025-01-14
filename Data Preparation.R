#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven )

#Import 5 datasets. Here we are using FDS Pakistan datase.  

HHroster <- read_dta("FDS_PAK_2024_Roster_complete.dta")
main <- read_dta("FDS_PAK_2024_HoH_complete.dta")
RA_adult <- read_dta("FDS_PAK_2024_Random_Member_complete.dta")
RA_woman <- read_dta("FDS_PAK_2024_Random_Woman_complete.dta")
RA_caregiver <- read_dta ("FDS_PAK_2024_Random_Child_complete.dta")


# Rename _uuid to uuid in all datasets
main <- main %>%
  rename(uuid = `_uuid`)

HHroster <- HHroster %>%
  rename(uuid = `_uuid`)

RA_adult <- RA_adult %>%
  rename(uuid = `_uuid`)

RA_woman <- RA_woman %>%
  rename(uuid = `_uuid`)

RA_caregiver <- RA_caregiver %>%
  rename(uuid = `_uuid`)

###Age of the child - Immunization 
# Calculate the age of the child which was asked the questions on immunization
RA_caregiver <- RA_caregiver %>%
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
    HHroster %>% select(uuid, rosterposition, agetouse), # Select relevant columns from hhroster
    by = c("uuid" = "uuid", "selected_childla" = "rosterposition") # Match on uuid and positions
  ) %>%
  rename(child_labor_age = agetouse) # Rename agetouse to child_labor_age


### Household members size
# Create a new variable to count the number of household members in each household 
##Here we are using the variable "member" which assigned a 1 for each person that passes the check for actually being a household member. This check done automatically in the data collection form. 
household_counts <- HHroster %>%
  group_by(uuid) %>%
  summarise(HHmembersize = sum(member == 1, na.rm = TRUE))
##Join with the main data set
main <- main %>%
  left_join(household_counts, by = "uuid")


#Disaggregation variables 
## Roster ----
## 1. Population Groups ----
##Population group variable is available in the main dataset. Here we pull it to the HHroster dataset. 
HHroster <- HHroster %>%
  left_join(
    main %>% select(uuid, Intro_07), # Select relevant columns from main
    by = c("uuid" = "uuid") # Match on uuid 
  ) 


## 2. Age ----

# Convert agetouse from character to numeric
HHroster$agetouse <- as.numeric(HHroster$agetouse)

### Create two new variables for different age categories
### 4 categories (0-4, 5-17, 18-59, 60+)

HHroster$HH_04_cat4 <- cut(HHroster$agetouse, breaks = c(-1, 4, 17, 59, Inf), 
                         labels = c("0-4", "5-17", "18-59", "60+"))

### 2 categories (<18, >18)

HHroster$HH_04_cat2 <- cut(HHroster$agetouse, breaks = c(-1, 17, Inf), 
                         labels = c("0-17", "18-60+"))

table(HHroster$HH_04_cat4)
table(HHroster$HH_04_cat2)

## 3. Disability ----

### Washington Group Index
#Disability status identifiers are calculated based on guidelines by the [Washington Group on Disability Statistics](https://www.washingtongroup-disability.com/fileadmin/uploads/wg/WG_Document__6C_-_Analytic_Guidelines_for_the_WG-ES__Stata_.pdf) for over 5-year-olds. Note that in FDS we have three levels of disability level (some difficulty, a lot of difficulty, cannot do at all, + don't know, refuse to answer).

#Disability is specified as at least one domain/question coded A LOT OF DIFFICULTY or CANNOT DO AT ALL. 

###Step 1: Add the level 0 - no difficulty, for those members above 5 years of age who were not identified as having disabilities. 
#Vision
HHroster <- HHroster %>% 
  mutate(Dis_03 = if_else(is.na(Dis_03) & agetouse > 5, 0, Dis_03))
#Hearing
HHroster <- HHroster %>% 
  mutate(Dis_06 = if_else(is.na(Dis_06) & agetouse > 5, 0, Dis_06))
#Mobility
HHroster <- HHroster %>% 
  mutate(Dis_09 = if_else(is.na(Dis_09) & agetouse > 5, 0, Dis_09))
#Cognition
HHroster <- HHroster %>% 
  mutate(Dis_12 = if_else(is.na(Dis_12) & agetouse > 5, 0, Dis_12))
#Self-care
HHroster <- HHroster %>% 
  mutate(Dis_15 = if_else(is.na(Dis_15) & agetouse > 5, 0, Dis_15))
#Communication
HHroster <- HHroster %>% 
  mutate(Dis_18 = if_else(is.na(Dis_18) & agetouse > 5, 0, Dis_18))

###Step 2: Generate frequency distributions on each of the WG-SS domain variables

### 0 No difficulty 
### 1	Some difficulty
### 2	A lot of difficulties
### 3	Cannot do at all
### 98	Don’t know
### 99	Refused to Answer

#Vision 
barplot(table(HHroster$Dis_03), main = "Vision")
#Hearing
barplot(table(HHroster$Dis_06), main = "Hearing")
#Mobility
barplot(table(HHroster$Dis_09), main = "Mobility")
#Cognition
barplot(table(HHroster$Dis_12), main = "Cognition")
#Self-care
barplot(table(HHroster$Dis_15), main = "Self-care")
#Communication
barplot(table(HHroster$Dis_18), main = "Communicating")


#Step 3: Construct disability variable 
#Initialize disability variable with 2
HHroster$disability <- 2

# Replace disability with NA if all specified columns are missing and age is > 5
HHroster$disability <- ifelse(
  HHroster$agetouse > 5 &
    is.na(HHroster$Dis_03) & is.na(HHroster$Dis_06) & is.na(HHroster$Dis_09) &
    is.na(HHroster$Dis_12) & is.na(HHroster$Dis_15) & is.na(HHroster$Dis_18),
  NA, 
  HHroster$disability
)

# Replace disability with 1 if any of the specified conditions are met and age is > 5
HHroster$disability <- ifelse(
  HHroster$agetouse > 5 & (
    HHroster$Dis_03 %in% c(2, 3) | HHroster$Dis_06 %in% c(2, 3) | 
      HHroster$Dis_09 %in% c(2, 3) | HHroster$Dis_12 %in% c(2, 3) | 
      HHroster$Dis_15 %in% c(2, 3) | HHroster$Dis_18 %in% c(2, 3)
  ),
  1,
  HHroster$disability
)

# Tabulate the disability variable
table(HHroster$disability, useNA = "ifany")

## 4. Country of Origin ----
#The individual information on the country of origin comes from the HHroster. To have one single variable for country of origin information, the country code for the country of enumeration (i.e. PAK for Pakistan) will be entered as below. This question is asked only to individuals older than 15. For individuals younger than 15, the value is equaled to the country of origin of the household (as responded by the Head of the Household)

HHroster <- HHroster %>%
  left_join(
    main %>% select(uuid, origincntry), # Select relevant columns from main
    by = c("uuid" = "uuid") # Match on uuid 
  ) 
HHroster <- HHroster %>%
  rename(origincountry_roster = origincntry) #Rename to origincountry_roster

HHroster <- HHroster %>%
  mutate( # country of origin from ID_00 and ID_00_specify
    COO = case_when(
      ID_00 == 1 ~ "PAK", ##ensure to adjust here the country code (where FDS took place)
      ID_00 == 2 ~ as.character(HHroster$ID_00_specify),
      ID_00 == 3 ~ "Stateless",
      ID_00 == 99 ~ "99",
      ID_00 == 98 ~ "98", 
      is.na(ID_00) ~ as.character(HHroster$origincountry_roster), #For individuals under 15 years, ID_00 is not asked and therefore NA. For these individuals, we take "origincntry" which is a the country of origin of the household (answered by the main)
    )
  ) 


## Head of the Household (main dataset) ----
#1. Age ----
# Age with no categories 
main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, agetouse), # Select relevant columns from hhroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position
  ) %>%
  rename(HH_04_HoH = agetouse) # Rename agetouse to HH_04_hOh


# 2 categories 
main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, HH_04_cat2), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
main <- main %>%
  rename(HH_04_HoH_cat2 = HH_04_cat2) #HH_04_HoH_cat2

# 4 categories 
main <- main %>%
  left_join(
    HHroster %>% select(uuid,rosterposition, HH_04_cat4), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid  and position 
  ) 
main <- main %>%
  rename(HH_04_HoH_cat4 = HH_04_cat4) #HH_04_HoH_cat4

#2. Gender ----

main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, HH_02), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
main <- main %>%
  rename(HH_02_HoH = HH_02) #Rename to HH_02_HoH

#3. Disability ----

main <- main %>%
  left_join(
    HHroster %>% select(uuid, rosterposition, disability), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
main <- main %>%
  rename(disability_HoH = disability) #Rename to disability_HoH

#4. Country of Origin ----

main <- main %>%
  left_join(
    HHroster %>% select(uuid,rosterposition, COO), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
main <- main %>%
  rename(COO_HoH = COO) #Rename to COO_HoH


## Randomly Selected Adult (RA_adult dataset) ----

#1. Age ----
# 2 categories 
RA_adult$age_selected <- as.numeric(as.character(RA_adult$age_selected))

RA_adult$RA_HH_04_cat2 <- cut(RA_adult$age_selected, breaks = c(-1, 17, Inf), 
                              labels = c("0-17", "18-60+"))

# 4 categories 
RA_adult$age_selected <- as.numeric(as.character(RA_adult$age_selected))

RA_adult$RA_HH_04_cat4 <- cut(RA_adult$age_selected, breaks = c(-1, 4, 17, 59, Inf), 
                              labels = c("0-4", "5-17", "18-59", "60+"))

#2. Population Group ----

RA_adult <- RA_adult %>%
  left_join(
    main %>% select(uuid, Intro_07), # Select relevant columns from main dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 

#3. Country of origin ----

RA_adult <- RA_adult %>%
  left_join(
    HHroster %>% select(uuid, COO, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_adult <- RA_adult %>%
  rename(COO_RA = COO) #Rename to COO_RA

#4. Disability ----
RA_adult <- RA_adult %>%
  left_join(
    HHroster %>% select(uuid, disability, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition") # Match on uuid and HHroster position
  ) 
RA_adult <- RA_adult %>%
  rename(disability_RA = disability) #Rename to disability_RA

#5. Gender ----

RA_adult <- RA_adult %>%
  rename(HH_02_RA = `HH_02_selected`)

## Randomly Selected Women (RA_woman dataset) ----

#1. Age ----

RA_woman$agerandomwoman <- as.numeric(as.character(RA_woman$agerandomwoman))

RA_woman$RW_HH_04_cat2 <- cut(RA_woman$agerandomwoman, breaks = c(-1, 17, Inf), 
                              labels = c("0-17", "18-60+"))

# 4 categories 
RA_woman$RW_HH_04_cat4 <- cut(RA_woman$agerandomwoman, breaks = c(-1, 4, 17, 59, Inf), 
                              labels = c("0-4", "5-17", "18-59", "60+"))

#2. Population Group  ----

RA_woman <- RA_woman %>%
  left_join(
    main %>% select(uuid, Intro_07), # Select relevant columns from main dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 


#3. Country of origin ----

RA_woman <- RA_woman %>%
  left_join(
    HHroster %>% select(uuid, COO, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_woman <- RA_woman %>%
  rename(COO_RW = COO) #Rename to COO_RW

#4. Disability ----
RA_woman <- RA_woman %>%
  left_join(
    HHroster %>% select(uuid, disability, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_woman <- RA_woman %>%
  rename(disability_RW = disability) #Rename to disability_RW

## Randomly Selected caregiver (RA_caregiver dataset) ----

#1. Population Group ----

RA_caregiver <- RA_caregiver %>%
  left_join(
    main %>% select(uuid, Intro_07), # Select relevant columns from main dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 

#2. Disability ----
RA_caregiver <- RA_caregiver %>%
  left_join(
    HHroster %>% select(uuid, disability, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition_caregiver" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(disability_RC = disability) #Rename to disability_RC

#3. Country of Origin ----

RA_caregiver <- RA_caregiver %>%
  left_join(
    HHroster %>% select(uuid, COO, rosterposition), # Select relevant columns from HHroster
    by = c("uuid" = "uuid", "rosterposition_caregiver" = "rosterposition") # Match on uuid and HHroster position
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(COO_RC = COO) #Rename to COO_RC

#4. Gender  ----
RA_caregiver <- RA_caregiver %>%
  rename(HH_02_RC = `finalcaregiverSEX`)

#5. Age ----
RA_caregiver <- RA_caregiver %>%
  rename(HH_04_RC = `finalcaregiverAGE`)


#Crowding index

####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep

table(main$HH14) ##How many separate structures or buildings do the members of your household occupy? 
table(main$HHmembersize)

main <- main %>%
  mutate(crowding=HHmembersize/HH14
  ) %>%
  mutate(crowding_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 2)
  )

table(main$crowding_cat)



#### ADD LABELS ----

# Define labels for population groups
popgroup_labels <- c(
  "1" = "Refugees and Asylum Seekers",
  "3" = "Host Community"
)

HHroster <- HHroster %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

main <- main %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_adult <- RA_adult %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_woman <- RA_woman %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

#### Define labels for gender
gender_labels <- c(
  "1" = "Male",
  "2" = "Female"
)

# Apply labels to all gender variables in one block
HHroster <- HHroster %>%
  mutate(HH_02 = recode_factor(HH_02, !!!gender_labels))

main <- main %>%
  mutate(HH_02_HoH = recode_factor(HH_02_HoH, !!!gender_labels))

RA_adult <- RA_adult %>%
  mutate(HH_02_RA = recode_factor(HH_02_RA, !!!gender_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(HH_02_RC = recode_factor(HH_02_RC, !!!gender_labels))

# Define labels for disability
disability_labels <- c(
  "1" = "Disabled",
  "2" = "Non-Disabled"
)

HHroster <- HHroster %>%
  mutate(disability = recode_factor(disability, !!!disability_labels))

main <- main %>%
  mutate(disability_HoH = recode_factor(disability_HoH, !!!disability_labels))

RA_adult <- RA_adult %>%
  mutate(disability_RA = recode_factor(disability_RA, !!!disability_labels))

RA_woman <- RA_woman %>%
  mutate(disability_RW = recode_factor(disability_RW, !!!disability_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(disability_RC = recode_factor(disability_RC, !!!disability_labels))


#Save the datasets in a STATA dta. 

output_path <- "C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis/"

write_dta(HHroster, paste0(output_path, "HHroster.dta"))
write_dta(main, paste0(output_path, "main.dta"))
write_dta(RA_adult, paste0(output_path, "RA_adult.dta"))
write_dta(RA_woman, paste0(output_path, "RA_woman.dta"))
write_dta(RA_caregiver, paste0(output_path, "RA_caregiver.dta"))

#Save the datasets in a R dataset.

saveRDS(HHroster, file = paste0(output_path, "HHroster.rds"))
saveRDS(main, file = paste0(output_path, "main.rds"))
saveRDS(RA_adult, file = paste0(output_path, "RA_adult.rds"))
saveRDS(RA_woman, file = paste0(output_path, "RA_woman.rds"))
saveRDS(RA_caregiver, file = paste0(output_path, "RA_caregiver.rds"))




#Create a new variable for strata, intro_06, submission_date
#Adjust the order of the variables, Pop groups, age, gender, disability, country or origin, strata, submission_date, uuid, 
#change the path to save the datatsets to analysis folder 
#agetouse