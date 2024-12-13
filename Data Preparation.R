#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven )

#Import 5 datasets. Here we are using FDS Pakistan dataset. 
library(haven)
roster <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis/FDS_PAK_2024_Roster_complete.dta")
HoH <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis/FDS_PAK_2024_HoH_complete.dta"
)
RA_adult <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis/FDS_PAK_2024_Random_Member_complete.dta"
)
RA_woman <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis/FDS_PAK_2024_Random_Woman_complete.dta"
)
RA_caregiver <- read_dta ("C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Pakistan/Data Management/4 Analysis/FDS_PAK_2024_Random_Child_complete.dta"
)


# Rename _uuid to uuid in all datasets
HoH <- HoH %>%
  rename(uuid = `_uuid`)

roster <- roster %>%
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
HoH <- HoH %>%
  left_join(
    roster %>% select(uuid, rosterposition, HH_04), # Select relevant columns from hhroster
    by = c("uuid" = "uuid", "selected_childla" = "rosterposition") # Match on uuid and positions
  ) %>%
  rename(child_labor_age = HH_04) # Rename HH_04 to child_labor_age


### Household members size
# Create a new variable to count the number of household members in each household 
##Here we are using the variable "member" which assigned a 1 for each person that passes the check for actually being a household member. This check done automatically in the data collection form. 
household_counts <- roster %>%
  group_by(uuid) %>%
  summarise(HHmembersize = sum(member == 1, na.rm = TRUE))
##Join with the main data set
HoH <- HoH %>%
  left_join(household_counts, by = "uuid")


#Disintegration variables 
## Roster 
## 1. Population Groups - Population group variable is available in the HoH dataset. Here we pull it to the roster dataset. 
roster <- roster %>%
  left_join(
    HoH %>% select(uuid, Intro_07), # Select relevant columns from HoH
    by = c("uuid" = "uuid") # Match on uuid 
  ) 
roster <- roster %>%
  rename(Intro_07_roster = Intro_07) #Rename to Intro_07_roster 

##2. Age
### Create two new variables for different age categories
### 4 categories (0-4, 5-17, 18-59, 60+)

roster$HH_04_cat4 <- cut(roster$HH_04, breaks = c(-1, 4, 17, 59, Inf), 
                         labels = c("0-4", "5-17", "18-59", "60+"))

### 2 categories (<18, >18)

roster$HH_04_cat2 <- cut(roster$HH_04, breaks = c(-1, 17, Inf), 
                         labels = c("0-17", "18-60+"))

table(roster$HH_04_cat4)
table(roster$HH_04_cat2)

### 3. Disability 

### Washington Group Index
#Disability status identifiers are calculated based on guidelines by the [Washington Group on Disability Statistics](https://www.washingtongroup-disability.com/fileadmin/uploads/wg/WG_Document__6C_-_Analytic_Guidelines_for_the_WG-ES__Stata_.pdf) for over 5-year-olds. Note that in FDS we have three levels of disability level (some difficulty, a lot of difficulty, cannot do at all, + don't know, refuse to answer).

#Disability is specified as at least one domain/question coded A LOT OF DIFFICULTY or CANNOT DO AT ALL. 

###Step 1: Add the level 0 - no difficulty, for those members above 5 years of age who were not identified as having disabilities. 
#Vision
roster <- roster %>% 
  mutate(Dis_03 = if_else(is.na(Dis_03) & HH_04 > 5, 0, Dis_03))
#Hearing
roster <- roster %>% 
  mutate(Dis_06 = if_else(is.na(Dis_06) & HH_04 > 5, 0, Dis_06))
#Mobility
roster <- roster %>% 
  mutate(Dis_09 = if_else(is.na(Dis_09) & HH_04 > 5, 0, Dis_09))
#Cognition
roster <- roster %>% 
  mutate(Dis_12 = if_else(is.na(Dis_12) & HH_04 > 5, 0, Dis_12))
#Self-care
roster <- roster %>% 
  mutate(Dis_15 = if_else(is.na(Dis_15) & HH_04 > 5, 0, Dis_15))
#Communication
roster <- roster %>% 
  mutate(Dis_18 = if_else(is.na(Dis_18) & HH_04 > 5, 0, Dis_18))

###Step 2: Generate frequency distributions on each of the WG-SS domain variables

### 0 No difficulty 
### 1	Some difficulty
### 2	A lot of difficulties
### 3	Cannot do at all
### 98	Don’t know
### 99	Refused to Answer

#Vision 
barplot(table(roster$Dis_03), main = "Vision")
#Hearing
barplot(table(roster$Dis_06), main = "Hearing")
#Mobility
barplot(table(roster$Dis_09), main = "Mobility")
#Cognition
barplot(table(roster$Dis_12), main = "Cognition")
#Self-care
barplot(table(roster$Dis_15), main = "Self-care")
#Communication
barplot(table(roster$Dis_18), main = "Communicating")


##Step 3: Codes (99) Refuse to answer and (98) Don’t know, are recoded to Missing.

roster <- roster %>%
  mutate(
    Dis_03 = ifelse(Dis_03 == 98 | Dis_03 == 99, NA, Dis_03),
    Dis_06 = ifelse(Dis_06  == 98 | Dis_06  == 99, NA, Dis_06),
    Dis_09  = ifelse(Dis_09 == 98 | Dis_09 == 99, NA, Dis_09),
    Dis_12 = ifelse(Dis_12 == 98 | Dis_12 == 99, NA, Dis_12),
    Dis_15 = ifelse(Dis_15 == 98 | Dis_15 == 99, NA, Dis_15),
    Dis_18 = ifelse(Dis_18 == 98 | Dis_18 == 99, NA, Dis_18)
  )

## Step 4: Create disability status indicator for the Washington Group short set on disability

roster <- roster %>%
  mutate(disability = case_when(
    Dis_03 %in% c(2, 3) |
      Dis_06 %in% c(2, 3) |
      Dis_09 %in% c(2, 3) |
      Dis_12 %in% c(2, 3) |
      Dis_15 %in% c(2, 3) |
      Dis_18 %in% c(2, 3) ~ 1,
    Dis_03 %in% c(0, 1) |
      Dis_06 %in% c(0, 1)|
      Dis_09 %in% c(0, 1)|
      Dis_12 %in% c(0, 1)|
      Dis_15 %in% c(0, 1)|
      Dis_18 %in% c(0, 1) ~ 2, 
    TRUE ~ NA_real_
  ))


### 4. Country of Origin 
#The individual information on the country of origin comes from the roster. To have one single variable for country of origin information, the country code for the country of enumeration (i.e. PAK for Pakistan) will be entered as below. This question is asked only to individuals older than 15. For individuals younger than 15, the value is equaled to the country of origin of the household (as responded by the Head of the Household)

roster <- roster %>%
  left_join(
    HoH %>% select(uuid, origincntry), # Select relevant columns from HoH
    by = c("uuid" = "uuid") # Match on uuid 
  ) 
roster <- roster %>%
  rename(origincountry_roster = origincntry) #Rename to Intro_07_roster

roster <- roster %>%
  mutate( # country of origin from ID_00 and ID_00_specify
    COO = case_when(
      ID_00 == 1 ~ "PAK", ##ensure to adjust here the country code (where FDS took place)
      ID_00 == 2 ~ as.character(roster$ID_00_specify),
      ID_00 == 3 ~ "Stateless",
      ID_00 == 99 ~ "99",
      ID_00 == 98 ~ "98", 
      is.na(ID_00) ~ as.character(roster$origincountry_roster), #For individuals under 15 years, ID_00 is not asked and therefore NA. For these individuals, we take "origincntry" which is a the country of origin of the household (answered by the HoH)
    )
  ) 


## Head of the Household (HoH dataset)
#1. Age
# Age with no categories 
HoH <- HoH %>%
  left_join(
    roster %>% select(uuid, rosterposition, HH_04), # Select relevant columns from hhroster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position
  ) %>%
  rename(HH_04_HoH = HH_04) # Rename HH_04 to HH_04_hOh


# 2 categories 
HoH <- HoH %>%
  left_join(
    roster %>% select(uuid, rosterposition, HH_04_cat2), # Select relevant columns from roster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
HoH <- HoH %>%
  rename(HH_04_HoH_cat2 = HH_04_cat2) #HH_04_HoH_cat2

# 4 categories 
HoH <- HoH %>%
  left_join(
    roster %>% select(uuid,rosterposition, HH_04_cat4), # Select relevant columns from roster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid  and position 
  ) 
HoH <- HoH %>%
  rename(HH_04_HoH_cat4 = HH_04_cat4) #HH_04_HoH_cat4

#2. Gender

HoH <- HoH %>%
  left_join(
    roster %>% select(uuid, rosterposition, HH_02), # Select relevant columns from roster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
HoH <- HoH %>%
  rename(HH_02_HoH = HH_02) #Rename to HH_02_HoH

#3. Disability 

HoH <- HoH %>%
  left_join(
    roster %>% select(uuid, rosterposition, disability), # Select relevant columns from roster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
HoH <- HoH %>%
  rename(disability_HoH = disability) #Rename to disability_HoH

#4. Country of Origin 

HoH <- HoH %>%
  left_join(
    roster %>% select(uuid,rosterposition, COO), # Select relevant columns from roster
    by = c("uuid" = "uuid", "HHposinfo" = "rosterposition") # Match on uuid and position 
  ) 
HoH <- HoH %>%
  rename(COO_HoH = COO) #Rename to COO_HoH


## Randomly Selected Adult (RA_adult dataset)

#1. Age
# 2 categories 
RA_adult$age_selected <- as.numeric(as.character(RA_adult$age_selected))

RA_adult$RA_HH_04_cat2 <- cut(RA_adult$age_selected, breaks = c(-1, 17, Inf), 
                              labels = c("0-17", "18-60+"))

# 4 categories 
RA_adult$age_selected <- as.numeric(as.character(RA_adult$age_selected))

RA_adult$RA_HH_04_cat4 <- cut(RA_adult$age_selected, breaks = c(-1, 4, 17, 59, Inf), 
                         labels = c("0-4", "5-17", "18-59", "60+"))

#2. Population Group 

RA_adult <- RA_adult %>%
  left_join(
    HoH %>% select(uuid, Intro_07), # Select relevant columns from HoH dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 
RA_adult <- RA_adult %>%
  rename(Intro_07_RA = Intro_07) #Rename to Intro_07_RA

#3. Country of origin

RA_adult <- RA_adult %>%
  left_join(
    roster %>% select(uuid, COO, rosterposition), # Select relevant columns from roster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and roster position
  ) 
RA_adult <- RA_adult %>%
  rename(COO_RA = COO) #Rename to COO_RA

#4. Disability 
RA_adult <- RA_adult %>%
  left_join(
    roster %>% select(uuid, disability, rosterposition), # Select relevant columns from roster
    by = c("uuid" = "uuid", "rosterposition") # Match on uuid and roster position
  ) 
RA_adult <- RA_adult %>%
  rename(disability_RA = disability) #Rename to disability_RA

#5. Gender 

RA_adult <- RA_adult %>%
  rename(HH_02_RA = `HH_02_selected`)

## Randomly Selected Women (RA_woman dataset)

RA_woman$agerandomwoman <- as.numeric(as.character(RA_woman$agerandomwoman))

RA_woman$RW_HH_04_cat2 <- cut(RA_woman$agerandomwoman, breaks = c(-1, 17, Inf), 
                              labels = c("0-17", "18-60+"))

# 4 categories 
RA_woman$RW_HH_04_cat4 <- cut(RA_woman$agerandomwoman, breaks = c(-1, 4, 17, 59, Inf), 
                              labels = c("0-4", "5-17", "18-59", "60+"))

#2. Population Group 

RA_woman <- RA_woman %>%
  left_join(
    HoH %>% select(uuid, Intro_07), # Select relevant columns from HoH dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 
RA_woman <- RA_woman %>%
  rename(Intro_07_RW = Intro_07) #Rename to Intro_07_RW

#3. Country of origin

RA_woman <- RA_woman %>%
  left_join(
    roster %>% select(uuid, COO, rosterposition), # Select relevant columns from roster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and roster position
  ) 
RA_woman <- RA_woman %>%
  rename(COO_RW = COO) #Rename to COO_RW

#4. Disability 
RA_woman <- RA_woman %>%
  left_join(
    roster %>% select(uuid, disability, rosterposition), # Select relevant columns from roster
    by = c("uuid" = "uuid", "rosterposition" = "rosterposition") # Match on uuid and roster position
  ) 
RA_woman <- RA_woman %>%
  rename(disability_RW = disability) #Rename to disability_RW

## Randomly Selected caregiver (RA_caregiver dataset)

#1. Population Group 

RA_caregiver <- RA_caregiver %>%
  left_join(
    HoH %>% select(uuid, Intro_07), # Select relevant columns from HoH dataset
    by = c("uuid" = "uuid") # Match on uuid 
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(Intro_07_RC = Intro_07) #Rename to Intro_07_RC

#2. Disability 
RA_caregiver <- RA_caregiver %>%
  left_join(
    roster %>% select(uuid, disability, rosterposition), # Select relevant columns from roster
    by = c("uuid" = "uuid", "rosterposition_caregiver" = "rosterposition") # Match on uuid and roster position
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(disability_RC = disability) #Rename to disability_RC

#3. Country of Origin

RA_caregiver <- RA_caregiver %>%
  left_join(
    roster %>% select(uuid, COO, rosterposition), # Select relevant columns from roster
    by = c("uuid" = "uuid", "rosterposition_caregiver" = "rosterposition") # Match on uuid and roster position
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(COO_RC = COO) #Rename to COO_RC

#4. Gender 
RA_caregiver <- RA_caregiver %>%
  rename(HH_02_RC = `finalcaregiverSEX`)

#5. Age
RA_caregiver <- RA_caregiver %>%
  rename(HH_04_RC = `finalcaregiverAGE`)
