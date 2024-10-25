#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes )

#Import "main" and "roster" data file. Here we are using FDS Pakistan dataset. 
library(haven)
main <- read_dta("FDS_PAK_2024_Allresp_wRepGroups.dta")
hhroster <- read_dta("FDS_PAK_2024_Roster(hhmembers)_cleaned_wvarlabels.dta")

#Preparing missing elements in the FDS PAK dataset which are needed for RBM reporting
#1. Add age categories and move the coloumn after HH_04 (age in completed years)
hhroster$HH_04_cat <- cut(hhroster$HH_04,
                          breaks = c(-1, 4, 17, 59, Inf),
                          labels = c("0-4", "5-17", "18-59", "60+"))
hhroster <- hhroster %>%
  relocate(HH_04_cat, .after = HH_04)

#Importing disability rotation groups as they are missing from the  hhroster
library(readxl)
FDS_PAK_raw <- read_excel("C:\\Users\\KAPS\\OneDrive - UNHCR\\Documents\\Documents UNHCR\\GDS\\SDSS\\GST\\Indicator mapping\\FDS\\R Scripting\\RBM_Forced_Displacement_Survey\\FDS_PAK_DATA_24102024.xlsx")
dis_seeing <- read_excel("C:\\Users\\KAPS\\OneDrive - UNHCR\\Documents\\Documents UNHCR\\GDS\\SDSS\\GST\\Indicator mapping\\FDS\\R Scripting\\RBM_Forced_Displacement_Survey\\FDS_PAK_DATA_24102024.xlsx", 
                         sheet = "dis_seeing")
dis_walking <- read_excel("C:\\Users\\KAPS\\OneDrive - UNHCR\\Documents\\Documents UNHCR\\GDS\\SDSS\\GST\\Indicator mapping\\FDS\\R Scripting\\RBM_Forced_Displacement_Survey\\FDS_PAK_DATA_24102024.xlsx", 
                          sheet = "dis_walking")
dis_hearing <- read_excel("C:\\Users\\KAPS\\OneDrive - UNHCR\\Documents\\Documents UNHCR\\GDS\\SDSS\\GST\\Indicator mapping\\FDS\\R Scripting\\RBM_Forced_Displacement_Survey\\FDS_PAK_DATA_24102024.xlsx", 
                          sheet = "dis_hearing")
dis_self_care <- read_excel("C:\\Users\\KAPS\\OneDrive - UNHCR\\Documents\\Documents UNHCR\\GDS\\SDSS\\GST\\Indicator mapping\\FDS\\R Scripting\\RBM_Forced_Displacement_Survey\\FDS_PAK_DATA_24102024.xlsx", 
                            sheet = "dis_self_care")
dis_communicating <- read_excel("C:\\Users\\KAPS\\OneDrive - UNHCR\\Documents\\Documents UNHCR\\GDS\\SDSS\\GST\\Indicator mapping\\FDS\\R Scripting\\RBM_Forced_Displacement_Survey\\FDS_PAK_DATA_24102024.xlsx", 
                                sheet = "dis_communicating")
dis_remembering <- read_excel("C:\\Users\\KAPS\\OneDrive - UNHCR\\Documents\\Documents UNHCR\\GDS\\SDSS\\GST\\Indicator mapping\\FDS\\R Scripting\\RBM_Forced_Displacement_Survey\\FDS_PAK_DATA_24102024.xlsx", 
                              sheet = "dis_remembering")
#For each data frame, trim and separate the position of the HH member from the name of the household member. 
#Disability in communication 
dis_communicating <- dis_communicating %>%
  mutate(
    calculate_dis_communicating_person = str_squish(calculate_dis_communicating_person),  
    memberposition_communication = str_extract(calculate_dis_communicating_person, "^[0-9]+"), 
    calculate_dis_communicating_person = str_trim(str_remove(calculate_dis_communicating_person, "^[0-9]+\\s*[-–—]\\s*")))
View(dis_communicating)
dis_communicating <- rename(dis_communicating, "membername_communication" = calculate_dis_communicating_person)


#Disability in seeing
dis_seeing <- rename(dis_seeing, "membername_seeing" = calculate_dis_seeing_person)
dis_seeing <- dis_seeing %>%
  mutate(
    membername_seeing = str_squish(membername_seeing),  
    memberposition_seeing = str_extract(membername_seeing, "^[0-9]+"), 
    membername_seeing = str_trim(str_remove(membername_seeing, "^[0-9]+\\s*[-–—]\\s*")))

#TRY TO PUSH  

##TYPE SOMETHING
