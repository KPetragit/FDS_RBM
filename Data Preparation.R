#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven )

#Import "main" and "roster" data file. Here we are using FDS Pakistan dataset. 
library(haven)
main <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/Survey Programme Team/Countries/Pakistan/Data/4 Analysis/FDS_PAK_2024_Allrespondents_complete.dta")

hhroster <- read_dta("C:/Users/KAPS/OneDrive - UNHCR/Survey Programme Team/Countries/Pakistan/Data/4 Analysis/FDS_PAK_2024_Roster_complete.dta")