
# Script that loads the data from a .dta file and then reads it into an RDS
# object, for manipulation by the Shiny App

library(tidyverse)
library(readstata13)
library(fs)

# Read in data from STATA dta file

couples <- read.dta13("HCMST 2017 fresh sample for public sharing draft v1.1.dta") %>% 
  mutate(ppeduc = fct_lump(ppeduc, 6, 
                           other_level = "Never graduated high school"),
         ppeduc = fct_recode(ppeduc, "High school graduate" = "HIGH SCHOOL GRADUATE - high school DIPLOMA or the equivalent (GED)"),
         ppeduc = fct_relevel(ppeduc, "Never graduated high school", 
                              "Some college, no degree", 
                              "High school graduate", 
                              "Associate degree", 
                              "Bachelors degree", 
                              "Masters degree", 
                              "Professional or Doctorate degree")) %>% 
  mutate(marry_age = as.numeric(Q21D_Year) - (2017 - ppage))

  colnames(couples) <- gsub('hcm2017q24_', '', colnames(couples))
  
  gathered_couples <- 
    couples %>% 
    gather(`_school`:`_met_online`, 
           key = "meeting_type", 
           value = "value") %>% 
    filter(value == "yes")
  
  # Remove in final script
  
  gathered_couples %>% count(meeting_type) %>% arrange(desc(n))
  
  gathered_couples %>% count(meeting_type) %>% arrange(desc(n)) %>% mutate(n = n / n())
  
  gathered_couples %>% group_by(ppeduc) %>% count(meeting_type) %>% arrange(desc(n)) %>% View()
  
  gathered_couples2 <- gathered_couples %>% gather(`_R_cowork`:`_btwn_I_neighbor`, key = "connections", value = "connect_value") %>% filter(connect_value == "yes")

  couples_orig <- read.dta13("HCMST_ver_3.04.dta")
  
  gathered_couples %>% filter(Q25 == "Same High School") %>% count(meeting_type) %>% arrange(desc(n))
  # roughly half those who went to the same high school as their partner met their partner in high school
  #Q12
  
  # Do attended same school but did not meet because of school
  # Do attended same college, but did not meet because of college

# Create temp file to write the R data structure to

couples_file <- tempfile()

# Write the R data structure to the temporary file

write_rds(couples, "how_couples_meet/couples_file.rds")

