
# Script that loads the data from a .dta file and then reads it into an RDS
# object, for manipulation by the Shiny App

# Load neccessary dependencies

library(tidyverse)
library(readstata13)
library(fs)

# Read in data from STATA dta file

couples <- read.dta13("HCMST 2017 fresh sample for public sharing draft v1.1.dta") %>% 
  
  # Recode respondent education levels to collapse all the levels for varying
  # levels of grade school completion into one level - "Never graduated high
  # school"
  
  mutate(ppeduc = fct_lump(ppeduc, 6, 
                           other_level = "Never graduated high school"),
         
         # Recode respondent education levels for more sensible factor names,
         # that will be reflected in the Shiny App UI
         
         ppeduc = fct_recode(ppeduc, "High school graduate" = "HIGH SCHOOL GRADUATE - high school DIPLOMA or the equivalent (GED)",
                             "Bachelor's degree" = "Bachelors degree",
                             "Master's degree" = "Masters degree"),
         
         # Reorder respondent education factor levels to reflect an ascending
         # order of education completed, which will be reflected in the Shiny
         # App UI selector
         
         ppeduc = fct_relevel(ppeduc, "Never graduated high school", 
                              "Some college, no degree", 
                              "High school graduate", 
                              "Associate degree", 
                              "Bachelor's degree", 
                              "Master's degree")) %>%
  
  mutate(w6_q10 = fct_lump(w6_q10, 6, 
                           other_level = "Never graduated high school"),
         
         # Recode respondent education levels for more sensible factor names,
         # that will be reflected in the Shiny App UI
         
         w6_q10 = fct_recode(w6_q10, "High school graduate" = "HS graduate or GED"),
         
         # Reorder respondent education factor levels to reflect an ascending
         # order of education completed, which will be reflected in the Shiny
         # App UI selector
         
         w6_q10 = fct_relevel(w6_q10, "Never graduated high school", 
                              "Some college, no degree", 
                              "High school graduate", 
                              "Associate degree", 
                              "Bachelor's degree", 
                              "Master's degree")) %>% 
  
  # Recode couple identity levels for more sensible factor names,
  # that will be reflected in the Shiny App UI
  
  mutate(w6_same_sex_couple = fct_recode(w6_same_sex_couple, 
                                         "Heterosexual Couples" = "NOT same-sex souple",
                                         "Same Sex Couples" = "same_sex_couple"))

# Clean column names for couple dataset - remove repeated hcm2017q24 header

colnames(couples) <- gsub('hcm2017q24_', '', colnames(couples))
  
# Create a reshaped version of the couples dataset in which the original wide
# format is made more narrow - previously each meeting type had its own column;
# now all of the meeting types are factor levels under the single variable name
# "meeting_type"

  gathered_couples <- 
    
    couples %>% 
    
    # Include all possible meeting types included in this survey in the gather
    # command
    
    gather(`school`:`met_online`, 
           key = "meeting_type", 
           value = "value") %>% 
    
    # Recode different meeting type values to properly formatted, informative
    # factor labels for the Shiny App UI
    
    mutate(meeting_type = fct_recode(meeting_type, 
                                     "Primary or Secondary School" = "school",
                                     "College" = "college",
                                     "Military"  = "mil",
                                     "Church" = "church",
                                     "Volunteer Organization" = "vol_org",
                                     "Customer-Client Relationship" = "customer",
                                     "Bar or Restaurant" = "bar_restaurant",
                                     "Private Party" = "party",
                                     "Internet" = "internet_other",
                                     "Internet Dating or Phone App" = "internet_dating",
                                     "Internet Social Network" = "internet_soc_network",
                                     "Online Gaming" = "internet_game",
                                     "Internet Chat" = "internet_chat",
                                     "Internet Site" = "internet_org",
                                     "Public Place" = "public",
                                     "Blind Date" = "blind_date",
                                     "On Vacation" = "vacation",
                                     "One-time Service Interaction" = "single_serve_nonint",
                                     "Business Trip" = "business_trip",
                                     "Work Neighbors" = "work_neighbors",
                                     "Met Online" = "met_online")) %>% 
    
    # Filter only for those with a value of "yes" to remove irrelevant rows
    # generated when the data was reshaped - each caseID now has a row for each
    # meeting type, but only the ones where a given meeting type occurred
    # contain relevant information -- the rest are placeholders
    
    # It is true that a given couple may still be represented as multiple rows
    # because there may have been more than one meeting type associated with
    # that couple, however, this provides the best list of all reported meeting
    # types
    
    filter(value == "yes")
  
# Create temp file to write the R data structures to

couples_file <- tempfile()

# Creating a temp file for the gathered_couples data structure will prevent us
# from having to redo these data processing steps - the Shiny App can directly
# read in that version of the data as needed

gathered_couples_file <- tempfile()

# Write the R data structure to the temporary files, ensuring that they are in
# the Shiny App folder directory

write_rds(couples, "how_couples_meet/couples_file.rds")

write_rds(gathered_couples, "how_couples_meet/gathered_couples_file.rds")
