
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
    gather(`school`:`met_online`, 
           key = "meeting_type", 
           value = "value") %>% 
    filter(value == "yes")
  
  # Remove in final script
  
  gathered_couples %>% count(meeting_type) %>% arrange(desc(n))
  
  gathered_couples %>% filter(Q5 == "Yes, we are a same-sex couple") %>% count(meeting_type) %>% arrange(desc(n))
  
  gathered_couples %>% filter(xlgb == "LGB sample") %>% count(meeting_type) %>% arrange(desc(n))
  
  gathered_couples %>% count(meeting_type) %>% arrange(desc(n)) %>% mutate(n = n / n())
  
  gathered_couples %>% group_by(ppeduc) %>% count(meeting_type) %>% arrange(desc(n)) %>% View()
  
  gathered_couples2 <- gathered_couples %>% gather(`R_cowork`:`btwn_I_neighbor`, key = "connections", value = "connect_value") %>% filter(connect_value == "yes")

  gathered_couples %>% filter(meeting_type == "blind_date") %>% count(PPREG4)
  
  gathered_couples %>% 
    filter(met_through_family != "yes" 
           & met_through_as_nghbrs != "yes" 
           & met_through_friend != "yes" 
           & met_as_through_cowork != "yes") %>% 
    count(meeting_type) %>% 
    arrange(desc(n))
  
  c4 <- couples %>% filter(met_through_family != "yes" 
                     & met_through_as_nghbrs != "yes" 
                     & met_through_friend != "yes" 
                     & met_as_through_cowork != "yes") %>%
    gather(`school`:`met_online`, 
           key = "meeting_type", 
           value = "value") %>% 
    filter(value == "yes")
  
  gathered_couples %>% filter(meeting_type == "college") %>% 
    count(met_through_friend) %>% 
    arrange(desc(n))
  
  
  # blind dates
  
  gathered_couples %>% filter(meeting_type == "blind_date") %>% count(ppagecat)
  
  couples_orig <- read.dta13("HCMST_ver_3.04.dta")
  
  gathered_couples %>% filter(Q25 == "Same High School") %>% count(meeting_type) %>% arrange(desc(n))
  # roughly half those who went to the same high school as their partner met their partner in high school
  #Q12
  
  # Do attended same school but did not meet because of school
  # Do attended same college, but did not meet because of college

# Create temp file to write the R data structure to

couples_file <- tempfile()

gathered_couples_file <- tempfile()

# Write the R data structure to the temporary file

write_rds(couples, "how_couples_meet/couples_file.rds")

write_rds(gathered_couples, "how_couples_meet/gathered_couples_file.rds")


c4 <- couples %>% filter(met_through_family != "yes" 
                         & met_through_as_nghbrs != "yes" 
                         & met_through_friend != "yes" 
                         & met_as_through_cowork != "yes") %>%
  gather(`school`:`met_online`, 
         key = "meeting_type", 
         value = "value") %>% 
  filter(value == "yes") %>% count(meeting_type) %>% arrange(desc(n))

#regardless of income, meeting the same ways

htmlwidgets::saveWidget(config(x, displayModeBar = FALSE), "graph.html")

couples %>% filter(Q12 != "Refused") %>% group_by(Q12, partyid7) %>% count()

couples %>% filter(Q12 != "Refused") %>% group_by(Q12, partyid7) %>% count() %>% View()

gathered_couples %>% filter(meeting_type == "church") %>% count(ppincimp)

couples %>% filter(partnership_status == "married") %>% gather(`school`:`met_online`, 
                                                               key = "meeting_type", 
                                                               value = "value") %>% 
  filter(value == "yes") %>% count(meeting_type) %>% arrange(desc(n))

couples %>% filter(partnership_status == "partnered, not married") %>% gather(`school`:`met_online`, 
                                                                              key = "meeting_type", 
                                                                              value = "value") %>% 
  filter(value == "yes") %>% count(meeting_type) %>% arrange(desc(n))

x <- couples %>% ggplot(aes(x = age_when_met)) + geom_histogram(binwidth = 1) + theme_few()

couples %>% filter(interracial_5cat == "yes") %>% ggplot(aes(x = age_when_met)) + geom_histogram(bins = 20) + theme_few()

couples %>% ggplot(aes(x = time_from_rel_to_cohab)) + geom_histogram() + theme_few()

couples %>% ggplot(aes(x = time_from_met_to_rel)) + geom_histogram() + theme_few()

couples %>% filter(time_from_rel_to_cohab < 3) %>%  ggplot(aes(x = time_from_rel_to_cohab)) + geom_histogram(bins = 30) + theme_few()
#month breakup graph

#same sex couple stuff
#w6_same_sex_couple_gender

gathered_couples %>% filter(w6_same_sex_couple_gender != "hetero couple") %>% count(meeting_type) %>% arrange(desc(n))

gathered_couples %>% filter(w6_same_sex_couple_gender == "lesbian couple") %>% count(meeting_type) %>% arrange(desc(n))

gathered_couples %>% filter(w6_same_sex_couple_gender == "gay male couple") %>% count(meeting_type) %>% arrange(desc(n))

couples %>% filter(w6_same_sex_couple_gender != "hetero couple") %>% ggplot(aes(x = age_when_met)) + geom_histogram(bins = 70) + theme_few()
