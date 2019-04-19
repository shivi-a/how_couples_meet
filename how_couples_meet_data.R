
# Script that loads the data from a .dta file and then reads it into an RDS
# object, for manipulation by the Shiny App

library(tidyverse)
library(readstata13)
library(fs)

# Read in data from STATA dta file

couples <- read.dta13("HCMST 2017 fresh sample for public sharing draft v1.1.dta")

# Create temp file to write the R data structure to

couples_file <- tempfile()

# Write the R data structure to the temporary file

write_rds(couples, "how_couples_meet/couples_file.rds")