# Research questions for the geographic ICSSI paper 
# What is the prevalence of international collaboration on datasets 
# between scientists from countries with different income status and research capacity? 
library(readxl)

# Read in the World Bank Classification (all years) <- Mrudang
og_hist <- read_xlsx("OG_HIST.xlxs")
# Read in Sci_Tech_capacity classification 

sci_tech <- read.csv2("sci_tech_capacity.csv", sep = ",")

# this is a file
getwd()
