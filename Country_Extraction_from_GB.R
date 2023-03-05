# Country tables
# Updated 3/4/2023 
#install.packages("maps")
library(maps)
library(stringr)
#install.packages("sf")
library(sf)
#install.packages("mapview")
library(mapview)
library(ggmap)
library(igraph)
library(splitstackshape)
library(dplyr)
library(tidyverse)

#setwd("E:/GenBank_2020Data/Taxonomy_Network_Analysis/HIV/HIV_virus_el")
setwd()

i = 2000
file <- paste0("HIV_virus_el_",i,".csv")
hiv_el <- read.csv(file) 
hiv_el$X <- NULL



#hiv_journal_dedup <- hiv_el[!duplicated(hiv_el$journal), ] # do this for journal counting. Prevent inflation b/c of names in EL 
all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")
hiv_el$country <- sapply(str_extract_all(hiv_el$journal, all_countries), toString)

myvars <- c("country.etc", "lat", "long")
country.lat.long <- world.cities[myvars] 
colnames(country.lat.long)[1] <- "country"

cntry <- country.lat.long[!duplicated(country.lat.long$country), ]
hiv_el_country <- merge(x = hiv_el, y = cntry, by = 'country', all.x = TRUE) 

hiv_el_country.file <- paste0("E:/GenBank_2020Data/Taxonomy_Network_Analysis/HIV/HIV_virus_country_el/HIV_virus_country_el_",i,".csv")
write.csv(hiv_el_country, hiv_el_country.file, row.names = FALSE)