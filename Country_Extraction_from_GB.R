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


all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")
hiv_el$country <- sapply(str_extract_all(hiv_el$journal, all_countries), toString)

myvars <- c("country.etc", "lat", "long")
country.lat.long <- world.cities[myvars] 
colnames(country.lat.long)[1] <- "country"

cntry <- country.lat.long[!duplicated(country.lat.long$country), ]
hiv_el_country <- merge(x = hiv_el, y = cntry, by = 'country', all.x = TRUE) 