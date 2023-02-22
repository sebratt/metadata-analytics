# Testing for Git 
#All authors 
# Can we use Jun's semantic scholar disambiguated authors approach to ascertain 
# the authors are disambiguated? 

# Steps would be:
# 1. Find the GenBank IDs for all of Joy's / Jian's authors. 
# 2. Search Jun's file 'genbank_semanticscholar_publication_author_with...csv'
# 3. Merge subset of matches with "genbank_published_author_info_202108.csv'
# 4. Identify names to confirm author id and name are reconciled.
# 5. Add the author ID to the spreadsheet "authors for lookup" (n=400)
library(readr)
install.packages("splitstackshape")
library(splitstackshape)
library(readxl)
<<<<<<< HEAD
ghghg
library(whatever)
=======
>>>>>>> 18b623b591ca664ad3a0350d9d8cbc90f0858387

Author_tracking <- read_xlsx("Author_tracking_QLiu.xlsx") # this is the file 


ss_genbank_authors <- read_csv("genbank2021/csv_name_disambiguation/genbank_semanticscholar_publication_author_with_genbank_reference_ids.csv")

# Match Joy GenBank IDs to SS GenBank IDS 
ids_Joy <- Author_tracking[,5]
colnames(ids_Joy)[1] <- "Genbank_ids"
colnames(ss_genbank_authors)[3] <-  "Genbank_ids"

ids_Joy2 <- cSplit(ids_Joy, "Genbank_ids", sep = ",", direction = "long")

result <- merge(x = ss_genbank_authors, y= ids_Joy2, by ="Genbank_ids", all.y = TRUE)

#ids_Joy3 <- cSplit(ids_Joy2, "Genbank_ids", sep = "1A", direction = "wide") #didn't work because replaces comma, removing important info 

#not <- table(is.na(result$ssid)) #2119 T = 1675
#not3 <- table(is.na(result2$ssid)) #2119 T = 1675

# Need to fix by getting R to programmatically  you know how to get R to programmatically "Find "1A" or "2A" or "3A" or "1B" or "2B" or "3B" patterns and insert a comma before it?

author_name_jun <- read_csv("genbank2021/csv_name_disambiguation/genbank_published_author_info_202108.csv")
View(genbank_published_author_info_202108)

result2 <- cSplit(result, "author_id_list", sep = " ", direction = "long")
colnames(result2)[3] <-  "author_id"

result3 <- merge(x = result2, y= author_name_jun, by ="author_id", all.x = TRUE)

# Check ambiguous authors: is the problem solved with Jun's SS approach? 
# example:  Fujiwara T. (a name in Joy's list that has >5 papers per year)

fujiwara_ids <- c('AB025228', 'AF219994', 'AB015223', 'AB024763', 'AF219990', 'AB032920', 'AB045381, 
                  AB045382', 'AJ299430', 'AJ299431', 'AF233369', 'AB049211', 'AJ278286', 'AB060300', 
                  'AJ277440', 'AY043290', 'AB033823', 'AB064943', 'AY043484', 'AB073713')
colnames(fujiwara_ids)[1] <- "match_ids_GB"

result3 <- cSplit(result3, "Genbank_ids", sep='_1', direction = 'wide')
colnames(result3)[10] <- "match_ids_GB"
colnames(fujiwara_ids)[1] <- "match_ids_GB"
m <- merge(x = result3, y = fujiwara_ids, by = "match_ids_GB")

write.csv(fujiwara, "fujiwara_disambig.csv")




