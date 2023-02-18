# All authors 
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

Author_tracking <- read_xlsx("Author_tracking_QLiu.xlsx")


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
