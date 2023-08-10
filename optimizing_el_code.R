# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Code to reduce the time to process final_collaboration_df 
# Sarah Bratt & Abhishek Nanoti
# Date created: 08-10-2023
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
setwd("/groups/sebratt/ananoti")
library(tidyverse)

final_collaboration_df <- read.csv("final_collaboration.csv")
#EL1992 <- final_collaboration_df[which(final_collaboration_df$year_etc.x =='1992'),] 


authorInfo <- final_collaboration_df %>% select(c(refid, authors.x, year_etc.x,author_id,name))


authorInfo_grouped_test_1 <- authorInfo %>%
  group_by(refid, year_etc.x) %>%
  summarise(author_id = paste(author_id, collapse = ", "),
            name = paste(name, collapse = ", "))


Authors <- authorInfo_grouped_test_1 %>% select(author_id, year_etc.x)

B <- Authors[Authors$year_etc.x == 1992, ]
table(B$year_etc.x)
B <- B %>% select(author_id)
#B <- B[which(!is.na(B),]
B <- cSplit(B, "author_id", ".,",'wide')

#B$author_id_001 <- as.factor(B$author_id_001)
#summary(B$author_id_001)

B[] <- lapply(B, as.character)

#summary_B <- as.data.frame(summary(B))

Nas_in_df <- as.data.frame(sapply(B, function(x) sum(is.na(x))))

nrowB <- dim(B)[1] 
ncolB <- dim(B)[2] 
B$teamsize <- rowSums(!is.na(B))


B <- B %>% filter(teamsize < 15) # filter B for team size less than 15

# 77 rows removed. 3662-3585

#rows_with_author_id825 <- B[!is.na(B$author_id_825), ]

teamsize_summary <- summary(B$teamsize)
min_teamsize <- teamsize_summary[1]
teamsize_1stquartile <- teamsize_summary[2]
teamsize_median <- teamsize_summary[3]
teamsize_mean <- teamsize_summary[4]
teamsize_3rdquartile <- teamsize_summary[5]
teamsize_Max <- teamsize_summary[6]

#barplot(teamsize_summary)

#B_greatthan5 <- B %>%
#  filter(teamsize > 13) %>%
#  pull(refid)



# Remove cols so we just have Author columns   
B$teamsize <- NULL
B <- B[,1:16]

B$iz.na <- is.na(B[,3]) # this is line that calculates single authors.The logic is that if there is an NA is the author_oo2 column, it is a single author. 
false_aka_coAUTH_B <- table(B$iz.na)[1]
true_aka_SOLO_B <- table(B$iz.na)[2]
paste0("There are ", true_aka_SOLO_B, " single-authored records (datasets or submissions) and ",
       false_aka_coAUTH_B, " more-than-one authored records.")



B<-B[!B$iz.na,] # this is line that removes single authors.
B$iz.na <- NULL
B$teamsize <- NULL 
portion_coauthorship_B <- 100*(false_aka_coAUTH_B/nrowB)
portion_soloauthorship_B <- 100*(true_aka_SOLO_B/nrowB)
paste0(round(portion_coauthorship_B, 2),"% (percent) of records are co-authored. ",
       round(portion_soloauthorship_B,2), "% (percent) are single-authored records.")



df.B <- data.frame(Auth1 = character(), Auth2 = character(), stringsAsFactors = FALSE)


# Loop through each row of B
for (my.row in 1:dim(B)[1]) {
  if (sum(!is.na(B[my.row, ])) < 2) {
    next
  }
  
  # Extract author_id columns (exclude the first column which is refid)
  author_id_cols <- B[my.row, -1]
  
  # Convert author_id columns to character type
  author_id_cols <- as.character(author_id_cols)
  
  print(my.row)
  # Create pairs of non-missing author_id values
  tmpB <- t(combn(author_id_cols[!is.na(author_id_cols)], 2))
  
  # Create a data frame from the pairs
  tmp.df_B <- data.frame(Auth1 = unlist(tmpB[, 1]), Auth2 = unlist(tmpB[, 2]), stringsAsFactors = FALSE)
  
  # Append to df.B
  df.B <- rbind(df.B, tmp.df_B)
  
  gc() # for every var written and overwritten, R 'holds onto the memory'. Allows R to reclaim memory. Will keep vars in the global environment. 
}

