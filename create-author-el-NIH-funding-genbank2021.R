# # # # # # # # # # # # # # # # 
#
# Create author-author edgelists w/ attributes for authors in GenBank w/ pmid 
# For years: 2019, 2020, 2021 as a sample
# 3 csvs for direct submission.
# 3 csvs for publication.
# 
# Created: Sept. 29 2022, Sarah Bratt
# Last Draft: Oct 4, 2022
# Oct 18, 2022
#
# # # # # # # # # # # # # # # # 

setwd("C:/Users/sebratt/Downloads/")

#install.packages("splitstackshape") # parse out names with cSplit function.
#install.packages("igraph") # network object transformation and statistcs.
#install.packages("stringdist")
install.packages("lsa")
library(lsa)
library(splitstackshape) # load splitstackshape for manipulation and cleaning
library(igraph) # load igraph for network functions
library(stringdist)
library(stringr)

################## Load data, the GenBank and NIH data with no NAs in the subprojects cost column. Other df is core-proj not NA 
dat <- read.csv("C:/Users/sebratt/Downloads/CORE-projects-NIH-lnk/GB_NIH_SUBPROJ_COST_subset.csv")
colnames(dat)

#seems to be lots of repetition.Based on PMID? CORE_Project?
# Looks like based on PI. It is de-normalized by PI. 

# Fuzzy matching, maybe. Does last name appear in author.y list? 
# first csplit on PI so last and first name are isolated. 

PI_dat <- dat[, c(6,8,9,11,21,24, 31, 38,45,50)]
  
PI_dat2 <- cSplit(PI_dat, "PI_NAMEs", sep = ",", direction = "wide")




df.auths <- PI_dat2[, c(1,5)]

PI_dat2$lower <- str_to_sentence(PI_dat2$PI_NAMEs_1)
  
PI_dat2$author_contains <- grepl(PI_dat2$lower, PI_dat2$authors.x)

for(i in 1:nrow(PI_dat2))
  {
  #i = 607681
  
  PI_dat2$has_author[i] <-grepl(PI_dat2$lower[i], PI_dat2$authors.x[i])
  print(i)

  }


# lapply? sapply 


# What could explain PI being on the list of authors? size of team, 
# prestige of PI, IC+_Name, Org, date, journal, type of ref *dataset vs pub.
# Can we preduct it, or is it just random? Analysis of variance between groups?

PI_dat2$TF <- grepl(PI_dat2$lower[1], PI_dat2$authors.x)
distinct <- PI_dat2[!duplicated(PI_dat2$PROJECT_NUMBER),]
table(distinct$has_author)  


# read in cost subset
setwd("C:/Users/sebratt/Downloads/")
total_cost <- read.csv("GB_NIH_TOTAL_COST_subset.csv")
distinct_totalcost <- total_cost[!duplicated(total_cost$PROJECT_NUMBER),]

distinct_totalcost <- cSplit(distinct_totalcost, "PI_NAMEs", sep = ",", direction = "wide")
distinct_totalcost$lower <- str_to_sentence(distinct_totalcost$PI_NAMEs_1)

for(i in 1:nrow(distinct_totalcost))
{
  #i = 5
  
  distinct_totalcost$has_author[i] <-grepl(distinct_totalcost$lower[i], distinct_totalcost$authors.x[i])
  print(i)
  
}



PI_dat3 <- dat[!duplicated(dat$PROJECT_NUMBER),]
PI_dat3 <- cSplit(PI_dat3, "PI_NAMEs", sep = ",", direction = "wide")  
PI_dat3$lower <- str_to_sentence(PI_dat3$PI_NAMEs_1)

for(i in 1:nrow(PI_dat3))
{
  #i = 607681
  
  PI_dat3$has_author[i] <-grepl(PI_dat3$lower[i], PI_dat3$authors.x[i])
  print(i)
  
}


table(distinct_totalcost$has_author)
table(distinct_totalcost$title.y)

distinct_totalcost$PI_NAMEs_3 <- NULL
cost <- rbind(distinct_totalcost, PI_dat3)

write.csv(cost, "cost_name_match.csv")


dim(PI_dat3)
dim(distinct_totalcost)

b <- as.data.frame(table(cost$ED_INST_TYPE))
plot(b$Var1, b$Freq)


aggregate_n <- aggregate(cost$TOTAL_COST.y, list(cost$ED_INST_TYPE), FUN = sum)
aggregate_y <- aggregate(cost$TOTAL_COST_SUB_PROJECT.y, list(cost$ED_INST_TYPE), FUN = sum)


f <- cost$TOTAL_COST_SUB_PROJECT.y

# Time to make an edglist of authors. Include the PI. 
cost

df.auths$authors.x<- gsub(" and",",", df.auths$authors.x) 
df.auths$authors.x <- gsub(" and ",",", df.auths$authors.x)
df.auths$authors.x <- gsub("and ",",", df.auths$authors.x)

df.auths2<- cSplit(df.auths, "authors.x", ".,",'long') # split t



# Then fuzzy match authors with PI_names_1 and authors.x
# using cosine similarity
# amatch? from stringdist
#https://rdrr.io/cran/stringdist/man/amatch.html 

Name.1 <- PI_dat2$authors.x

Name.2 <- PI_dat2$PI_NAMEs_1

df1 <- data.frame(Name.1, Name.2)
df1$similar <- stringsim(Name.1, Name.2)
df1 <- data.frame(Name.1, Name.2)
df1$similar <- stringsim(Name.1, Name.2)


PI_dat2$cosine_score <- amatch(PI_dat2$authors.x, PI_dat2$PI_NAMEs_1)

cosine(t(as.matrix(PI_dat2)))



x <- c(33, 33, 43, 55, 48, 37, 43, 24)
y <- c(37, 38, 42, 46, 46, 59, 41, 50)

cosine(x,y)

pres <- c("Bill Clinton", "Barack Obama")

pres_df <- data.frame(President = c("Joseph R. Biden, Jr", "Donald J. Trump", "Barack H. Obama", "George W. Bush", "William J. Clinton"),
                      Vice_President = c("Kamala D. Harris", "Michael R. Pence", "Joseph R. Biden", "Dick B. Cheney", "Albert A. Gore, Jr."))

    








                 

df.auths<- dat[, c("authors.x", "title.x", "TOTAL_COST_SUB_PROJECT.y", "")] # select columns - consider attributes to add to EL for ASIST22 collab-capacity Hackathon 




# parse out the "and" and substitute for the three variants in formatting for the author names. 
df.auths$authors<- gsub(" and",",", df.auths$authors) 
df.auths$authors <- gsub(" and ",",", df.auths$authors)
df.auths$authors <- gsub("and ",",", df.auths$authors)

df.auths<- cSplit(df.auths, "authors", ".,",'wide') # split the author cell into columns (horizontally), where comma is the delimiter.

nrow <- dim(df.auths)[1] # count the number of rows: this is the total number of submissions for that year.
ncol <- dim(df.auths)[2] 
df.auths$teamsize <- rowSums(!is.na(df.auths)) # Create a column containing teamsize by using rowSums to count the number not NA cells, to get the count of authors on a submission.

teamsize_summary <- summary(df.auths$teamsize)
min_teamsize <- teamsize_summary[1]
teamsize_1stquartile <- teamsize_summary[2]
teamsize_median <- teamsize_summary[3]
teamsize_mean <- teamsize_summary[4]
teamsize_3rdquartile <- teamsize_summary[5]
teamsize_Max <- teamsize_summary[6]

N <- data.frame(df.auths)
N <- N[ -c(1:8) ]     # Remove columns that are not authors so we can just deal with author names. 
colnames(N) # should only be "authors_..."

N$iz.na <- is.na(N[,2]) # If the second column has NA's, this indicates solo-authorshup.
false_aka_coAUTH <- table(N$iz.na)[1] # how many are multiple authored datasets? 
true_aka_SOLO <- table(N$iz.na)[2] # how many are single-authored datasets?
paste0("There are ", true_aka_SOLO, " single-authored records (datasets or submissions) and ",
       false_aka_coAUTH, " more-than-one authored records.")

N<-N[!N$iz.na,] # Remove the rows with NA so we just have multi-authored records
N$iz.na <- NULL # Remove the NA column.
N$teamsize <- NULL # Remove the teamsize column. 
portion_coauthorship <- 100*(false_aka_coAUTH/nrow)
portion_soloauthorship <- 100*(true_aka_SOLO/nrow)
paste0(round(portion_coauthorship, 2),"% (percent) of records are co-authored. ",
       round(portion_soloauthorship,2), "% (percent) are single-authored records.")


for (my.row in 1:dim(N)[1]) 
{
  # my.row <- 1 + my.row
  # print(my.row)
  tmpN <- t(combn(N[my.row,!is.na(N[my.row,])], 2))
  tmp.df <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)
  
  colnames(tmp.df) <- c("Auth1", "Auth2")
  
  # tmp.df$ref.years<- ref.years[my.row] # Add if you want to give pair attributes, such as the year.
  # tmp.df$ref.type <- ref.type[my.row]  # Add if you want to give pair attributes, such as the reference type.
  
  df.N <- rbind(df.N, tmp.df)
  gc() # for every var written and overwritten, R 'holds onto the memory'. Allows R to recalim memory. Will keep vars in global environment. 
  
}

# Write the Linkedlist of the submission network for that year to a .csv file. 
# no spaces = paste0.
i = 2019
linkedlist <- paste("submission-linkedlist-genbank2021-", i, ".csv", sep = "")
write.csv(df.N, linkedlist)


######################################## End Cleaning and Formatting of data ############################################

