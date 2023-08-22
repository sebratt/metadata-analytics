# # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# Funding (w/ "Has PI") Author processing to create an EL # #
#
# Nov 5, 2022 started.
# Nov 7, 2022 resume.
# Nov. 14 resume after got cut off last thurs when computer batter died :/
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
library(dplyr)
library(splitstackshape)
library(tidyverse) # metapackage of all tidyverse packages
library(stringr)

# read in chunk 1 
setwd("C://Users//sebratt//OneDrive - University of Arizona")

d <- read.csv2("refs2_first_NIH_author.csv", sep = ",")
d$X <- NULL


#d <- d[c(130:170, 9928:9950),] #just testing out the " Jr." cSplit issue, which I catch below in gsub 
d$authors<- gsub(" and",",", d$authors) 
d$authors <- gsub(" and ",",", d$authors)
d$authors <- gsub("and ",",", d$authors)
d$authors <- gsub(" Jr","Jr", d$authors) # " Jr." cSplit problem
d$authors <- gsub(" ","", d$authors) # get ride of spaces cuz I saw a \t in the author data. DOes not fix.
#Harwich,M.D. Jr., 

# Rearrange so all vars are before authors
colnames(d)
d.auths <- d[, c(1,8,12,13,3:7,2)]
d.auths[1:3,]

d.auths <- cSplit(d.auths, "authors", ".,",'wide',type.convert="as.character")
#typeof(d.auths$authors_001)
rm(d)

# team size: Count number of  authors (for each pub/sub)
d.auths$teamsize <- rowSums(!is.na(d.auths)) # Create a column containing teamsize by using rowSums to count the number not NA cells, to get the count of authors on a submission 

colnames(d.auths)
d.auths2 <- d.auths[,c(1:12,205,10:204)] # rearrange so all author name cols are to the right to prep for below
d.auths2[,11]
colnames(d.auths2)[3] <- "PI_LastName"

N <- d.auths2
N$iz.na<-is.na(N[,12]) # Look in author_002 column. Is it NA? This tells us how many are single or "solo" authors. 
# If TRUE, author is a single author/solo. 
# FALSE is at least 1 other co-author
table(N$iz.na) # first chunk = 1306

single_authors <- subset(N, iz.na=="TRUE")  # subset df for only single authors

N<-N[N$iz.na=="FALSE",] # subset for only co-authored references
N$iz.na <- NULL
colnames(N)[3] <- "PI_LastName"


# Loop to make edgelist (Auth1 Auth2 pairs)  
# First make a vector for each columns you want to add to the Auth1-Auth2 edgelist (as attributes) 

refid <- d.auths2$refid                    
ref_type <- d.auths2$ref_type                 
lower <- d.auths2$PI_LastName                  
has_author <- d.auths2$has_author               
PI_IDS <- d.auths2$PI_IDS      
FULL_PROJECT_NUM.y <- d.auths2$FULL_PROJECT_NUM.y      
TOTAL_COST.y  <- d.auths2$TOTAL_COST.y          
TOTAL_COST_SUB_PROJECT.y <- d.auths2$TOTAL_COST_SUB_PROJECT.y
APPLICATION_ID   <- d.auths2$APPLICATION_ID
teamsize <- d.auths2$teamsize
  
N.year <- N %>% dplyr :: select(starts_with('authors'))#We want JUST the authors awesome.
colnames(N.year)
  
  tmpN <- t(combn(N.year[1,(N.year[1,])], 2)) #all possible combinations of pairs in first row then transposed.
  tmpN[1,] #the first pair
  df.N <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)  
  df.N <- df.N[complete.cases(df.N), ]
  df.N
  
  df.N$refid <- refid[1]
  df.N$ref_type <- ref_type[1]
  df.N$lower <- lower[1] 
  df.N$has_author <- has_author[1]
  df.N$PI_IDS <- PI_IDS[1]
  df.N$FULL_PROJECT_NUM.y <- FULL_PROJECT_NUM.y[1]
  df.N$TOTAL_COST.y  <- TOTAL_COST.y[1]
  df.N$TOTAL_COST_SUB_PROJECT.y  <- TOTAL_COST_SUB_PROJECT.y[1]
  df.N$APPLICATION_ID <- APPLICATION_ID[1]
  df.N$teamsize  <- teamsize[1]
  
  
  for (my.row in 3:dim(N.year)[1]) 
  {
    #my.row = 2
    #my.row <- 1 + my.row
    print(my.row)
    #print(my.row/nrow(N.year)*100)
    
    tmpN <- t(combn(N.year[my.row,(N.year[my.row,])], 2)) #all possible combinations of pairs in first row then transposed.
    tmp.df <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)  
    tmp.df <-tmp.df[complete.cases(tmp.df), ]
    
    
    tmp.df$refid <- refid[my.row]
    tmp.df$ref_type <- ref_type[my.row]
    tmp.df$lower <- lower[my.row] 
    tmp.df$has_author <- has_author[my.row]
    tmp.df$PI_IDS <- PI_IDS[my.row]
    tmp.df$FULL_PROJECT_NUM.y <- FULL_PROJECT_NUM.y[my.row]
    tmp.df$TOTAL_COST.y  <- TOTAL_COST.y[my.row]
    tmp.df$TOTAL_COST_SUB_PROJECT.y  <- TOTAL_COST_SUB_PROJECT.y[my.row]
    tmp.df$APPLICATION_ID <- APPLICATION_ID[my.row]
    tmp.df$teamsize  <- teamsize[my.row]
    

    
    df.N <- rbind(df.N, tmp.df)
  }

  
  filename <- paste0("chunk_1_EL_1.csv")
  View(df.N)
  write.csv(df.N,filename)
  #}, error=function(e){})
