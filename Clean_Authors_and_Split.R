# # # # # # # # # # # # # # # # # # # 
# Clean the author column 
# Also has code for detecting if the PI is in the author list 
# Feb 2023

library(splitstackshape)
library(dplyr)

d <- read.csv() # read organization file

d$authors<- gsub(" and",",", d$authors) 
d$authors <- gsub(" and ",",", d$authors)
d$authors <- gsub("and ",",", d$authors)

# team size sub: count number of  authors (for each pub/sub)

d <- cSplit(d, "authors", ".,",'wide') # can also split "long" to do a vertical split 


# This is "extra" but you can check it out if you'd like, Mrudang!
# For the organization data, you may have to rearrange the columnsso authors are at the end. 
# Then can make a column that counts how many authors are on the publication
d$teamsize <- (rowSums(!is.na(d))-3) # the -3 is for the number of columns before the author cols

# This code is a starting place to see if the PI is in the author list
#index <- c(4:(ncol(d)-5))
j <- ncol(d)
author_cols <- colnames(d[c(4:j)])
author_cols <- author_cols[4:j]


e <- d %>% filter_at(vars(all_of(author_cols)
                          
), any_vars(. %in% d$Author)) 



d$type <- "publication" #pubs
d[grep('Submitted',d$journal),]$type<-'submission' # datasets
d[grep('Unpublished',d$journal),]$type<-'Unpublished'
d[grep('In press',d$journal),]$type<-'In press'


dat <- subset(d, d$type == "submission")
table(dat$type)

e<-d 
# I have to find the code that makes the "type" columns. But! Until then 

f <- e[which(e$type == "submission"),]
data_auths_IDs <- f$Author
d$has.submission <- ifelse(d$Author %in% data_auths_IDs, yes = "1", no="0")
l <- e[which(e$type == "Unpublished"),]
unpub_auths_IDs <- l$Author
d$has.Unpublished <- ifelse(d$Author %in% unpub_auths_IDs, yes = "1", no="0")
j <- e[which(e$type == "patent"),]
pat_auths_IDs <- j$Author
d$has.pat <- ifelse(d$Author %in% pat_auths_IDs, yes = "1", no="0")

z <- e[which(e$type == "publication")] 
pub_auths_IDs <- z$Author
d$has.pub <- ifelse(d$Author %in% pub_auths_IDs, yes = "1", no="0")

q <- e[which(e$type == "In press")]
inpress_auths_IDs <- q$Author
d$has.inpress <- ifelse(d$Author %in% inpress_auths_IDs, yes = "1", no="0")


#table(d$has.submission)

g <- d[, c("ID_1", "Author", "type", "has.submission", "has.pub", "has.Unpublished", "has.inpress", "has.pat", "year")]
g2 <- datknow[, c("ID_1", "Author", "authors", "pubmed")]
m <- merge(g, g2, by=c("ID_1", "Author"), allow.cartesian = TRUE)


filename <- paste0("has.submission_all_take4",i,".csv")
setwd("C:/Users/sarah/Dropbox/GenBank2016/Statistical properties/D2K1997-2017/has.submission/take4/")
write.csv(m, filename)