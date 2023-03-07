# Disambiguation network test
# A couple of cases for specific author disambiguation 
# Sarah B. 
# 03-07-2023

# load libraries
library(splitstackshape)

# 1. Format author IDs as edgelist (EL); do the same for author names to set up comparison
# 2. Plot network of author names (2002)
# 3. Plot network of author IDs (2002)

setwd("/home/u12/sebratt/genbank2021/")

# Read data from Jun's disambiguation
# First is the author ID and affiliation from semantic scholar
# Second is the link to GenBank IDs. We can treat these like "author names"

author_name_jun <- read.csv2("csv_name_disambiguation/genbank_published_author_info_202108.csv", sep=",")
View(head(author_name_jun))

name_table <- table(author_name_jun$name) # 675668 unique rows (names) 
name_table <- as.data.frame(name_table)

authorid_table <- table(author_name_jun$author_id) # 734960 unique rows (author ids)
authorid_table <- as.data.frame(authorid_table)

# how many more are there when we disambig?
734960 - 675668
675668/734960 
# = 59,292
# Quantifying that, we could say there are 10% more unique authors once disambiguated than we originally thought. 
# Example; J. Wang we thought was one person, but J. Wang is actually 5 separate people. Which is why we have more authors now.


ss_genbank_authors <- read_csv("csv_name_disambiguation/genbank_semanticscholar_publication_author_with_genbank_reference_ids.csv")
View(head(ss_genbank_authors))

# Subset for 2002. Only ss_genbank_authors has year so. 
subset2022 <- ss_genbank_authors[which(ss_genbank_authors$reference_year_list=='2002'), ]


# format author IDs as edgelist (EL) w/ space delimiter
df.auths <- cSplit(subset2022, "author_id_list", " ",'wide')

N <- data.frame(df.auths)
N$iz.na <- is.na(N[,7]) # If the second author column has NA, this indicates solo-authorshup.
false_aka_coAUTH <- table(N$iz.na)[1]
true_aka_SOLO <- table(N$iz.na)[2]


# Artifact of the data submission process, i.e., submitter only puts own name, even if produced a pub with a team, or truly reflective of the process co-authorship?
# The solo authors may or may not be connected to the giant component. For now, we are excluding them from analysis because data formatting code 
# gives issues. They will likely be engaged in other collaborations, however, so we shouldadd them somehow. Ask Jeff.


N<-N[!N$iz.na,] # Remove the rows with 
N$iz.na <- NULL # Remove the NA column

N <- N[6:ncol(N)]

tmpN <- t(combn(N[1,!is.na(N[1,])], 2)) #all possible combinations of pairs in first row then transposed.

tmpN[1,] #the first pair
df.N <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)  # Order doesn't matter. 
# This is not a permutation; (I checked) there are no "reverse" pairings. 
# i.e. Honda and Yakayama only appear as "H, Y" not "Y, H". 

# Loop for all rows to transpose the authors into a format for iGraph analysis. 

for (my.row in 2:dim(N)[1]) 
{
  # my.row <- 1 + my.row
  # print(my.row)
  tmpN <- t(combn(N[my.row,!is.na(N[my.row,])], 2))
  tmp.df <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)
  
  colnames(tmp.df) <- c("Auth1", "Auth2")
  
  # tmp.df$ref.years<- ref.years[my.row] # Add if you want to give pair attributes, such as the year.
  # tmp.df$ref.type <- ref.type[my.row]  # Add if you want to give pair attributes, such as the reference type.
  
  df.N <- rbind(df.N, tmp.df)
  #gc() # for every var written and overwritten, R 'holds onto the memory'. Allows R to recalim memory. Will keep vars in global environment. 
  
}


linkedlist <- paste("verify-disambig-linkedlist-2002.csv", sep = "")
write.csv(df.N, linkedlist)



##################################### Visualize Network 2002 for disambig verification ################


install.packages("igraph")
library(igraph)
g <- graph.data.frame(df.N, directed=FALSE)  

#E(g)$weight <- 1
#g <- simplify(g, edge.attr.comb="sum")


V(g)$size <- 1
#E(g)$width <- E(g)$weight
V(g)$label <- ""
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0, main="")


table(E(g)$ref.type)
E(g)$color <- "red"
E(g)$color[E(g)$ref.type == "publication"] <- "blue"
E(g)$color[E(g)$ref.type == "submission"] <- "green"


# this next like will take a longer and longer time on biger and bigger networks.
# don't run if you don't have time.
#l <- layout.fruchterman.reingold(g) 
#l <- layout.kamada.kawai(g)

V(g)$x <- l[,1]
V(g)$y <- l[,2]


plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0, main="")

V(g)$color <- "brown"
V(g)$name
match()









