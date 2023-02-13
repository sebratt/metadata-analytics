#Load packages
library(rentrez)
library(tidyverse)
library(reshape2)

#1. All the databases available to use from NCBI
entrez_dbs()

#2. Check all the terms that are available in the db e.g. nucleotide. 
entrez_db_searchable('nucleotide')


#3. Load CSV
#The original CSV shows one row as one author, and one column with one GB Accession Number. 

gb7 <- read_csv('GBID_window7.csv')
View(gb7)

#4. Melt down the CSV into a single-column data frame.
gb7 <- melt(gb7, id.vars = 'author')
gb7 <- gb7[3]

#5. Identify Accession Numbers as the search term used to fetch from db, so add "[ACCN]" following every GBID.
#Depends on terms from step 2. 
gb7$value_tag <- sub("$", "[ACCN]", gb7$value) 
View(gb7)


#6. Create a tryCatch to detact warnings and errors. Collect all the irregular GB Accession Numbers for other use.
gbid_e <- list() #error list
gbid_w <- list() #warning list
data <- c() #empty data list

#TryCatch function
genbankrecords <- function(i){
  tryCatch(
    expr = {
      gbid_search <- entrez_search(db = 'nucleotide', term = gb7$value_tag[i])
      record <- entrez_fetch(db = 'nucleotide', id = gbid_search$ids, rettype = 'xml')
      data <<- c(data, record)
    },
    error = function(e){
      message(paste('Caught an error:'))
      gbid_e <<- c(gbid_e, list(gb7$value[i]))
      print(e)
    },
    warning = function(w){
      message('Caught an warning')
      gbid_w <<- c(gbid_w, list(gb7$value[i]))
      print(w)
    }
  )
}

#7. Run the loop to fetch GB records from nucleotide using ACCN
for (i in 1:39500) {
  cat(i, "\n") #iteration count
  flush.console() 
  genbankrecords(i)
}

#8. Save data
cat(data, file = 'fullGBrecord_win7.txt')



#-------------------------------------------------------------------------------
#Repeat similar steps but instead, use the file created above to fetch PMC and PubMed records using PMID
#1. Load data
pb7 <- read_csv('window7_pubmedID-1.csv') #This is the cleaned PMID from Yash
View(pb7)

#2. Add PMID tag
pb7$pubmedID_tag <- sub("$", "[PMID]", pb7$pubmedID)

#3. Create tryCatch
pmid_e <- list()
pmid_w <- list()
data <- c()

pubmedrecords <- function(i){
  tryCatch(
    expr = {
      pmid_search <- entrez_search(db = 'pmc', term = pb7$pubmedID_tag[i])
      record <- entrez_fetch(db = 'pmc', id = pmid_search$ids, rettype = 'xml')
      data <<- c(data, record)
    },
    error = function(e){
      message(paste('Caught an error:'))
      pmid_e <<- c(pmid_e, list(pb7$pubmedID[i]))
      print(e)
    },
    warning = function(w){
      message('Caught an warning')
      pmid_w <<- c(pmid_w, list(pb7$pubmedID[i]))
      print(w)
    }
  )
}


#4. Run the loop
for (i in 1:20){
  cat(i, "\n") #iteration count
  flush.console() 
  pubmedrecords(i)
}
#print(data[1])
cat(data, file = 'PMCrecord_win7_1.txt')


#5. Use the error list from the loop in step 4 to fetch from PubMed datbase.
data <- c()
pubmedrecords2 <- function(i){
  tryCatch(
    expr = {
      pmid_search <- entrez_search(db = 'pubmed', term = pmid_e[i])
      record <- entrez_fetch(db = 'pubmed', id = pmid_search$ids, rettype = 'xml')
      data <<- c(data, record)
    },
    error = function(e){
      message(paste('Caught an error:'))
      print(e)
    },
    warning = function(w){
      message('Caught an warning')
      print(w)
    }
  )
}

for (i in 1:2028){
  cat(i, "\n") #iteration count
  flush.console() 
  pubmedrecords2(i)
}
cat(data, file = 'PubMedrecord_win7_1.txt')


