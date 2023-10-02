
sample = read.csv("newsample2.csv")
author <- data.frame(sample$Author)


#V(g)$degree <- degree(g)

#index <- which(V(g)$name %in% my.list.of.100.authors)

#V(g)$degree[indexX]


### Loop for all years
for (i in 1997:2018)
{
  print(i)
  
  ### Loading rda graph object
  load(file = paste0("pub-network-graph-intersected",i,".rda"))
  
  
  ### New empty data frame
  authormetrics = data.frame(Author=character(100),IDInNetwork = numeric(100),Degree=numeric(100),DegCentr=numeric(100),Betweenness=numeric(100),EigenVector=numeric(100),Closeness=numeric(100),stringsAsFactors = FALSE)
  
  ### Looping over sample authors
  for (j in 1:100){
    print(j)
    author=as.character(sample$Author[j])
    #degree(g, v = V(g)[strg])
    
    ###Handling error to catch cases where author is not present in network
    
    id = tryCatch({as.numeric(V(g)[author])}, error = function(e)
    {id = "NA"})
    
    
    ###Author not present in this network, a.k.a did not publish this year
    ###Hence no metrics available for this author for this year
    if (id == "NA"){
      authormetrics$Author[j]=author
      authormetrics$IDInNetwork[j]=""
      authormetrics$Degree[j]=""
      authormetrics$DegCentr[j]=""
      authormetrics$Betweenness[j]=""
      authormetrics$EigenVector[j]=""
      authormetrics$Closeness[j]=""
      next
    }
    
    ###Metrics for authors in network
    
    ###Degree
    degree= as.numeric(degree(g, v = V(g)[author]))
    #degree= as.numeric(degNet[author])
    
    
    ###Degree Centrality (same as degree at node level??)
    degCen= as.numeric(centr_degree(g,mode="all")$res[id])
    #degCen= as.numeric(degCenNet$res[id])
    
    
    ###Betweenness
    btwn= as.numeric(centr_betw(g)$res[id])
    #btwn= as.numeric(BtwnNet$res[id])
    
    
    #Eigen vector value
    eigenvec= as.numeric(centr_eigen(g)$vector[id])
    #eigenvec= as.numeric(EigenNet$vector[id])
    
    
    ###Closeness
    closeness= as.numeric(centr_clo(g)$res[id])
    #closeness= as.numeric(CloseNet$res[id])
    
    
    ### Adding values to data frame
    authormetrics$Author[j]=author
    authormetrics$IDInNetwork[j]=id
    authormetrics$Degree[j]=degree
    authormetrics$DegCentr[j]=degCen
    authormetrics$Betweenness[j]=btwn
    authormetrics$EigenVector[j]=eigenvec
    authormetrics$Closeness[j]=closeness
    
    
  }
  
  name <- paste("pub-author-metric-sample2-",i, ".csv", sep = "")
  write.csv(authormetrics, name)
} 

