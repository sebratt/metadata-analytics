 
REF_w_year$type <- "publication" #pubs
REF_w_year[grep('Submitted',REF_w_year$journal),]$type<-'submission' # datasets
REF_w_year[grep('Unpublished',REF_w_year$journal),]$type<-'Unpublished'
REF_w_year[grep('In press',REF_w_year$journal),]$type<-'In press'



dat <- subset(REF_w_year, REF_w_year$type == "submission")
table(dat$type)

