# Regression work 2019
# Prepare and normalize data for hypothesis testing.

options(scipen = 999)
# The second diameter is actually 'mean distance"
# # we need: diameter, net.mean.dist, net.transitivity, net.centr_degree, net.centr_closeness. 

# see delay: # pats and data subs; pubs too. 
# "big jumP" possiblities: delay 2001?
mfrow = par(c(2,2))



setwd("C:/Users/sarah/Documents/rscripts/network1992_2018loop/patent/")

fname.regression.vars <- "master_patent_networkmetrics.csv"
setwd("C:/Users/sarah/Documents/rscripts/network1992_2018loop/datasubmissions/")
sub.regression <- "master_submission_networkmetrics.csv"
setwd("C:/Users/sarah/Documents/rscripts/network1992_2018loop/publication/")
pub.regression <- "master_publication_networkmetrics.csv"

file.choose() 
sub.regression.fname <- "C:\\Users\\sarah\\Documents\\Rscripts\\network1992_2018loop\\datasubmissions\\master_datasubmissions.csv"
vars <- read.csv(fname.regression.vars, quote="\"", header = TRUE, sep=",", stringsAsFactors=F)

setwd("C:/Users/sarah/Documents/rscripts/network1992_2018loop/patent/")
vars <- read.csv("master_patent_networkmetrics.csv")
setwd("C:/Users/sarah/Documents/rscripts/network1992_2018loop/datasubmissions/")
vars2 <- read.csv("master_datasubmissions.csv")

View(vars2)
colnames(vars)
colnames(vars)[2] <- "pat.num"

my.cols <- c("pat.num", "portion_coauthorship", "teamsize_median","teamsize_mean",  "nodeNum","edgeNum", "density",
             "assortativity", "cluster.coeffient")
             
master_datasubmissions.csv
             
myyyy.cols <- c("pat.num", "pat.auth.median", "sub.net.vcount", "sub.net.ecount"
             , "sub.net.density", "sub.net.diameter", "sub.net.mean.dist"
             , "sub.net.transitivity", "sub.net.centr_degree", "sub.net.centr_closeness")

# The networks to measures might describe the network better; detect the network properties in a more accurate way. 

library(PerformanceAnalytics)
chart.Correlation(vars[ , my.cols])

dummy <- rep(1, dim(vars)[1])
dummy[1:14] <- 0

plot(vars$year, vars$nrow, type = "n", main="patents")
text(vars$year, vars$nrow, labels = 1:length(vars$nrow))
abline(v = vars$year[9], col = "red", lty = 3)

plot(vars2$ï..year, vars2$nrow, type ="n", main ="data submissions")
text(vars2$ï..year, vars2$nrow, labels = 1:length(vars2$nrow))
abline(v = vars$year[9], col = "red", lty = 3)

plot(vars$cluster.coeffient[-zap])
plot(vars$pat.num[-21], vars$nodeNum[-21])
plot(vars$pat.num, vars$nodeNum)

zap <- c(1,2,3,21)

plot(log10(vars$pat.num[-zap]), log10(vars$density[-zap]))

plot(log10(vars$density), type = "l")
plot(log10(vars$pat.num[-zap]), log10(vars$cluster.coeffient[-zap]))

plot(log10(vars$pat.num[-zap]))
plot(log10(vars$pat.num[-zap]), log(vars$cluster.coeffient[-zap]))

fit <- lm(log10(vars$pat.num[-zap]) ~ vars$nodeNum[-zap])  + vars$mean_auths[-zap]
          # + vars$pat.auth.mean[-zap]
         
          # + log10(vars$sub.net.density[-zap])
          #+ vars$sub.net.transitivity[-zap]
          + log10(vars$sub.net.centr_closeness[-zap])
          #+ vars$sub.net.mean.dist[-zap]
            #vars$sub.net.diameter[-zap]
          #+ dummy[-zap]
          # + vars$sub.net.mean.dist
          #+ vars$pat.auth.mean[-21]
          )
fit
summary(fit)
par(mfrow =c(1,2))
plot(fit)



library(faraway)
vif(fit)

#par(mfrow =c(1,1))
#plot(fit$residuals)


hist(vars$pat.num)
qqnorm(vars$pat.num)
qqnorm(sqrt(vars$pat.num))
qqnorm(log(vars$pat.num))

qqnorm(fit$residuals)
qqline(fit$residuals)
# The null-hypothesis of this test is that the population is normally distributed.
shapiro.test(sqrt(vars$pat.num))

library(MASS)
fit <- rlm(vars$pat.num ~ vars$nodeNum)
summary(fit)

# residuals:
# QQ plot: 

# Model 1.1 and 1.2 - predict patent productivity with sub network
# y = # of patents 
# x = median (try mean in a different model) team size, density, closeness (clustering coefficient), betweeness.



# Model 2.1 and 2.2 - predict patents with pub network
# y = # of patents 
# x = median (try mean in a different model) team size, density, closeness (clustering coefficient)


# Model 3.1 and 3.2 - predicts publication productivity with subs network
# y = # of publications
# x = median /mean team size, density, closeness (clustering coefficient)



