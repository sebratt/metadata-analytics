#install.packages("tidyverse")
#install.packages("dplyr")

library(tidyverse)
library(dplyr)

setwd("C:/Users/ual-laptop/Downloads/GenBank Research/Spring 2023 Research/Data")

GenBankdata <-  read_csv(file = "C:/Users/ual-laptop/Downloads/GenBank Research/Spring 2023 Research/Data/GB_NIH_SUBPROJ_COST_subset.csv")

print(GenBankdata)
summary(GenBankdata)
str(GenBankdata)

org_avg_fund_1992 <- print(GenBankdata %>%
                            filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1992) %>%
                            group_by(ORG_NAME) %>%
                            summarize(AvgFunding = mean(TOTAL_COST_SUB_PROJECT.y)) %>%
                            arrange(desc(AvgFunding)),
                          n = Inf)

org_avg_fund_1993 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1993) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST_SUB_PROJECT.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

summary(GenBankdata$BUDGET_START_YEAR)
str(GenBankdata$BUDGET_START)
summary(GenBankdata$FY.x)
summary(GenBankdata$FY.y)
summary(GenBankdata$year_etc.x)
summary(GenBankdata$year_etc.y)

GenBankdata <- GenBankdata %>%
  mutate(BUDGET_START_YEAR = substr(BUDGET_START, 
                                    nchar(BUDGET_START) - 4 + 1, 
                                    nchar(BUDGET_START)))

GenBankdata$BUDGET_START_YEAR <- as.numeric(GenBankdata$BUDGET_START_YEAR)
print((GenBankdata$BUDGET_START_YEAR))

# Scatter Plot for the year 1992
scatterPlotOrgFunding <- ggplot(data = org_avg_fund_1992,
                                     aes(x = ORG_NAME,
                                         y = AvgFunding))

scatterPlotOrgFunding + geom_point(color = "dark blue") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  ggtitle("Org and Funding Scatter Plot for 1992")

# Scatter Plot for the year 1993
scatterPlotOrgFunding <- ggplot(data = org_avg_fund_1993,
                                aes(x = ORG_NAME,
                                    y = AvgFunding))

scatterPlotOrgFunding + geom_point(color = "dark blue") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  ggtitle("Org and Funding Scatter Plot for 1993")
