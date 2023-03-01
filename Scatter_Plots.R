#install.packages("tidyverse")
#install.packages("dplyr")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

setwd("C:/Users/ual-laptop/Downloads/GenBank Research/Spring 2023 Research/Data")

GenBankdata <-  read_csv(file = "C:/Users/ual-laptop/Downloads/GenBank Research/Spring 2023 Research/Data/GB_NIH_TOTAL_COST_subset.csv")

print(GenBankdata)
summary(GenBankdata)
str(GenBankdata)

org_avg_fund_1992 <- print(GenBankdata %>%
                            filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1992) %>%
                            group_by(ORG_NAME) %>%
                            summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                            arrange(desc(AvgFunding)),
                          n = Inf)

org_avg_fund_1993 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1993) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_1994 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1994) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_1995 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1995) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_1996 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1996) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_1997 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1997) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_1998 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1998) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_1999 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 1999) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2000 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2000) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2001 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2001) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2002 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2002) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2003 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2003) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2004 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2004) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2005 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2005) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2006 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2006) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2007 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2007) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2008 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2008) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2009 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2009) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2010 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2010) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2011 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2011) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2012 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2012) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2013 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2013) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2014 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2014) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2015 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2015) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2016 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2016) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2017 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2017) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2018 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2018) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2019 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2019) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2020 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2020) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)

org_avg_fund_2021 <- print(GenBankdata %>%
                             filter(is.na(ORG_NAME) == FALSE, year_etc.y == 2021) %>%
                             group_by(ORG_NAME) %>%
                             summarize(AvgFunding = mean(TOTAL_COST.y)) %>%
                             arrange(desc(AvgFunding)),
                           n = Inf)


# create a vector of years
years <- c(1992:2021)

# create a list of vectors for each column
mean <- c(0)
median <- c(0)
min <- c(0)
max <- c(0)

# combine the vectors into a tibble
data <- tibble(years, mean, median, min, max)

data$years <- as.numeric(data$years)

for (i in 1:length(years)) {
  
  data[i, 2] <- GenBankdata %>%
    filter(!is.na(ORG_NAME), year_etc.y == data[[i, 1]]) %>%
    summarize(mean = mean(TOTAL_COST.y)) %>%
    pull(mean)
  
  data[i, 3] <- GenBankdata %>%
    filter(!is.na(ORG_NAME), year_etc.y == data[[i, 1]]) %>%
    summarize(median = median(TOTAL_COST.y)) %>%
    pull(median)
  
  data[i, 4] <- GenBankdata %>%
    filter(!is.na(ORG_NAME), year_etc.y == data[[i, 1]]) %>%
    summarize(min = min(TOTAL_COST.y)) %>%
    pull(min)
  
  data[i, 5] <- GenBankdata %>%
    filter(!is.na(ORG_NAME), year_etc.y == data[[i, 1]]) %>%
    summarize(max = max(TOTAL_COST.y)) %>%
    pull(max)
  
}

write.csv(data, "Summary_statistics_org_data.csv", row.names = TRUE)

# Scatter Plot for the year 1992
scatterPlotOrgFunding1992 <- ggplot(data = org_avg_fund_1992,
                                     aes(x = ORG_NAME,
                                         y = AvgFunding))

scatterPlotOrgFunding1992 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (1992)")

# Scatter Plot for the year 1993
scatterPlotOrgFunding1993 <- ggplot(data = org_avg_fund_1993,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding1993 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (1993)")

# Scatter Plot for the year 1994
scatterPlotOrgFunding1994 <- ggplot(data = org_avg_fund_1994,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding1994 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (1994)")

# Scatter Plot for the year 1995
scatterPlotOrgFunding1995 <- ggplot(data = org_avg_fund_1995,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding1995 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (1995)")

# Scatter Plot for the year 1996
scatterPlotOrgFunding1996 <- ggplot(data = org_avg_fund_1996,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding1996 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (1996)")

# Scatter Plot for the year 1997
scatterPlotOrgFunding1997 <- ggplot(data = org_avg_fund_1997,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding1997 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (1997)")

# Scatter Plot for the year 1998
scatterPlotOrgFunding1998 <- ggplot(data = org_avg_fund_1998,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding1998 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (1998)")

# Scatter Plot for the year 1999
scatterPlotOrgFunding1999 <- ggplot(data = org_avg_fund_1999,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding1999 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (1999)")

# Scatter Plot for the year 2000
scatterPlotOrgFunding2000 <- ggplot(data = org_avg_fund_2000,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2000 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2000)")

# Scatter Plot for the year 2001
scatterPlotOrgFunding2001 <- ggplot(data = org_avg_fund_2001,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2001 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2001)")

# Scatter Plot for the year 2002
scatterPlotOrgFunding2002 <- ggplot(data = org_avg_fund_2002,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2002 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2002)")

# Scatter Plot for the year 2003
scatterPlotOrgFunding2003 <- ggplot(data = org_avg_fund_2003,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2003 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2003)")

# Scatter Plot for the year 2004
scatterPlotOrgFunding2004 <- ggplot(data = org_avg_fund_2004,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2004 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2004)")

# Scatter Plot for the year 2005
scatterPlotOrgFunding2005 <- ggplot(data = org_avg_fund_2005,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2005 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2005)")

# Scatter Plot for the year 2006
scatterPlotOrgFunding2006 <- ggplot(data = org_avg_fund_2006,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2006 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2006)")

# Scatter Plot for the year 2007
scatterPlotOrgFunding2007 <- ggplot(data = org_avg_fund_2007,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2007 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2007)")

# Scatter Plot for the year 2008
scatterPlotOrgFunding2008 <- ggplot(data = org_avg_fund_2008,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2008 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2008)")

# Scatter Plot for the year 2009
scatterPlotOrgFunding2009 <- ggplot(data = org_avg_fund_2009,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2009 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2009)")

# Scatter Plot for the year 2010
scatterPlotOrgFunding2010 <- ggplot(data = org_avg_fund_2010,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2010 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2010)")

# Scatter Plot for the year 2011
scatterPlotOrgFunding2011 <- ggplot(data = org_avg_fund_2011,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2011 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2011)")

# Scatter Plot for the year 2012
scatterPlotOrgFunding2012 <- ggplot(data = org_avg_fund_2012,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2012 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2012)")

# Scatter Plot for the year 2013
scatterPlotOrgFunding2013 <- ggplot(data = org_avg_fund_2013,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2013 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2013)")

# Scatter Plot for the year 2014
scatterPlotOrgFunding2014 <- ggplot(data = org_avg_fund_2014,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2014 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2014)")

# Scatter Plot for the year 2015
scatterPlotOrgFunding2015 <- ggplot(data = org_avg_fund_2015,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2015 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2015)")

# Scatter Plot for the year 2016
scatterPlotOrgFunding2016 <- ggplot(data = org_avg_fund_2016,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2016 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2016)")

# Scatter Plot for the year 2017
scatterPlotOrgFunding2017 <- ggplot(data = org_avg_fund_2017,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2017 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2017)")

# Scatter Plot for the year 2018
scatterPlotOrgFunding2018 <- ggplot(data = org_avg_fund_2018,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2018 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2018)")

# Scatter Plot for the year 2019
scatterPlotOrgFunding2019 <- ggplot(data = org_avg_fund_2019,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2019 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2019)")

# Scatter Plot for the year 2020
scatterPlotOrgFunding2020 <- ggplot(data = org_avg_fund_2020,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2020 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2020)")

# Scatter Plot for the year 2021
scatterPlotOrgFunding2021 <- ggplot(data = org_avg_fund_2021,
                                    aes(x = ORG_NAME,
                                        y = AvgFunding))

scatterPlotOrgFunding2021 + geom_point(color = "#6495ED") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = lm,
              level = 0,
              color = "red") +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Average Funding ($)", x ="Organizations") +
  ggtitle("Organizations vs Average Funding (2021)")
