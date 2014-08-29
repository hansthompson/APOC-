library(plyr)
library(dplyr)
library(lubridate)

load("gov_data.rda")

income  <- filter(gov_data, Amount > 0 )
expense <- filter(gov_data, Amount < 0 )

income$full_name <- factor(income$full_name)

ddply(income, .(full_name, Group.Candidate.Name), function(x) {sum(x$Amount)} )

tidy_income <- ddply(income, .(full_name, Group.Candidate.Name), function(x) {sum(x$Amount)} )

tidy_income[table(tidy_income$full_name) > 15,]


