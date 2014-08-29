library(plyr)
library(dplyr)
library(lubridate)
library(igraph)


load("gov_data.rda")




income  <- filter(gov_data, Amount > 0 )
expense <- filter(gov_data, Amount < 0 )

connections <- 15

income$full_name <- factor(income$full_name)

ddply(income, .(full_name, Group.Candidate.Name), function(x) {sum(x$Amount)} )

tidy_income <- ddply(income, .(full_name, Group.Candidate.Name), function(x) {sum(x$Amount)} )

giver_nodes <- names(table(tidy_income$full_name))[table(tidy_income$full_name) > connections]

tidy_income[tidy_income$full_name %in% giver_nodes,]
