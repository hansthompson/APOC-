library(plyr)
library(dplyr)
library(lubridate)
library(igraph)


load("gov_data.rda")




income  <- filter(gov_data, Amount > 0 )
expense <- filter(gov_data, Amount < 0 )

connections <- 25

income$full_name <- factor(income$full_name)

tidy_income <- ddply(income, .(full_name, Group.Candidate.Name), function(x) {sum(x$Amount)} )

giver_nodes <- names(table(tidy_income$full_name))[table(tidy_income$full_name) > connections]

some_income <- tidy_income[tidy_income$full_name %in% giver_nodes,]

network <- graph.data.frame(some_income)

plot(network)
