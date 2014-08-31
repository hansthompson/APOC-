library(plyr)
library(dplyr)
library(lubridate)
library(igraph)


load("gov_data.rda")




income  <- filter(gov_data, Transaction.Type == "Income" )
expense <- filter(gov_data, Transaction.Type == "Expenditure" )

connections <- 3
election <- "State Primary"
year <- 2013
show_pacs <- FALSE


dput(levels(gov_data$Group.Candidate.Name))



income$Date <- mdy(income$Date)
income$full_name <- factor(income$full_name)

income <- income[which(income$Election == election & income$Report.Year == year),]

tidy_income <- ddply(income, .(full_name, Group.Candidate.Name), function(x) {sum(x$Amount)} )

giver_nodes <- names(table(tidy_income$full_name))[table(tidy_income$full_name) > connections]

some_income <- tidy_income[tidy_income$full_name %in% giver_nodes,]

network <- graph.data.frame(some_income)

plot(network)
