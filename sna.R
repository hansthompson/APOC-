library(plyr)
library(dplyr)
library(lubridate)
library(igraph)
library(ggplot2)
library(stringr)

load("gov_data.rda")


income  <- filter(gov_data, Transaction.Type == "Income" )
expense <- filter(gov_data, Transaction.Type == "Expenditure" )
income$Date <- mdy(income$Date)
income$full_name <- factor(income$full_name)
tidy_income <- ddply(income, .(full_name, Group.Candidate.Name), function(x) {sum(x$Amount)} )
colnames(tidy_income)[3] <- "Amount"


connections <- 2
#election <- "State Primary"
year <- 2013
#income <- income[which(income$Election == election & income$Report.Year == year),]
show_pacs <- FALSE
#dput(levels(gov_data$Group.Candidate.Name))
candidates <- c("Amy Demboski",  "Hollis S. French")
tidy_income <- tidy_income[tidy_income$Group.Candidate.Name %in% candidates,]


#giver_nodes <- names(table(tidy_income$full_name))[table(tidy_income$full_name) >= connections]
#some_income <- tidy_income[tidy_income$full_name %in% giver_nodes,]

network <- graph.data.frame(tidy_income[,1:2])

plot(network)

ggplot(data = tidy_income, aes(x = Amount, fill = Group.Candidate.Name)) + geom_histogram()
