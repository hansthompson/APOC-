library(dplyr)
library(stringr)
Bleach_Names <- function(name) {
    require(stringr)
    name <- tolower(name)
    name <- str_trim(name)
    name <- str_replace_all(name, "[[:punct:]]", " ")
    
    simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }

    
    sapply(name, simpleCap)
}


files <- list.files(getwd(), pattern = ".csv")

all_data <- data.frame()

for(i in seq(files)) {

dat <- read.csv(files[i])

all_data <- rbind(all_data , dat)
}

head(all_data)
#clean headers 
colnames(all_data) <- c(colnames(all_data)[2:29],"")
#remove columns
all_data<- all_data[,c(1:15,18:20)]
#combine first and last name for donors
all_data <- mutate(all_data, full_name = str_trim(paste0(paste0(Bleach_Names(all_data$Last.Business.Name), ", "), Bleach_Names(all_data$First.Name))))


#convert Amount to numeric 
all_data$Amount <- gsub("\\$", "", all_data$Amount)
all_data$Amount <- gsub("\\)","", all_data$Amount)
all_data$Amount <- gsub("\\(","-", all_data$Amount)
all_data$Amount <- gsub(",", "", all_data$Amount)
all_data$Amount <- as.numeric(all_data$Amount)


gov_data <- select(all_data, Date, Transaction.Type, Payment.Type, Payment.Detail, Amount, Last.Business.Name, 
                             First.Name, Address, City, State, Zip, Country, Occupation, Employer, Purpose.of.Expenditure, 
                             Group.Candidate.Name, full_name)

gov_data <- all_data

save(gov_data, file = "gov_data.rda")
