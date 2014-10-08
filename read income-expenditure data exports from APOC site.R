library(dplyr)
library(stringr)
library(lubridate)
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
gov_data$Date <- mdy(gov_data$Date)
gov_data$full_name <- factor(gov_data$full_name)

PACs <- c("AK Sea Pilot PAC Fund", "Alaska Democratic Labor Caucus", 
  "Alaska Libertarian Party", "Alaska Women for Political Action ", 
  "Alaska Young Democrats", "ATATruckPAC", "District 1 Republicans", 
  "District One Democrats", "DNC Services Corporation", "First City Republican Women's Club", 
  "Greater Juneau Democratic District", "KODIAK DEMOCRARTIC PARTY",  "Matanuska Telephone Association Employee PAC", 
  "Planned Parenthood Votes PAC", "Totem Ocean Trailer Express Alaska Employee Political Action Committee", 
  "University of Alaska Federation of Teachers Local 2404", "A.L.I.V.E. - Voluntary",  "ABC Alaska PAC", "ACS Employee PAC", "AK CARE (Alaska Committee for the Advancement of Rural Energy)", 
  "AkH&LA PAC (Alaska Hotel & Lodging Association PAC)",  "Alaska's Doctors of Optometry", "Alaska Auto Dealers Assn. PAC", 
  "Alaska Bering Sea Crabbers Political Action Committee", "Alaska Business Political Action Committee", 
  "Alaska Cabaret, Hotel, Restaurant and Retailer's Association Political Action Committee (Alaska CHARR PAC)", 
  "Alaska Conservation Voters", "Alaska Correctional Officers Association / PAC", 
  "Alaska Democratic Party", "ALASKA DEMOCRATIC PROGRESSIVE CAUCUS", 
  "Alaska District 30 Democrats ", "Alaska Energy 1st", "Alaska Federation of Republican Women", 
  "Alaska Ironworker's Political Action Committe", "Alaska Laborers' Political & Education Committee", 
  "Alaska Medical Political Action Committee", "Alaska Miners Association PAC", 
  "Alaska Nurses Association", "Alaska Pharmacists Association PAC", 
  "Alaska Piledrivers Political Action Committee", "Alaska Professional Fire Fighters Association", 
  "Alaska Prosperity Project", "Alaska Public Safety Coalition", 
  "Alaska REALTORS Political Action Committee", "Alaska Regional Council of Carpenters Political Action Committee", 
  "Alaska Republican Party", "Alaska Right To Life Pro-Life Victory Fund", 
  "Alaska Right to Life, PAC", "Alaska SafePAC ", "Alaska Sea Party:  Restoring Coastal Management", 
  "Alaska Society of Certified Public Accountants Political Action Committee", 
  "Alaska State Employees Association Local 52 Politcal Action Committee", 
  "Alaska State Home Building Association Build-Pac", "Alaska State Hospital & Nursing Home PAC", 
  "Alaska Travel Industry Association PAC dba Alaska Travel PAC", 
  "Alaskans for Limited Government", "Alaskans Protecting the Constitution", "Anchorage Central Labor Coucil PAC (Anchorage Central Labor Council Political Action Committee)", 
  "Anchorage CHARR PAC", "Anchorage Democrats", "Anchorage Education Association Political Action Committee", 
  "Anchorage Home Builders Association Build-Pac", "Anchorage Police Department Employees Association", 
  "Anchorage Republican Women's Club", "Anchorage Tomorrow",  "BP Alaska Employees Political Action Committee", 
  "Bristol Bay Deserves Truth", "Building and Construction Trades South Central Alaska", 
 "Capital City Republican Women", "Capital City Republicans ", 
  "Carpenters Legislative Improvement Committee", 
 "CDFU Fish Pac", 
 "Citizen's For Competition",
  "Citizens for Sustainable Communities Opposing Dillingham's Annexation & Fish Tax", 
  "Citizens Support Anchorage PAC", "Citizens United for Quality Healthcare", 
  "Classified Employees' Association PACE", "Concerned Citizens of the Unorganized Borough", 
  "ConocoPhillips Alaska Employee PAC", "Construction Trades PAC", "D34 Democrats ", 
 "Dentists of Alaska Political Action Committee",  "District 11 Republican Party", "District 21 RPA", 
  "District 33", "District 5(in litigation on boundaries) Republicans", 
  "District 6 Republican Party", "District 8 Democrats",  "Education Support Staff Association", "EMPLOYEES POLITICAL INFORMATION COMMITTEE", 
  "ENSTAR Employee PAC",  "Fairbanks Fire Fighters Association", 
  "Fairbanks Interior Workers", "Fairbanks Republican Women", "FEA - PACE (Fairbanks Education Association - Political Action Committee for Education)", 
  "Flint Hills Resources Alaska Employee Political Action Committee", "Friends of the Candidate", "Friends of the Interior", "Golden Heart PAC", "GOOD SENSE COMMITTEE", 
   "House Democratic Campaign Committee",  
  "IAFF L1264 PAC", "Insurance and Financial Advisors Political Action Committee", 
  "Interior Democrats", "Interior Taxpayers Association, Incorporated", 
  "International Brotherhood of Electrical Workers Local Union 1547 Political Action Committee", 
  "INUU PAC", "It's Our Oil, Duh", "IUOE Local 302 PAC", "IUOE Local 302 PAC \n(International Union of Operating Engineers Local 302 Political Action Committe", 
  "Juneau Central Labor Council Political Action Committee", "Juneau Pro-Choice Coalition Political Action Committee", 
 "Kenai Peninsula Education Association-PACE", 
  "Kenai Peninsula Republican Women",  "Laborers' International Union of North America, 341 PAC", "Local367PAC",  "Masters, Mates & Pilots Political Contribution Fund", 
  "Mat-Su Democrats", "Mat-Su Education Association", "MATANUSKA-SUSITNA REPUBLICAN WOMEN'S CLUB","Midnight Sun Republican Women's Club", "NEA-Alaska PACE (Political Action Committee for Education)", "No on  Proposition 3",  
  "Oh no, Not Ralph again!", "One Anchorage Yes on Prop 5", 
  "Plumbers & Pipefitters UA Local 262", "Political Action Committee / AGC", 
  "Potlatch Political Action Committee", "Protect Your Freedoms Vote No on Prop 5", 
  "Protect Your Rights - Vote NO on Prop 5", "Public Employees Local 71 Political League Candidate Fund", 
  "Public Safety Employees Association", "Putting Alaskans First Committee", 
  "Putting Alaskans First PAC", "Railroad Workers, PAC", "Restoring Liberty Alaska PAC", "Road Bonds Yes!", 
 "SCHOOL BONDS YES!", "Sealaska State Political Action Committee", 
  "Senate Democratic Campaign Committee", "Sitka Republican Women", "Smoke Free Palmer Vote Yes on Prop 3", 
  "Southeast Alaska Seineers Assn.", "Sportsmen's Conservation Alliance Political Action Comittee", 
  "TelAk PAC - Telalaska Political Action Committee", "Tesoro Alaska Political Action Committee", 
  "The Accountability Project\n", "The Ladies of the Valley,PAC", 
"Two Million is Too Much", "UA Local 375", 
  "UAA Faculty and Staff Association", "United Academics AAUP/AFT PAC", 
  "United Brotherhood of Carpenters & Joiners of America Local 1243 Political Action Committee", 
  "United Fishermen of Alaska Fish PAC", "United Food & Commercial Workers Union Local #1496", 
  "VAFT-COPE (Valdez Federation of Teachers - Committee on Political Education", 
  "Valley Republican Women's Club", "Vote No On 2", 
  "We Are Alaska", "We, The People", "Wells Fargo Bank Alaska PAC","Working Families of Alaska", "Abbott Loop Democrats", 
  "Alaska District 30 Democrats", "Alaska Fisheries Conservation Alliance", 
  "Alaska Laborers Political Action Committee", "Alaskans for a Fair Minimum Wage", 
  "Alaskans for Property Tax Relief Now","Bristol Bay Forever Inc.", "Campaign to Regulate Marijuana","Citizens Against New Taxes", "District 4-B GOP", "District 5 Republicans", 
  "District Thirty Three Democrats","Fairbanks Construction PAC", "Friends of Kodiak High School", 
  "International Union of Painters and Allied Trades Political Action Together Political Committee",  "MAT-SU DEMOCRATS", "Mat-Su Education Association PACE", "North Pole Republican Women",  "Planned Parenthood Votes Northwest Alaska PAC", 
  "Public Employees Local 71 Supporting League", "Senate Democratic Campaign Committee (SDCC)", 
  "STOP THE GIVEAWAY! VOTE YES ON PROP 1", "Support Prop W-1   Build a New Wasilla Library","VAFT-COPE (Valdez Federation of Teachers' Committee on Political Education)", "Vote No on One", "Vote YES On Houston Prop 1", 
  "Vote Yes! Repeal the Giveaway","Yes on Prop B-1 Mat-Su Alcohol Tax", 
  "Yes on School Bonds", "yesonkenai1", "Your Vote Counts",  "Alaska Cabaret, Hotel, Restaurant and Retailer's Association Political Action Committee (Alaska CHAR", 
  "Alaska Constitution Party", "Alaska Nurses Association-PAC", 
  "Alaska Republican Assembly", "Alaska SafePAC", "Alaska Women for Political Action", 
  "Alaska Women Vote", "Anchorage Central Labor Coucil PAC Anchorage Central Labor Council Political Action Committee)",  "Capital City Republicans",
  "District 36 Democrats", "District 36 Republicans", "District 9 Republican Party", "HDR, Inc. Alaska State Political Action Committee", 
  "It's About Alaska", "IUOE Local 302 PAC  (International Union of Operating Engineers Local 302 Political Action Committe", 
  "Keep Alaska Competitive - Vote No on 1", "KODIAK DEMOCRATIC PARTY", "Repeal 37","Southeast Alaska Seiners Pac",  
  "The Accountability Project", 
  "Valley Republican Women",  "We Are Alaska - No on One", "Yes On Prop One"
)
PACs <- gsub("\\)","", PACs)
PACs <- gsub("\\(","-", PACs)


pac_vector <- c()
##takes a long time.  Need a better method
for(i in seq(dim(gov_data)[1])) {
    
pac_vector <- c(pac_vector, str_detect(gov_data$Group.Candidate.Name[i], PACs))
                
}
    
gov_data <- cbind(gov_data, pac_vector)

save(gov_data, file = "gov_data.rda")
