setwd("~/Analysis/Political Data Science/OpenElections Volunteering/")
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)


##########             BEGIN                  ######################
##############    For Missouri MO Presidential Primary ###############
dfd <- read.csv("DEM2016PresidentialPreferencePrimaryResultsbyCongressionalDistrict.csv",
                   stringsAsFactors=FALSE )
dfr <- read.csv("REP2016PresidentialPreferencePrimaryResultsbyCongressionalDistrict.csv",
               stringsAsFactors=FALSE )
dfo <- read.csv("LIB2016PresidentialPreferencePrimaryResultsbyCongressionalDistrict.csv",
               stringsAsFactors=FALSE )



# gather() Reshaping wide format to long format
# data %>% gather(key, value, ..., na.rm = FALSE, convert = FALSE)

long_DFd <- dfd %>% gather(Candidate, Votes, c(5:14))
long_DFr <- dfr %>% gather(Candidate, Votes, c(5:17))
dfl <- dfo[-11]
long_DFl <- dfl %>% gather(Candidate, Votes, c(5:10))
dfc <- dfo[-c(5:10)]
dfc$Party <- gsub("LIB", "CON", dfc$Party)
long_DFc <- dfc %>% gather(Candidate, Votes, c(5))


output <- rbind(long_DFd, long_DFr, long_DFl, long_DFc)
names(output) <- c("county", "office", "district", "party", "candidate", "votes")
output$district <- gsub("CD ", "", output$district)
output$candidate <- gsub("\\.", " ", output$candidate)
output$candidate <- gsub("  ", " ", output$candidate)
unique(output$county)
unique(output$candidate)
write.csv(output, "20160315_MO_PresidentialPrimary.csv", row.names=FALSE)
write.csv(output, "openelections-data-mo/2016/20160315_MO_PresidentialPrimary.csv", row.names=FALSE)


##########             END                  ######################
##############    For Missouri MO Presidential Primary ###############



##########             BEGIN                  ######################
##############    For Iowa General 2016      ###############

df1 <- read.csv("85_PrecinctsExcel.csv",
               stringsAsFactors=FALSE, check.names=FALSE )

names(df) <- sub("-", "ZZZZ", names(df))
long_DF <- df %>% gather(countyprecinct, Votes, c(4:5044))
unique(long_DF$PoliticalPartyName)

long_DF$PoliticalPartyName <- gsub("Republican Party ", "REP", long_DF$PoliticalPartyName)
long_DF$PoliticalPartyName <- gsub("Democratic Party ", "DEM", long_DF$PoliticalPartyName)

long_DF$PoliticalPartyName <- gsub("Constitution Party ", "CON", long_DF$PoliticalPartyName)
long_DF$PoliticalPartyName <- gsub("Iowa Green Party ", "GRN", long_DF$PoliticalPartyName)

long_DF$PoliticalPartyName <- gsub("Libertarian Party ", "LIB", long_DF$PoliticalPartyName)

long_DF$PoliticalPartyName <- gsub("Legal Marijuana Now ", "OTH", long_DF$PoliticalPartyName)
long_DF$PoliticalPartyName <- gsub("New Independent Party Iowa ", "OTH", long_DF$PoliticalPartyName)
long_DF$PoliticalPartyName <- gsub("Nominated by Petition ", "OTH", long_DF$PoliticalPartyName)
long_DF$PoliticalPartyName <- gsub("Party for Socialism and Liberation ", "OTH", long_DF$PoliticalPartyName)
long_DF$PoliticalPartyName <- gsub("Stand Up To Bullies ", "OTH", long_DF$PoliticalPartyName)

long_DF$PoliticalPartyName <- gsub("Independent ", "IND", long_DF$PoliticalPartyName)

long_DF$PoliticalPartyName <- gsub(" ", "OTH", long_DF$PoliticalPartyName)

unique(long_DF$RaceTitle)
long_DF$RaceTitle <- gsub("President and Vice President ", "President", long_DF$RaceTitle)
long_DF$RaceTitle <- gsub("US Senator ", "U.S. Senate", long_DF$RaceTitle)


separate_DF <- long_DF %>% separate(RaceTitle, c("office", "district"), 
                                    sep = " Dist. ")
separate_DF$office <- gsub("US Rep.", "U.S. House", separate_DF$office)
separate_DF$office <- gsub("State Senator", "State Senate", separate_DF$office)
separate_DF$office <- gsub("State Rep.", "State House", separate_DF$office)
separte_DF <- sapply(separate_DF, as.character)
separte_DF[is.na(separte_DF)] <- ""

separate_DF$district <- as.character(separate_DF$district)
separate_DF$district[is.na(separate_DF$district)] <- ""

tmpT <- filter(separate_DF, grepl("Total", countyprecinct))
tmpT$countyprecinct <- gsub(" Total", "", tmpT$countyprecinct)
unique(tmpT$countyprecinct)
split_DFT <- tmpT %>% separate(countyprecinct, c("county", "precinct"), 
                                     sep = "ZZZZ")
outputT <- split_DFT %>% select(county, precinct, office, district, PoliticalPartyName,
                              CandidateName, Votes)
names(outputT) <- c("county", "precinct", "office", "district", "party", 
                   "candidate", "votes")


tmpAbs <- filter(separate_DF, grepl("Absentee", countyprecinct))
tmpAbs$countyprecinct <- gsub(" Absentee", "", tmpAbs$countyprecinct)
unique(tmpAbs$countyprecinct)
split_DFAbs <- tmpAbs %>% separate(countyprecinct, c("county", "precinct"), 
                               sep = "ZZZZ")
outputAbs <- split_DFAbs %>% select(county, precinct, office, district, PoliticalPartyName,
                                CandidateName, Votes)
names(outputAbs) <- c("county", "precinct", "office", "district", "party", 
                    "candidate", "absentee_votes")


tmpP <- filter(separate_DF, grepl("Polling", countyprecinct))
tmpP$countyprecinct <- gsub(" Polling", "", tmpP$countyprecinct)
unique(tmpP$countyprecinct)
split_DFP <- tmpP %>% separate(countyprecinct, c("county", "precinct"), 
                               sep = "ZZZZ")
outputP <- split_DFP %>% select(county, precinct, office, district, PoliticalPartyName,
                                CandidateName, Votes)
names(outputP) <- c("county", "precinct", "office", "district", "party", 
                    "candidate", "polling_votes")


xyz <- cbind(outputT, outputAbs$absentee_votes, outputP$polling_votes)
names(xyz) <- c("county", "precinct", "office", "district", "party", 
                    "candidate", "votes",  "absentee_votes", "polling_votes")
xyz <- xyz %>% select(county, precinct, office, district, party, candidate,
                      absentee_votes, polling_votes, votes )


final <- xyz %>% filter(!is.na(votes))
write.csv(final, "2016_ia_general_precinct.csv", row.names=FALSE)

##########             END                  ######################
##############    For Iowa General 2016 ###############





##########             BEGIN                  ######################
##############    For Iowa Primary 2016      ###############
df <- read.csv("85_PrecinctsExcel.csv",
               stringsAsFactors=FALSE, check.names=FALSE )

names(df) <- sub("-", "ZZZZ", names(df))

long_DF <- df %>% gather(countyprecinct, Votes, c(4:5119))
unique(long_DF$PoliticalPartyName)
#long_DF$PoliticalPartyName <- gsub("Republican Party ", "REP", long_DF$PoliticalPartyName)
#long_DF$PoliticalPartyName <- gsub("Democratic Party ", "DEM", long_DF$PoliticalPartyName)

#long_DF$PoliticalPartyName <- gsub(" ", "OTH", long_DF$PoliticalPartyName)

unique(long_DF$RaceTitle)
long_DF$RaceTitle <- gsub("President and Vice President ", "President", long_DF$RaceTitle)
long_DF$RaceTitle <- gsub("US Senator ", "U.S. Senate", long_DF$RaceTitle)
long_DF$RaceTitle <- gsub(" District ", " Dist. ", long_DF$RaceTitle)

separate_DF <- long_DF %>% separate(RaceTitle, c("office", "party"), 
                                    sep = " - ")

separate_DF <- separate_DF %>% separate(office, c("office", "district"), 
                                    sep = " Dist. ")
unique(separate_DF$office)
separate_DF$office <- gsub("United States Senator", "U.S. Senate", separate_DF$office)
separate_DF$office <- gsub("U.S. Rep.", "U.S. House", separate_DF$office)
separate_DF$office <- gsub("State Senator", "State Senate", separate_DF$office)
separate_DF$office <- gsub("State Rep.", "State House", separate_DF$office)
separte_DF <- sapply(separate_DF, as.character)
separte_DF[is.na(separte_DF)] <- ""

separate_DF$district <- as.character(separate_DF$district)
separate_DF$district[is.na(separate_DF$district)] <- ""

tmpT <- filter(separate_DF, grepl("Total", countyprecinct))
tmpT$countyprecinct <- gsub(" Total", "", tmpT$countyprecinct)
unique(tmpT$countyprecinct)
split_DFT <- tmpT %>% separate(countyprecinct, c("county", "precinct"), 
                               sep = "ZZZZ")
outputT <- split_DFT %>% select(county, precinct, office, district, party,
                                CandidateName, Votes)
names(outputT) <- c("county", "precinct", "office", "district", "party", 
                    "candidate", "votes")


tmpAbs <- filter(separate_DF, grepl(" Absentee", countyprecinct))
tmpAbs$countyprecinct <- gsub(" Absentee", "", tmpAbs$countyprecinct)
unique(tmpAbs$countyprecinct)
split_DFAbs <- tmpAbs %>% separate(countyprecinct, c("county", "precinct"), 
                                   sep = "ZZZZ")
outputAbs <- split_DFAbs %>% select(county, precinct, office, district, party,
                                    CandidateName, Votes)
names(outputAbs) <- c("county", "precinct", "office", "district", "party", 
                      "candidate", "absentee_votes")


tmpP <- filter(separate_DF, grepl("Polling", countyprecinct))
tmpP$countyprecinct <- gsub(" Polling", "", tmpP$countyprecinct)
unique(tmpP$countyprecinct)
split_DFP <- tmpP %>% separate(countyprecinct, c("county", "precinct"), 
                               sep = "ZZZZ")
outputP <- split_DFP %>% select(county, precinct, office, district, party,
                                CandidateName, Votes)
names(outputP) <- c("county", "precinct", "office", "district", "party", 
                    "candidate", "polling_votes")


xyz <- cbind(outputT, outputAbs$absentee_votes, outputP$polling_votes)
names(xyz) <- c("county", "precinct", "office", "district", "party", 
                "candidate", "votes",  "absentee_votes", "polling_votes")
xyz <- xyz %>% select(county, precinct, office, district, party, candidate,
                      absentee_votes, polling_votes, votes )


final <- xyz %>% filter(!is.na(votes))
final$party <- gsub("Dem", "DEM", final$party)
final$party <- gsub("Rep", "REP", final$party)
write.csv(final, "20160607_ia_primary_precinct.csv", row.names=FALSE)



