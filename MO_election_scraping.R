rm(list=ls())
setwd("~/Analysis/Political Data Science/OpenElections Volunteering/")
library(XML)
require(RSelenium)
library(dplyr)
library(tidyr)


initializeElection <- function(selected_election) {
    pJS <- phantom()
    Sys.sleep(5) # give the binary a moment
    # RSelenium has a main reference class named remoteDriver. To connect to a server 
    # need to instantiate a new remoteDriver with appropriate options.
    remDrv <- remoteDriver(browserName = 'phantomjs')
    remDrv$open()
    
    #INPUTS
    url <- 'http://enrarchives.sos.mo.gov/enrnet/CountyResults.aspx'
#    url <- 'http://enr.sos.mo.gov/CountyResults.aspx'
    
    
    # Simulate browser session and fill out form -----------------------------------
    
    remDrv$navigate(url) #send headless browser to url
    
    #----- ELECTION SELECTION from DROPDOWN ------------------#
    #Select the Election from DROPDOWN - search using xpath id
    elec_xp <- paste0('//*[@id="cboElectionNames"]/option[' , selected_election , ']')
    remDrv$findElement(using = "xpath", elec_xp)$clickElement() #election is set
    Sys.sleep(5) # give the binary a moment
    
    #Verify if the selection worked...
    webElem <- remDrv$findElement(using = 'xpath', 
                                  "//select[@id='cboElectionNames']")
    #webElem$getElementAttribute('value')    # check to see if it worked
    
    # ---- Click the button to select the Election
    eBTN <- '//*[@id="MainContent_btnElectionType"]'
    remDrv$findElement(using = 'xpath', 
                       eBTN)$clickElement()
#    Sys.sleep(5) # give the binary a moment
    return(remDrv)
}

find_selected_county <- function(raw) {
    all_counties <- xpathSApply(htmlParse(raw), '//*[@id="cboCounty"]/option', xmlValue)
    counties_val  <- xpathSApply(htmlParse(raw), '//*[@id="cboCounty"]/option', xmlAttrs) 
    chosen_county <- grep("selected", counties_val)
    county <- all_counties[chosen_county]
    county #this is useful to name the CSV file
}

getRawDataForCounty <- function(selected_county){
    
    #----- COUNTY SELECTION from DROPDOWN ------------------#
    co_xp <- paste0('//*[@id="cboCounty"]/option[' , selected_county , ']')
    print(co_xp)
    remDrv$findElement(using = "xpath", co_xp)$clickElement() #county has been set
    
    # //*[@id="cboCounty"]/option[14] # inspect in webbrowser and copy xpath
    
    #-------------Click the button to select the County-------------#
    cBTN <- '//*[@id="MainContent_btnCountyChange"]'
    remDrv$findElement(using = 'xpath', 
                       cBTN)$clickElement()
    Sys.sleep(5) # give the binary a moment
    
    ## Get the HTML data from the page and process it...
    raw <- remDrv$getPageSource()[[1]]

    counties_val  <- xpathSApply(htmlParse(raw), '//*[@id="cboCounty"]/option', xmlAttrs) 
    chosen_county <- grep("selected", counties_val)
#    print(chosen_county)
#    print(find_selected_county(raw))        
    
    #Extract the Table (Election results)
    resTable <- raw %>% readHTMLTable()
    resDf <- resTable[[1]]   # return desired data frame from list of tables
#    print(nrow(resDf))
    return(resDf)
}


transformDataToDesiredFormat <- function(rawData){
    res <- rawData
#    res <- resDf
    ## Make a compact Df, by removing lines with Party Totals and Vote counts and blank lines
    res <- res %>% filter(V1 !="Â") %>% select(V1, V2, V3)
    res %>% mutate_if(is.factor, as.character) -> res
    
    races <- res %>% filter(V1 != "Â", V2=="Â") %>% select(V1)

    #find the starting row numbers for each race
    # example match(races[6], res$V1)
    
    #sapply takes each race name one at a time...
    
    race_start <- sapply(races, function(x){match(x, res$V1)})
    race_end <- race_start - 1
    race_end <- c(race_end[-1], nrow(res))  # -1 drops first element & add last element
    
    res$V4 <- 0
    for (i in 1:nrow(races)){
        for (j in (race_start[i]+1):race_end[i]) {
            # print(c(j))
            res$V4[j] <- races[i,1]
            #        print(c(race_start[])
        }
    }
    
    results <- res[-1,] %>% filter(V2 != "Â")
    names(results) <- c("candidate", "party", "votes", "office")
    
    separate_DF <- results %>% separate(office, c("office", "district"), 
                                        sep = " - District ", fill = "right")
    separate_DF <- sapply(separate_DF, as.character)
    separate_DF[is.na(separate_DF)] <- ""
    separate_DF <- as.data.frame(separate_DF)
    
    output <- separate_DF %>% select(office, district, party, candidate, votes)
    return(output)
}


cleanDF <- function(df){
    df$party <- gsub("Republican", "REP", df$party)
    df$party <- gsub("Democratic", "DEM", df$party)
    df$party <- gsub("Libertarian", "LIB", df$party)
    df$party <- gsub("Constitution", "CON", df$party)
    
    df$office <- gsub("U.S. Representative", "U.S. House", df$office)
    df$office <- gsub("U.S. Senator", "U.S. Senate", df$office)
    df$office <- gsub("State Senator", "State Senate", df$office)
    df$office <- gsub("State Representative", "State House", df$office)
    
    df$office <- gsub("U.S. President", "President", df$office)
    return(df)
}

# select election
selected_election <- 2

# Initialize
remDrv <- initializeElection(selected_election)

# find all counties for above election
## Get the HTML data from the page and process it...
raw <- remDrv$getPageSource()[[1]]
print(find_selected_county(raw))  # verifying no county has been selected
all_counties <- xpathSApply(htmlParse(raw), '//*[@id="cboCounty"]/option', xmlValue)

# select county
output_all <- NULL
for (co in seq_along(all_counties)){
#for (co in 1:1){
    print( paste0("processing...  ", co," ",all_counties[co])
          )
    resDf <- getRawDataForCounty(co)
    print(dim(resDf))
    output_county <- transformDataToDesiredFormat(resDf)
    print(paste("Done Transforming", co))
    output_county <- cleanDF(output_county)
    output_county$county <- all_counties[co]
    output_all <- rbind(output_all, output_county)
}

output_all <- output_all %>% select(county, office, district, party, candidate, votes)
output_all$votes <- as.numeric(gsub(",", "", output_all$votes))
write.csv(output_all, "20161108__mo_special__general__state_senate.csv", row.names=FALSE)

pJS$stop() # close the PhantomJS process, note we dont call remDr$closeServer()
















#################################################################################
#------------Stuff not being used ------------#

all_elections <- xpathSApply(htmlParse(raw), 
                             '//*[@id="cboElectionNames"]/option', xmlValue) 
e_list <- xpathSApply(htmlParse(raw),
                      '//*[@id="cboElectionNames"]/option', xmlAttrs) 
chosen_election <- grep("selected", e_list) #find the element that containts the word selected
election <- all_elections[chosen_election]
election #this is useful



county <- find_selected_county(raw)



#' @param res The table of results
#' @param election The Name of the election
#' @param County The name of the county (in MO)
format_columns_and_save_as_CSV <- function(results_list, counties_list, election_list){
  #  
  
  #come up with appropriate name for the file
  
  #write the CSV file
}














