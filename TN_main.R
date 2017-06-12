rm(list=ls())
setwd("~/Analysis/Political Data Science/OpenElections Volunteering/")

source("TN_election_pdf_helper_functions.R")
source("TN_election_pdf_helper_functions_mult_col_candidate_list_v2.R")
main()


###### start of 2006-11 US Senate parameters #######

URL <- 'http://share.tn.gov/sos/election/results/2006-11/USSPct.pdf'

filename <- "USSPct.pdf"
newfilename <- "20061107__tn__general__us_senate__precinct.csv"

rmString1 <- "November 7, 2006"
rmString2 <- "^General Election"

district_separtor_str <- ""
CANDIDATE_BLK_EXTRA_LINES <- 2

DF_FILTER_STRINGS <- c("COUNTY", "DISTRICT TOTALS ", "^Page ", "STATEWIDE TOTALS")

demarc_race_str <- "Governor"
demarc_county_str <- "COUNTY:"
demarc_page_str <- "Page"

####### end of 2006-11 US Senate parameters #######


###### start of 2006-11 Governor parameters #######

URL <- 'http://share.tn.gov/sos/election/results/2006-11/GovPct.pdf'

filename <- "GovPct.pdf"
newfilename <- "20061107__tn__general__governor__precinct.csv"

rmString1 <- "November 7, 2006"
rmString2 <- "^General Election"

district_separtor_str <- ""
CANDIDATE_BLK_EXTRA_LINES <- 2

DF_FILTER_STRINGS <- c("COUNTY", "DISTRICT TOTALS ", "^Page ", "STATEWIDE TOTALS")

demarc_race_str <- "Governor"
demarc_county_str <- "COUNTY:"
demarc_page_str <- "Page"

####### end of 2006-11 Governor parameters #######



###### start of 2004-08 Republican Primary State House parameters #######

URL <- 'http://share.tn.gov/sos/election/results/2004-8/repthpct.pdf'

filename <- "repthpct.pdf"
newfilename <- "20040508__tn__primary__republican__state_house__precinct.csv"

rmString1 <- "August 5, 2004"
rmString2 <- "^Republican Primary"

district_separtor_str <- "District "
CANDIDATE_BLK_EXTRA_LINES <- 2

DF_FILTER_STRINGS <- c("COUNTY", "DISTRICT TOTALS ", "^Page ", "STATEWIDE TOTALS")

demarc_race_str <- "Tennessee House of Representatives"
demarc_county_str <- "COUNTY:"
demarc_page_str <- "Page"

####### end of 2004-08 Republican Primary US House parameters #######


###### start of 2004-08 Democratic Primary State House parameters #######

URL <- 'http://share.tn.gov/sos/election/results/2004-8/demthpct.pdf'

filename <- "demthpct.pdf"
newfilename <- "20040508__tn__primary__democratic__state_house__precinct.csv"

rmString1 <- "August 5, 2004"
rmString2 <- "^Democratic Primary"

district_separtor_str <- "District "
CANDIDATE_BLK_EXTRA_LINES <- 2

DF_FILTER_STRINGS <- c("COUNTY", "DISTRICT TOTALS ", "^Page ", "STATEWIDE TOTALS")

demarc_race_str <- "Tennessee House of Representatives"
demarc_county_str <- "COUNTY:"
demarc_page_str <- "Page"

############### end of parameters ##################




###### start of 2004-08 Republican Primary State Senate parameters #######

URL <- 'http://share.tn.gov/sos/election/results/2004-8/reptspct.pdf'

filename <- "reptspct.pdf"
newfilename <- "20040508__tn__primary__republican__state_senate__precinct.csv"

rmString1 <- "August 5, 2004"
rmString2 <- "^Republican Primary"

district_separtor_str <- "District "
CANDIDATE_BLK_EXTRA_LINES <- 2

DF_FILTER_STRINGS <- c("COUNTY", "DISTRICT TOTALS ", "^Page ", "STATEWIDE TOTALS")

demarc_race_str <- "Tennessee Senate"
demarc_county_str <- "COUNTY:"
demarc_page_str <- "Page"

####### end of 2004-08 Republican Primary US House parameters #######


###### start of 2004-08 Democratic Primary State Senate parameters #######

URL <- 'http://share.tn.gov/sos/election/results/2004-8/demtspct.pdf'

filename <- "demtspct.pdf"
newfilename <- "20040508__tn__primary__democratic__state_senate__precinct.csv"

rmString1 <- "August 5, 2004"
rmString2 <- "^Democratic Primary"

district_separtor_str <- "District "
CANDIDATE_BLK_EXTRA_LINES <- 2

DF_FILTER_STRINGS <- c("COUNTY", "DISTRICT TOTALS ", "^Page ", "STATEWIDE TOTALS")

demarc_race_str <- "Tennessee Senate"
demarc_county_str <- "COUNTY:"
demarc_page_str <- "Page"

############### end of parameters ##################



###### start of 2004-08 Republican Primary US House parameters #######

URL <- 'http://share.tn.gov/sos/election/results/2004-8/repuhpct.pdf'

filename <- "repuhpct.pdf"
newfilename <- "20040508__tn__primary__republican__us_house__precinct.csv"

rmString1 <- "August 5, 2004"
rmString2 <- "^Republican Primary"

district_separtor_str <- "District "
CANDIDATE_BLK_EXTRA_LINES <- 2

DF_FILTER_STRINGS <- c("COUNTY", "DISTRICT TOTALS ", "^Page ", "STATEWIDE TOTALS")

demarc_race_str <- "U.S. House"
demarc_county_str <- "COUNTY:"
demarc_page_str <- "Page"

####### end of 2004-08 Republican Primary US House parameters #######


###### start of 2004-08 Democratic Primary US House parameters #######

URL <- 'http://share.tn.gov/sos/election/results/2004-8/demuhpct.pdf'

filename <- "demuhpct.pdf"
newfilename <- "20040508__tn__primary__democratic__us_house__precinct.csv"

rmString1 <- "August 5, 2004"
rmString2 <- "^Democratic Primary"

district_separtor_str <- "District "
CANDIDATE_BLK_EXTRA_LINES <- 2

DF_FILTER_STRINGS <- c("COUNTY", "DISTRICT TOTALS ", "^Page ", "STATEWIDE TOTALS")

demarc_race_str <- "U.S. House"
demarc_county_str <- "COUNTY:"
demarc_page_str <- "Page"

############### end of parameters ##################

source("TN_election_pdf_helper_functions.R")
main()
