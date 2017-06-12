setwd("~/Analysis/Political Data Science/OpenElections Volunteering/")
library(data.table)
library(dplyr)
library(tidyr)
library(pdftools)
library(pdftables)
library(tabulizer)


url <- 'http://elections.alaska.gov/results/08GENR/data/sovc/hd2.pdf'

f <- system.file("hd2.pdf", package = "tabulizer")
out1 <- extract_tables('hd1.pdf', pages = 1)
str(out1)
df <- do.call(cbind, out1[])
dftab <- out1[[1]]

out2 <- extract_tables('AAP_cta_table_volume_3.pdf', method = "data.frame")
str(out2)
out2[[1]]

out3 <- extract_text('hd2.pdf')
cat(out3, sep = "\n")
cat(out3)
writeLines(out3, "outfile.txt")

get_n_pages('hd1.pdf')
get_page_dims('hd1.pdf')


download.file(url, "hd2.pdf", mode = "wb")
txt <- pdf_text("hd2.pdf")

#page 1 is txt[1]
str(txt)
str(txt[1])


processPdfPage <- function(x) {
    dfx <- read.csv(textConnection(x))
    dfx$col1 <- rownames(dfx)
    rownames(dfx) <- NULL
    return(dfx)
    
    }

ldf <- lapply(txt, processPdfPage )
df3 <- data.frame(col1 = ldf[[1]][,2], col2 = ldf[[1]][,1])
df1$col1[5]
df4 <- data.frame(col1 = ldf[[2]][,2], col2 = ldf[[2]][,1])
df <- do.call(cbind, txt)
df <- do.call(read.csv(textConnection), list(txt))


df <- read.csv(textConnection(txt[1]))
df2 <- read.csv(textConnection(txt[2]))
df$col1 <- rownames(df)
rownames(df) <- NULL
dim(df)
df[,1] <- NULL

# drop rows 1:7 from df
df <- slice(df, -(1:7))
df <- slice(df, -11)

library(splitstackshape)
cSplit(df, "col1", sep = " ")

dftt <- data.frame(x = c("x: 123", "y: error: 7"))
dftt
dftt %>% separate(x, c("key", "value"), ": ", extra = "merge")


dfs <- df %>% separate(col1, c("V1", "V2", "V3", "V4", "V5", "V6",
                               "V7", "V8", "V9", "V10", "V11"), 
                       sep = " ", fill = "right")


#### For 2016 Alaska General ##############
df <- read.table("20161108_general_AK_resultsbyprct.txt",
                 sep = "," )


###############          OR     #################

#### For 2014 Alaska General ##############
txt <- readLines("20141104_general_AK_resultsbyprecinct.txt")
txt <- gsub('\\\\"', '""', txt) # note the weird double backslashing because
# `readLines` adds extra backslashes
df <- read.csv(textConnection(txt), stringsAsFactors=FALSE, header = FALSE)
#########################################


####### Start reshaping into desired format ##############

df$V7 <- NULL
df$V5 <- NULL
names(df)
names(df) <- c("precinct", "office", "candidate", "party", "votes")

dfs <- filter(df, 
              !grepl('Number of Precincts|Registered Voters|Times Counted', candidate))

dfs$office <- gsub(" DISTRICT ", " DISTRICT zzzz", dfs$office)

split_df <- dfs %>% separate(office, c("office", "district"), 
                               sep = " zzzz")
dfs <- split_df

dfs$office <- gsub("US PRESIDENT ", "President", dfs$office)
dfs$office <- gsub("US SENATOR ", "U.S. Senate", dfs$office)
dfs$office <- gsub("UNITED STATES SENATOR ", "U.S. Senate", dfs$office)
dfs$office <- gsub("US REPRESENTATIVE ", "U.S. House", dfs$office)
dfs$office <- gsub("HOUSE DISTRICT", "State House", dfs$office)
dfs$office <- gsub("SENATE DISTRICT", "State Senate", dfs$office)
dfs$office <- gsub("GOVERNOR/LT GOVERNOR", "Governor", dfs$office)


dfs$district[dfs$precinct=="HD99 Fed Overseas Absentee "] <- ""
dfs$district[dfs$office=="U.S. House"] <- 1


dfs$district <- as.character(dfs$district)
dfs$district[is.na(dfs$district)] <- ""

dfs$district <- gsub(" ", "", dfs$district)

split_df <- dfs %>% separate(precinct, c("state_legislative_district", "delete"), 
                             sep = "-", remove = FALSE)
split_df$state_legislative_district <- gsub("District ", "", split_df$state_legislative_district)
split_df$state_legislative_district <- gsub(" ", "", split_df$state_legislative_district)
split_df$state_legislative_district <- gsub("HD99FedOverseasAbsentee", "", split_df$state_legislative_district)
split_df$state_legislative_district <- gsub("^0", "", split_df$state_legislative_district)



dfs <- split_df %>% select(state_legislative_district, precinct, office, district, party, candidate, votes )


### write CSV file in desired layout/format for OpenElections github ###

write.csv(dfs, "20141104_ak_general_precinct.csv", row.names=FALSE)

write.csv(dfs, "20161108_ak_general_precinct.csv", row.names=FALSE)
