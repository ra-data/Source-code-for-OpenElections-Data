rm(list=ls())
library(dplyr)
library(tidyr)
library(pdftools)


# remove unwanted rows based on keyword strings

delete_rows_by_strings <- function(fdf, DF_FILTER_STRINGS) {
    fdf <- fdf %>%
        filter(!grepl(paste(DF_FILTER_STRINGS, collapse="|"), V1))
    return(fdf)
}

standardize_office_name <- function(fdf) {
    fdf$office <- gsub("U.S. House of Representatives", "U.S. House", fdf$office)
    fdf$office <- gsub("Tennessee Senate ", "State Senate", fdf$office)
    fdf$office <- gsub("Tennessee House of Representatives ", "State House", fdf$office)
    fdf$office <- gsub("United States ", "", fdf$office)
    return(fdf)
}


standardize_party_name <- function(fdf) {
  fdf$party <- gsub("\\(D\\)", "DEM", fdf$party)
  fdf$party <- gsub("- \\(R\\)", "REP", fdf$party)
  fdf$party <- gsub("\\(R\\)", "REP", fdf$party)
  fdf$party <- gsub("\\( R\\)", "REP", fdf$party)
  fdf$party <- gsub("\\(I\\)", "IND", fdf$party)
  fdf$party <- gsub("\\(D\\)", "DEM", fdf$party)
  fdf$party <- gsub("\\(R\\)", "REP", fdf$party)
  fdf$party <- gsub("\\(I\\)", "IND", fdf$party)  
  
  fdf$party[is.na(fdf$party)] <- "OTH"
  return(fdf)
}


find_rows_with_string <- function(string, start_indicator, fdf) {
    #' #######____________________________________________##########
    #' Next, we need to demarcate Counties.
    
    #' @param start_indicator = TRUE (starting point of new segment)
    #' if FALSE, it is the LAST row of the previous segment associated with the string
    #' 
  C_string_rows <- grep(string, fdf$V1)
  if(start_indicator==TRUE){
    C_runlength <- c(C_string_rows[1]-1,
                     diff(C_string_rows),
                     nrow(fdf)-C_string_rows[length(C_string_rows)]+1
    )
    return(list(c(1,C_string_rows), C_runlength))
  } else{ # the string denotes a segment END
    C_runlength <- c(C_string_rows[1],
                     diff(C_string_rows)
                     #,nrow(df)-C_string_rows[length(C_string_rows)]+1
    )
    return(list(C_string_rows, C_runlength))
  }
  

}


create_long_column_vector <- function(Cnames, Crunlength) {
    #' @param Cnames is the list of names to be added in the new column in df
    #' @param Crunlength gives number of rows for each element of Cnames
    #' @return value is of same length as df (Cnames*Crunlength)  
  value <- NULL
  for (cv in 1:length(Cnames)){
      print(cv)
      print(Cnames[cv])
      print(paste(cv, Crunlength[cv]))
      value <- append(value, 
                      rep(Cnames[cv], Crunlength[cv]))
  }
  return(value)
}


add_column_to_dataframe_based_on_string <- function(fdf, demarc_str, start_indicator, newcol_name) {
    message(newcol_name)
    message(demarc_str)
    newcolmeta <- find_rows_with_string(demarc_str, start_indicator = T, fdf)
    start_rows <- newcolmeta[[1]]
    row_lengths <- newcolmeta[[2]]
    message(row_lengths)
    C_names <- fdf[start_rows,"V1"]
    C_names <- gsub("^ *", "", C_names) #remove leading blank spaces
    fdf[ ,newcol_name] <- create_long_column_vector(C_names, row_lengths)
    return(fdf)
}


add_3columns_to_dataframe <- function(df, demarc_race_str, 
                                      demarc_county_str, demarc_page_str) {
    #Create a new column in df with RACE
    df <- add_column_to_dataframe_based_on_string(df, demarc_race_str, T, "Race" )
    
    #Create a new column in df with COUNTY NAMES
    df <- add_column_to_dataframe_based_on_string(df, demarc_county_str, T, "County" )
    #C_names[1] <- "START"
    
    #Create a new column in df with Page 
    df <- add_column_to_dataframe_based_on_string(df, demarc_page_str, F, "Page" )
    return(df)
}


create_precinct_name <- function(precnt, s_tkn) {
    group1 <- 1:precnt
    precinct_name <- paste(s_tkn[group1], collapse = " ")
    return(precinct_name)
}


create_vote_countStr <- function(precnt, s_tkn) {
    group1 <- (precnt+1):length(s_tkn)
    vote_str <- paste(s_tkn[group1], collapse = " ")
    return(vote_str)
}


create_list_candidates_and_numbers <- function(cand_df) {
    c2 <- cand_df %>%
        group_by(district) %>%
        mutate(
            cand_cols = ceiling(num_cand/numrows_col1),
            #calculate the candidate number to split by
            split_value = row_number() + numrows_col1,
            # some rows don't need to be split since they contain only one name
            split_value = ifelse(split_value > num_cand, 
                                 0,
                                 split_value)
        )

    
    #we know the value to split by, but it is different for each row.
    sep_c2 <- c2 %>%
        select(-c(num_cand, numrows_col1, cand_cols)) %>%
        rowwise() %>%
        #this is a trick to first use gsub and then to separate
        #' since separate cannot take variable sep.
        mutate(
            V2 = gsub(paste0(split_value," \\."),
                      "ZsepZ", 
                      V1)
        ) %>%
        separate(col=V2,                       
                 into=paste0("Sep",1:2),
                 sep="ZsepZ",
                 remove=T
        )
    
    melt_c2 <- sep_c2 %>%
        #melt based on COLUMNS Sep1 & Sep2
        gather(key, candidate,  -district, -split_value, -V1) %>%
        select(-key) %>% #we don't need the key column
        filter(!is.na(candidate)) 
    
    c1 <- melt_c2 %>%
        # pull out the number and name of the candidate
        extract(candidate, into=c("num","Name"),
                "(\\d) \\.(.*)",
                remove = F
        ) 
    
    clean_cand <- c1 %>%
        mutate( #Sometimes comes from candidate colm, sometimes from split_Value
            Number = ifelse(is.na(num), split_value, num),
            Candidate = ifelse(is.na(num), candidate, Name)
        ) %>%
        #keep the columns that we need
        select(V1, district, Number, Candidate)
    
    return(clean_cand)
}


main <- function() {
  
  if (!file.exists(filename)) {
      download.file(URL, filename, mode = "wb")
  }
  txt <- pdf_text(filename)
  
  #' Store the whole pdf in one dataframe of 1 column
  df <- read.csv(textConnection(txt), sep="\n", header=F,
                 stringsAsFactors = F)
  
  
  df <- add_3columns_to_dataframe(df, demarc_race_str, 
                            demarc_county_str, demarc_page_str)
  
  # rmString1, rmString2 have been defined as config parameters
  df$V1 <- gsub("^ *", "", df$V1) #remove leading blank spaces
  df <- df %>% filter(!grepl(rmString1, V1)) %>%
      filter(!grepl(rmString2, V1))
  
  ##------------------------------------
  ## Find out how many candidates per Race.
  ## logic for num_cand is based on number of columns for vote counts
  ## example, searching for row before "COUNTY" and see 1 2 3 4...and take max
  ## logic for numrows_col1 is based on count of rows between race name & vote count  column headers
  a <- df %>%
    group_by(Race) %>%
    mutate(key = grep("COUNTY", V1)[1]-1,   #row prior to first match
           num_cand =   as.numeric(max(unlist(strsplit(V1[key], split="")),
                      na.rm=T)),
           numrows_col1 = key - CANDIDATE_BLK_EXTRA_LINES, #
           diff = (num_cand == numrows_col1)  # catch where num of candidates
                            # is diff from extra rows between race & vote headers
           ) %>%
      select(-key)
  
  
  # create new column with tokens of V1 separated by 1-or-more spaces
  a$tokens <- strsplit(a$V1, split = "\\s+")
  
  

  a <- a %>% rowwise() %>% 
      mutate(le = length(tokens), 
             precinct_token_count = as.numeric(ifelse(le <= num_cand, le, le-num_cand)),
             precinct_n = create_precinct_name(precinct_token_count, tokens),
             vote_countStr = create_vote_countStr(precinct_token_count, tokens)
             )
  
  # split vote count string into individual candidate vote count columns
  max_candidates <- max(a$num_cand)
  candidate_index_headers <- paste("Z", 1:max_candidates, sep = "_")
  b <- a %>% 
      separate(vote_countStr, 
               into = candidate_index_headers,
               sep = " ")
  
  # split Race column into office and district columns
  b <- b %>% separate(Race, c("office", "district"), 
                      sep = district_separtor_str)
  
  # create new df with names of candidates by district
  c2 <- candidate_list
  candidate_list <- b %>% 
      group_by(district) %>% 
      slice(2:(numrows_col1 + 1)) %>% 
      select(V1, district, num_cand, numrows_col1)
  
  
  clean_cand <- create_list_candidates_and_numbers(candidate_list)  
  
  candidate_list1 <- clean_cand %>% 
      separate(Candidate, c("Candidate", "party"), 
               sep = " . ") %>% 
      unite(dist_cand, district, Number, 
            sep = "_Z_", remove = TRUE)
  
  
  df2 <- standardize_office_name(b)
  
  # remove undesired rows based on keywords specified as config parameters
  df2 <- delete_rows_by_strings(df2, DF_FILTER_STRINGS)
  
  #remove candidate name block rows appearing on each district and page
  df2 <- df2 %>% group_by(district, Page) %>% 
      slice(-(1:(max(numrows_col1) + CANDIDATE_BLK_EXTRA_LINES))) 
  
  # melting df
  # using underscore function of gather which uses string input for column names
  long_DF <- df2 %>% 
      gather_("candidate", "votes", paste0("Z_", 1:max_candidates))
  
  
  long_DF <- long_DF %>%
      unite(dist_cand, district, candidate, sep ="_", remove = TRUE)
  
  final <- merge(x = long_DF, y = candidate_list1, by = "dist_cand", all.x = TRUE) %>%
      select(dist_cand, County, office, precinct_n, votes, Candidate, party)
  final$County <- gsub("COUNTY: ", "", final$County)
  
  final <- final %>% filter(County != precinct_n) %>% 
      filter(!is.na(Candidate)) %>%
      separate(dist_cand, c("district", "del", sep = "_Z_")) %>%
      select(County, precinct_n, office, district, party, Candidate, votes)
  
  unique(final$party)
  final <- standardize_party_name(final)
  
  final$votes <- as.numeric(gsub(",", "", final$votes))
                      
  names(final) <- c("county", "precinct", "office", "district", "party", 
                  "candidate", "votes")
  final$district[is.na(final$district)] <- ""
  final$district <- gsub("NA", "", final$district)
  # final <- final %>% filter(!grepl("STATEWIDE TOTALS", precinct))

  write.csv(final, newfilename, row.names=FALSE)

  if(sum(is.na(final))){
      message("NA is found in final data frame")
      message("check ", newfilename )
  }
  
}




