library(pdftools)
library(fuzzyjoin)
library(tidyverse)

setwd("/Users/williamlief/Downloads")

# goal: one row per file with variables for each of the entries identified above

# Create a list of the variables
vars <- tibble(c("Ad ID",
             "Ad Text",
             "Ad Landing Page",
             "Ad Targeting Location:",
             "Ad Targeting Location - Living In:",
             "Ad Targeting Custom Audience:",
             "Interests:",
             "Excluded Connections:",
             "Age:",
             "Gender:",
             "Language:",
             "Placements:",
             "Placements", # 26 files look normal as PDFs but something is funky with how the colon is encoded and it isnt reading, breaking the fuzzy merge - see ./2015-06/P(1)0002132.pdf
             "People Who Match",
             "Ad Impressions",
             "Ad Clicks",
             "Ad Spend",
             "Ad Creation Date",
             "Ad End Date"))
names(vars) <- "vars"

# Create the parsing function
pdf_parse <- function(file) {
    # Summary - data is in long form with one row per line in the pdf and one variable containing the text from that line.
    # 1) create a column for variable names and a column with the variable contents
    # 2) Long lines are split, need to take a line that doesn't start with a variable name and push it back into the previous line
    #       Also, there's some random footer junk getting imported that I want to get rid of
    # 3) convert the data to wide form
    
    print(file)

    txt <- pdf_text(file) # read data
  
    # use a fuzzy join to merge the variable names onto the raw data, split the name from the data
    df <- read_table(txt, col_names = F) %>% select(line=X1) %>% 
      regex_left_join(vars, by = c(line = "vars")) %>%  # this is matching the variable name to the beginning of the text string on a given line
      mutate(content= str_sub(line, str_length(vars)+2,str_length(line)), # Split out the data from the variable names
             vars = gsub(":","",vars), # clean up the variable name formatting
             vars = gsub(" ","_",vars)) 
    
    #  Reconnect the rows that got split and push the text together to a single line, also get rid of the useless footer text
    df2 <- df %>% 
      mutate(vars=zoo::na.locf(vars, na.rm=FALSE)) %>% # connect together the variables that were split across multiple lines
      group_by(vars) %>%  # but get rid of the useless footer data
      mutate(var_n = row_number()) %>% 
      filter(!(var_n!=1 & vars %in% c("Ad_End_Date","Ad_Creation_Date"))) %>% # Ad_End_Date is sometimes missing, these dont seem to ever be split over rows, and one of them should always be the last line
      mutate(content=ifelse(is.na(content),line,content)) %>% # fill the missing content with the line data
      summarize_(content=~paste(content, collapse=' ')) # collapse to a single line by var, annoyingly this also sorts by var
    
    # Convert from long to wide - that is make a single row of data with the vars as the column names
    df3 <- df2 %>% 
      spread(vars, content) %>% 
      mutate(fname = file) # save the name of the file the data was extracted from
    
  return(df3)
}

# create a list of all the files
files <- list.files(path = "rawdata", full.names = TRUE, recursive = TRUE)

# read all the files and stack them togeth up
dat <- lapply(files,pdf_parse) %>% bind_rows

# clean up the ids that got mis-parsed, reorder the variables
dat2 <- dat %>% 
  mutate(Ad_ID = ifelse(is.na(Ad_ID),substr(.[[17]],8,11),Ad_ID),
         fname = sub("rawdata/","",fname),
         Location = ifelse(is.na(Ad_Targeting_Location),`Ad_Targeting_Location_-_Living_In`,Ad_Targeting_Location)) %>% 
  select(fname,Ad_ID, contains("AD_"), everything(), 
         -c(`<NA>`, Ad_Targeting_Location,`Ad_Targeting_Location_-_Living_In`))

# save the data
write_csv(dat2,"fb_russia_ads_clean.csv")

test <- read_csv("fb_russia_ads_clean.csv")?

# problems with the id
# 2016-q1/2016-02/P(1)0006506.pdf # redaction
# 2016-q2/2016-04/P(1)0000090.pdf # redaction
# 2016-q2/2016-04/P(1)0000106.pdf # redaction
# 2016-q2/2016-04/P(1)0000112.pdf # redaction
# 2016-q2/2016-04/P(1)0002335.pdf # redaction
# 2016-q2/2016-04/P(1)0002347.pdf # redaction
# 2015-06/P(1)0005957.pdf # unknown
# 2015-q4/2015-10/P(1)0006173.pdf # unknown
# 2016-q2/2016-05/P(1)0005959.pdf # unknown
# 2016-q2/2016-05/P(1)0005963.pdf
# 2016-q2/2016-05/P(1)0005965.pdf
# 2016-q2/2016-05/P(1)0005967.pdf
# 2016-q2/2016-05/P(1)0005969.pdf
# 2016-q2/2016-05/P(1)0005973.pdf
# 2016-q2/2016-05/P(1)0005975.pdf
# 2016-q2/2016-05/P(1)0005977.pdf
# 2016-q2/2016-05/P(1)0005979.pdf
# 2016-q2/2016-05/P(1)0006017.pdf
# 2016-q2/2016-05/P(1)0006025.pdf
# 2016-q2/2016-05/P(1)0006051.pdf
# 2016-q2/2016-05/P(1)0006057.pdf
# 2016-q2/2016-05/P(1)0006059.pdf
# 2016-q2/2016-05/P(1)0006067.pdf
# 2016-q2/2016-05/P(1)0006083.pdf
# 2016-q2/2016-05/P(1)0006085.pdf
# 2016-q2/2016-05/P(1)0006091.pdf
# 2016-q2/2016-05/P(1)0006095.pdf
# 2016-q2/2016-05/P(1)0006097.pdf
# 2016-q2/2016-05/P(1)0006127.pdf

# some debugging lines
file <- "2016-q2/2016-05/P(1)0006025.pdf"
system(paste0("open ",'"/Users/Williamlief/Downloads/rawdata/',file,'"'))
txt <- pdf_text("rawdata/2016-q2/2016-04/P(1)0000090.pdf")
df <- read_table(txt, col_names = F) %>% select(line=X1) %>% 
  regex_left_join(vars, by = c(line = "vars"))

