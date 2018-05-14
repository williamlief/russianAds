source("/Users/williamlief/Documents/Research Projects/RussianAds/Code/libs_and_paths.R")

setwd(russia_raw) # set the working directory to where the raw data is stored
# goal: one row per file with variables for each of the entries identified above

# Create a list of the words that start the data entries - these will form the variable names
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
             "Ad End Date",
             "Redactions Completed" # this is the start of footer text that I can get rid of
             )) 
names(vars) <- "vars"

# Create the parsing function
pdf_parse <- function(file) {
    # Summary - data is in long form with one row per line in the pdf 
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
      filter(vars!="Redactions_Completed"|is.na(vars)) %>% 
      # mutate(var_n = row_number()) %>% 
      # filter(
      #   !(var_n!=1 & vars %in% c("Ad_End_Date","Ad_Creation_Date"))) %>% # Ad_End_Date is sometimes missing, these dont seem to ever be split over rows, and one of them should always be the last line
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

# now that the heavy lifting is done from the external disk we will swap back to the main research folder
setwd(russia) 

# clean up the ids that got mis-parsed,
# clean up the number variables. Replace O with 0, strip out commas and RUB 
  # some commas are misread as periods, Clicks and Impressions appear to be integers so I can just strip non-digits out
  # BUT spend is in decimal form. If I strip periods I will inflate by a factor of 100. Does everything have a decimal?
  # Visual inspection suggests that there is always a decimal followed by two numbers (sometimes I see 0.00). 
  # SO, I can just strip all non digits from the ad spend and then divide by 100. 
  # Also, convert 'None' to 0
  # Should confirm the largest spend values
# format the date variables
  # the time part ends up being a lot messier with more bad data reading
  # so for now I am just parsing the date part. There are some erroneous spaces before or after the / so I get rid of those
  # doing one manual correction where a / was read as 1. 

dat2 <- dat %>% 
  mutate(Ad_ID = ifelse(is.na(Ad_ID),substr(`<NA>`,8,11),Ad_ID),
         fname = sub("rawdata/","",fname),
         Location = ifelse(is.na(Ad_Targeting_Location),`Ad_Targeting_Location_-_Living_In`,Ad_Targeting_Location),
         Clicks      = as.integer(gsub("[^[:digit:]]", "",gsub("O",0,Ad_Clicks))), # triple nest - replace O with 0, keep only digits, make integer
         Impressions = as.integer(gsub("[^[:digit:]]", "",gsub("O",0,Ad_Impressions))), # same as above
         Spend_RUB =      as.numeric(gsub("[^[:digit:]]", "",gsub("None",0,Ad_Spend)))/100,
         create1 = substr(gsub(" /|/ ", "/",Ad_Creation_Date),1,8), # strip the extra space, then pull the first 8 chars
         create1 = ifelse(create1 == "02/21117", "02/21/17", create1), # this is one manual correction
         Create_Date = parse_date(create1, "%m/%d/%y"),  # convert to a date format
         end1 = substr(gsub(" /|/ ", "/",Ad_End_Date),1,8), # same again, no manual correction needed this time
         End_Date = parse_date(end1, "%m/%d/%y")) %>% 
  rename(Custom_Audience= Ad_Targeting_Custom_Audience) 

# pull in the exchange rate data
forex <- read_csv("clean/exchangerates.csv") %>% 
  select(Create_Date = `YYYY/MM/DD`,
         forex = `RUB/USD`)

dat3 <- dat2 %>% 
  full_join(forex) %>% 
  arrange(Create_Date) %>% 
  mutate(forex=zoo::na.locf(forex, na.rm=FALSE),
         Spend_USD = round(Spend_RUB/forex,2)) %>%  # we dont have exchange rates for everyday, when missing, take the most recent exchange rate
  filter(!is.na(Ad_ID)) # get rid of the forex that was on dates w.o. ads

# sort the variables, order the data
dat4 <- dat3 %>% 
  select(fname, Ad_ID, Clicks, Impressions, Spend_RUB, Spend_USD, Create_Date, End_Date, Ad_Text, Ad_Landing_Page, 
         Location, Age, Gender, Excluded_Connections, Language, Placements, People_Who_Match, Interests, Custom_Audience,
         txt_Clicks = Ad_Clicks, txt_impressions = Ad_Impressions, txt_spend = Ad_Spend, txt_create = Ad_Creation_Date, txt_end = Ad_End_Date) # these are left in so we can compare the formatted extractions to the text string


# save the data
write_rds(dat4,"clean/fb_russia_ads_clean.rds") 
write_csv(dat4,"clean/fb_russia_ads_clean.csv")



# debugging/problem notes

# Known problem files
# 2016-q1/2016-02/P(1)0006506.pdf # redaction
# 2016-q2/2016-04/P(1)0000090.pdf # redaction
# 2016-q2/2016-04/P(1)0000106.pdf # redaction
# 2016-q2/2016-04/P(1)0000112.pdf # redaction
# 2016-q2/2016-04/P(1)0002335.pdf # redaction
# 2016-q2/2016-04/P(1)0002347.pdf # redaction
# 2015-q3/2015-07/P(1)0001918.pdf - Dates were on page 2, didnt get parsed in  it looks like




# some debugging lines to look at individual pdf files
# file <- "2016-q2/2016-04/P(1)0003303.pdf"
# #test <- filter(dat, fname== file)
# system(paste0("open ",'"/Volumes/SSD/DataHouse/RussianAds/rawdata/',file,'"')) # system is passing the command outside R into the terminal, this opens the actual pdf file up. Set to work on mac, not sure how Windows/Linus will react
# txt <- pdf_text(paste0("rawdata/",file))
# df <- read_table(txt, col_names = F) %>% select(line=X1) %>% 
#   regex_left_join(vars, by = c(line = "vars"))

