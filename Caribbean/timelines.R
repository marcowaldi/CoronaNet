library(dplyr)
library(googledrive)
library(googlesheets4)

###########################
## Timeline Automation ###
#########################
                                         

# Run this R script to create update the timelines for your countries
# If you have any questions DM me on Salack (Marco Waldbauer)

# You need to install and load these packages 
# (If you don't have them intalled R will tell you and you can either:
#       - write: install.packages("XYZ")
#       - or on the bottom right of your screen find 'packages' and search for them after clicking 'install')

# Also I'm fully aware that it is possible to automate it even further but the setup takes like 10min so I didn't want to deal with it :D

# Just follow the steps described by the comments starting with a number and you'll be done in no time!


# -------------- Loading in the Data ----------------------------------

# in order to load the data download the data from the CoronaNet Github (here: https://github.com/CoronaNetDataScience/corona_private/tree/master/data/CoronaNet/RA)
# 1. find the file ra_data_pull_purified_all.RDS and download it
# 2. paste the path to the file in the code below (where is says C:/Users/marco/xzy/ra_data_pull_purified_all.RDS)
#    2.1 be careful to use '/' and not '\' 
#    2.2 double check that your path ends with 'ra_data_pull_purified_all.RDS' (or if you renamed it the new name) 
data = readRDS( "C:/Users/marco/xyz/ra_data_pull_purified_all.RDS") 


# filtering out the important columns and renaming the types to fit in the sheet name limit
data = data %>%
  select(country,entry_type, type, date_start, recorded_date, date_end, date_end_spec, description, policy_id, record_id, update_type, ra_name) %>%
  mutate(type = case_when(
    type == "Closure and Regulation of Schools" ~ "Clos. and Reg. of Schools",
    type == "Restriction and Regulation of Businesses" ~ "Rest. and Reg. of Businesses",
    type == "Restriction and Regulation of Government Services" ~ "Rest. and Reg. of Gov. Services",
    type == "New Task Force, Bureau or Administrative Configuration" ~ "New Task Force, etc.",
    is.na(type) ~ "Other Policy Not Listed Above",
    T ~ type
  ))

# The Following step #3 only has to be done once and you're set. After setting the sheets up you only need to run the code 
# 3. Download the 'master_timeline' from GitHub here: https://github.com/marcowaldi/CoronaNet/tree/main/Caribbean
#   3.1 Add the master_timeline to a new Google Drive folder (it should still be an .xlsx file)
#     3.1.1 Open the master_timeline in google drive and convert it to a google sheet (top left -> click 'file' -> 'save as google sheet')
#     3.1.2 You can now delete the original master_timeline.xlsx from the drive folder
#   3.2 Create as many copies (right click -> make copy) of the master_timeline as you need and rename them to the country names
#     !!!! be careful, the names of the sheets need to match the CoronaNet way of spelling them 
#   3.3 You can customize the 'Table of Contents' page with flags or maps whatever you like (just don't change the sheet names)


# 4. Add your Countries here
#   4.1 just replace the countries that are already there and add new ones seperated by ',' and with "" in front and at the end
#     !!!! again: watch out that the spelling here, of the google sheet, and the CoronaNet data is identical
countries = c("Bahamas",
              "Barbados",
              "Liechtenstein",
              "Grenada")


# 5. Go into the Google Drive folder with the timelines, copy the html link and replace the 'link' below with it (be careful to keep the "") 
# In case you have never used the 'googledrive' package you have to authenticate yourself -> just follow the instructions given in the console
drive = drive_ls(path = "https://drive.google.com/drive/folders/crypticfolderid")

# you can check if you have all the sheets you need by removing the '#' below and executing the line that says 'drive'
# drive


# I will not go into detail about the code below because its a bit messy but if you are somewhat familiar with R feel free to customize it
# In case you have never used the 'googledrive' package you have to authenticate yourself -> just follow the instructions given in the console
# Don't Worry about the timeouts (it looks like the code get's stuck on a certain type) (you can only access google sheets a certain amount per minute)
# Just let the code run and wait a bit! The code is running as long as there is a red stop sign on the top right of the console
for (c in 1:length(countries)) {
  print("_________________________________________________")
  print(countries[c])
  print("________________________")
  if (countries[c] %in% drive$name) {
  print_country = data %>% 
    filter(data$country == countries[c])
  
  # information to be displayed on the 'table of contents' page [G2-J2] feel free to customize it
  last_info = print_country[which.max(print_country$recorded_date),]
  last_info2 = print_country[which.max(print_country$date_start),]
  last_ra = as.data.frame(last_info$ra_name)
  last_policy = as.data.frame(as.Date(last_info$recorded_date))
  num_pol = as.data.frame(nrow(print_country))
  latest_record = as.data.frame(as.Date(last_info2$date_start))
  
  country_info = cbind(last_ra, last_policy, num_pol, latest_record)
  
  
  id = paste(drive[which(drive[,1] == countries[c]),2])
  suppressMessages(range_write(id, country_info, sheet = "Table of Contents", range = "G2", col_names = F))
  types = unique(data$type)
  
  for (t in 1:length(types)) {
    if (types[t] %in% print_country$type) {
      print(types[t])
      temp = print_country %>%
        filter(type == types[t]) %>%
        group_by(policy_id) %>%
        arrange(date_start, date_end, entry_type, .by_group = T) %>%
        ungroup() %>%
        select(entry_type, date_start, date_end, date_end_spec, description, policy_id, record_id, update_type)
      
      suppressMessages(range_flood(id, sheet =  paste(types[t]), range = "A3:J250", reformat = FALSE))
      suppressMessages(range_write(id, temp, sheet = paste(types[t]), range = "A3", col_names = FALSE))
    } else { next}
  }
  } else {
    print("!!! COUNTRY SHEET NOT FOUND !!!")
    next}
}
