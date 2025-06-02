#### UPDATED - Water Quality Data for NMDS Analysis ####
## Script for summarizing SSI water quality data across 30-, 90-, 180-, and 365-day timescales
## For use in Emma's BMI data research project
## A few parts adapted from Emma Walker's Annual WQ Analysis.R script
## Updated to account for exact biological data sampling dates.

## Authors: Jessica Herrmann
## Date created: 5/12/25
## Date last edited: 5/12/25

#### Package libraries ####
library(dplyr)
library(tidyr)
library(reshape2)
library(lubridate)
library(roll)
library(zoo) # for rolling means & imputations
library(curl)
library(tidyquant)
library(dataRetrieval) # for calculating USGS water years
# library(mice) # for imputations
# library(VIM) # for visualizing missing data

#### Load data ####
## WQ dataset from Airtable
WQ_data <- curl("https://raw.githubusercontent.com/jnherrmann/BMI-Climate-Flow-WQ-Project/refs/heads/main/2000_2024_QW_data_corrected.csv")
WQ_data <- read.csv(WQ_data, header = T, sep = ",")

## Site codes cheat sheet, for converting back to old codes
site_codes <- curl("https://raw.githubusercontent.com/jnherrmann/SSI-metadata/refs/heads/main/SSI_Monitoring%20_Sites%20_Metadata.csv")
site_codes <- read.csv(site_codes, header = T, sep = ",")
site_codes <- site_codes[c(1:10, 12:30, 34:35),]
site_codes <- site_codes[, c(2:3)]

## BMI data sample dates (project-specific, not an exhaustive list)
bmi_dates <- curl("https://raw.githubusercontent.com/jnherrmann/BMI-Climate-Flow-WQ-Project/refs/heads/main/BMIdates_WY2003_2022_updated.csv")
bmi_dates <- read.csv(bmi_dates, header = T, sep = ",")

#### Clean WQ data ####
# Convert back to old site codes, for Emma's project
WQ_data <- left_join(WQ_data, site_codes, by = c('Site.ID' = 'New_site_code'))
colnames(WQ_data)[109] <- "Site_code" # to match BMI dataset column name

# Replace nutrient values under detection limits with NA 
## From 2000 to 2020: NO3 = <0.1 mg/L ; PO4 = <0.05 mg/L 
## From 2021 to Present: NO3 = <0.23 mg/L ; PO4 = <0.06mg/L

# Replaces Nitrate values with NA
WQ_data$NO3.final <- ifelse(WQ_data$Sample.Year <= 2020 & WQ_data$NO3.Average == "0.1",NA,
                            ifelse(WQ_data$Sample.Year >= 2021 & WQ_data$NO3.Average == "0.23",
                                   NA,WQ_data$NO3.Average)) 

# Replaces Phosphate values with NA
WQ_data$PO4.final <- ifelse(WQ_data$Sample.Year <= 2020 & WQ_data$PO4.Average == "0.05",NA,
                            ifelse(WQ_data$Sample.Year >= 2021 & WQ_data$PO4.Average == "0.06",
                                   NA,WQ_data$PO4.Average))

# Replace bacteria values under detection limits with NA (< 1 MPN)
# Need to replace values recorded as < 1MPN, but keep those = 1MPN

WQ_data$Bacteria_EColi.final <- ifelse(WQ_data$Bacteria_EColi.Qualifier == "<",NA,
                                       WQ_data$Bacteria_EColi)

# Subset by columns of interest - can modify this later
# names(WQ_data)
WQ_data <- WQ_data[,c(109,10,6:7,25,41,51,56,63,72:73,84:85,89:92)]

# Note: check to make sure the column numbers are correct                                    
# Add column for month
WQ_data$Date <- as.Date(WQ_data$Date, format = "%m/%d/%Y")

# Add column for water year
WQ_data$Date <- format(WQ_data$Date, format="%m/%d/%y")
WQ_data$Date <- mdy(WQ_data$Date) # in year-month-day format

WQ_data <- WQ_data %>%
  mutate(WY = calcWaterYear(WQ_data$Date)) # USGS water year

# Subset to years of interest, to match the 2021 - 2022 BMI dataset (WY 2019 - 2023)
WQ_subset <- subset(WQ_data, WY %in% c(2001:2023))                              # 4415 unique dates within our dataset, excluding days without data

# write.csv(WQ_data,"C:\\Users\\jessicaherrmann\\Downloads\\AllWQ.csv")

## For now, I'm going to drop the sites with missing site codes.
unique(WQ_subset$Site_code) # n = 32
WQ_subset <- WQ_subset[!is.na(WQ_subset$Site_code),]
unique(WQ_subset$Site_code) # n = 31                                            # Now 4395 unique days of data (several entries had "NA"/missing site codes)


#### Create template dataframe to pair with our WQ dataset ####
# Check for missing days, if so, add NA rows:
for (i in 1:length(WQ_subset$Site_code)){
  if(as.numeric(diff(range(WQ_subset$Date))) != (nrow(WQ_subset)+1)){
    fullDates <- seq(from = min(WQ_subset$Date),
                     to = max(WQ_subset$Date), by = "1 day")
    fullDates <- data.frame(Date = fullDates,
                            stringsAsFactors = FALSE)
  }}                                                                              # 8265 dates, including days without data

# Template df containing all unique collection dates in our timeframe
# Each date is repeated n = 31 times, once per each site
num_sites <- 31
fullDates <- fullDates[rep(seq_len(nrow(fullDates)), each = num_sites), ]       # We have 31 (discounting NA's) unique sites represented in the WQ df

# Second template df for each site
fullSites <- as.data.frame(unique(WQ_subset$Site_code))

# Sequentially duplicate each unique site code per unique date
# Appending sequential site list to itself n = 7965 times
num_dates <- 8265
sites_list <- replicate(num_dates, fullSites, simplify = FALSE)
fullSites <- do.call(rbind.data.frame, sites_list)
# head(fullSites)

# Now merge two df's
sitesDates <- cbind(fullDates, fullSites)                                       # n = 256215 = 31*8265
# View(sitesDates)

# Rename columns
colnames(sitesDates)[1] <- "Date"
colnames(sitesDates)[2] <- "Site_code"
# View(sitesDates)

# Merge template df with WQ df, sorting by date
WQ_subset2 <- full_join(sitesDates, WQ_subset,
                        by = c("Date", "Site_code")) %>%
  arrange(Site_code)                                                            # Great! We still have n = 256215 (no weird duplicates)

# Adding/formatting dates in new WQ df
WQ_subset2$Date <- as.Date(WQ_subset2$Date, format = "%Y/%m/%d")
WQ_dates <- parse_date_time(WQ_subset2$Date, "ymd")
WQ_subset2 <- WQ_subset2 %>%
  mutate(WY = calcWaterYear(WQ_subset2$Date))                                   # USGS water year
# Keeping these new date columns as "num" for now. Can reformat later.

# First remove "Qualifier" columns for Bacteria
WQ_subset3 <- WQ_subset2[,c(1:9, 11, 13, 15, 17:18)]



#### Rolling averages and data filtering ####

## Calculate rolling averages for all WQ parameters for specified time intervals
WQ_rollmeans <- WQ_subset3 %>%
  group_by(Site_code) %>%
  tq_mutate(select = DO.Average, mutate_fun = rollapply, width = 30, align = "right", FUN = mean, na.rm = TRUE, col_rename = "DO30") %>%
  tq_mutate(select = DO.Average, mutate_fun = rollapply, width = 90, align = "right", FUN = mean, na.rm = TRUE, col_rename = "DO90") %>%
  tq_mutate(select = DO.Average, mutate_fun = rollapply, width = 180, align = "right", FUN = mean, na.rm = TRUE, col_rename = "DO180") %>%
  tq_mutate(select = DO.Average, mutate_fun = rollapply, width = 365, align = "right", FUN = mean, na.rm = TRUE, col_rename = "DO365") %>%
  tq_mutate(select = Cond.Average, mutate_fun = rollapply, width = 30, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Cond30") %>%
  tq_mutate(select = Cond.Average, mutate_fun = rollapply, width = 90, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Cond90") %>%
  tq_mutate(select = Cond.Average, mutate_fun = rollapply, width = 180, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Cond180") %>%
  tq_mutate(select = Cond.Average, mutate_fun = rollapply, width = 365, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Cond365") %>%
  tq_mutate(select = H2OTemp.Average, mutate_fun = rollapply, width = 30, align = "right", FUN = mean, na.rm = TRUE, col_rename = "H2OTemp30") %>%
  tq_mutate(select = H2OTemp.Average, mutate_fun = rollapply, width = 90, align = "right", FUN = mean, na.rm = TRUE, col_rename = "H2OTemp90") %>%
  tq_mutate(select = H2OTemp.Average, mutate_fun = rollapply, width = 180, align = "right", FUN = mean, na.rm = TRUE, col_rename = "H2OTemp180") %>%
  tq_mutate(select = H2OTemp.Average, mutate_fun = rollapply, width = 365, align = "right", FUN = mean, na.rm = TRUE, col_rename = "H2OTemp365") %>%
  tq_mutate(select = pH.Average, mutate_fun = rollapply, width = 30, align = "right", FUN = mean, na.rm = TRUE, col_rename = "pH30") %>%
  tq_mutate(select = pH.Average, mutate_fun = rollapply, width = 90, align = "right", FUN = mean, na.rm = TRUE, col_rename = "pH90") %>%
  tq_mutate(select = pH.Average, mutate_fun = rollapply, width = 180, align = "right", FUN = mean, na.rm = TRUE, col_rename = "pH180") %>%
  tq_mutate(select = pH.Average, mutate_fun = rollapply, width = 365, align = "right", FUN = mean, na.rm = TRUE, col_rename = "pH365") %>%
  tq_mutate(select = Turb.Average, mutate_fun = rollapply, width = 30, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Turb30") %>%
  tq_mutate(select = Turb.Average, mutate_fun = rollapply, width = 90, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Turb90") %>%
  tq_mutate(select = Turb.Average, mutate_fun = rollapply, width = 180, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Turb180") %>%
  tq_mutate(select = Turb.Average, mutate_fun = rollapply, width = 365, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Turb365") %>%
  tq_mutate(select = NO3.Average, mutate_fun = rollapply, width = 30, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Nitrate30") %>%
  tq_mutate(select = NO3.Average, mutate_fun = rollapply, width = 90, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Nitrate90") %>%
  tq_mutate(select = NO3.Average, mutate_fun = rollapply, width = 180, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Nitrate180") %>%
  tq_mutate(select = NO3.Average, mutate_fun = rollapply, width = 365, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Nitrate365") %>%
  tq_mutate(select = PO4.Average, mutate_fun = rollapply, width = 30, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Phosphate30") %>%
  tq_mutate(select = PO4.Average, mutate_fun = rollapply, width = 90, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Phosphate90") %>%
  tq_mutate(select = PO4.Average, mutate_fun = rollapply, width = 180, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Phosphate180") %>%
  tq_mutate(select = PO4.Average, mutate_fun = rollapply, width = 365, align = "right", FUN = mean, na.rm = TRUE, col_rename = "Phosphate365") %>%
  tq_mutate(select = Bacteria_TC, mutate_fun = rollapply, width = 30, align = "right", FUN = mean, na.rm = TRUE, col_rename = "TotColiform30") %>%
  tq_mutate(select = Bacteria_TC, mutate_fun = rollapply, width = 90, align = "right", FUN = mean, na.rm = TRUE, col_rename = "TotColiform90") %>%
  tq_mutate(select = Bacteria_TC, mutate_fun = rollapply, width = 180, align = "right", FUN = mean, na.rm = TRUE, col_rename = "TotColiform180") %>%
  tq_mutate(select = Bacteria_TC, mutate_fun = rollapply, width = 365, align = "right", FUN = mean, na.rm = TRUE, col_rename = "TotColiform365") %>%
  ungroup()

## Create an identifier column (site and collection date in YYYYmmdd format)
WQ_rollmeans <- subset(WQ_rollmeans, Site_code %in% c("DC 1", "DC 2", "DC 4", "DC 6", "DC 8", "DC 9", "DC 11", "DC 12", "DC 13", "DC 15", "DC 16"))
WQ_rollmeans$Site_code <- gsub("DC ", "", WQ_rollmeans$Site_code)
colnames(WQ_rollmeans)[1] <- "Site"

# Site.Date is the new identifier column
WQ_rollmeans$Site.Date <- paste(WQ_rollmeans$Site, WQ_rollmeans$Date, sep = "_")

# Adding this identifier to the "BMI dates" dataset as well (this will be our join key)
bmi_dates$Site.Date <- paste(bmi_dates$Site, bmi_dates$Date, sep = "_")
bmi_dates2 <- as.data.frame(bmi_dates[,c(5)])
colnames(bmi_dates2)[1] <- "Site.Date"

## Filter by identifiers matching BMI dataset
WQ_final <- left_join(bmi_dates2, WQ_rollmeans, by = "Site.Date")
WQ_final$Month <- format(WQ_final$Date, "%m")
WQ_final$Year <- format(WQ_final$Date, "%Y")
WQ_final <- WQ_final[,c(1,3,2,48,49,4:47)]

WQ_final <- WQ_final %>%
  mutate(WY = calcWaterYear(WQ_final$Date))
WQ_final <- WQ_final[,c(1:5,17,6:49)]

WQ_final$Site <- as.integer(WQ_final$Site)
WQ_final$Month <- as.integer(WQ_final$Month)
WQ_final$Year <- as.integer(WQ_final$Year)

# Seasonal variable
WQ_final$Season <- as.factor(ifelse(WQ_final$Month %in% c(5,6,7), "Peak", "Base"))
WQ_final <- WQ_final[,c(1:6,51,7:17,19:50)]          

#### Write to CSV ####
write.csv(WQ_final, "C:\\Users\\jessicaherrmann\\Desktop\\WQ_final.csv")
