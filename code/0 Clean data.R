# ---
# Eat Out to Help Out
# Output: Process data for analysis of firm registrations.
# November 2021
# Contact: jph37@kent.ac.uk; i.galanakis@kent.ac.uk
# ---

# Load libraries ----
library('lubridate')
library('dplyr')
library('tidyr')
library('vroom')
library('data.table')

# Load and prepare datasets ----
## Companies House data ----
repD <- vroom("BasicCompanyDataAsOneFile-2020-10-01.csv")
#Re-format incorporation date.
repD$IncorporationDate <- as.Date(repD$IncorporationDate, "%d/%m/%Y")
#Keep 5 digit SIC codes.
repD$SIC5dg1 <- as.numeric(gsub("([0-9]+).*$", "\\1", repD$SICCode.SicText_1))
#Homogenise postcodes.
repD$Postcode <- toupper(gsub(" ", "", repD$RegAddress.PostCode))
#Select records from target period.
repDEOTHO <- repD[which(repD$IncorporationDate >= "2020-06-01" & repD$IncorporationDate <= "2020-10-01"),]
#Select records from comparison period.
repDBaseline <- repD[which(repD$IncorporationDate >= "2019-06-01" & repD$IncorporationDate <= "2019-10-01"),]

rm(repD)

#Remove entries with NA postcode.
repDEOTHO <- repDEOTHO[!(is.na(repDEOTHO$Postcode)),]
repDBaseline <- repDBaseline[!(is.na(repDBaseline$Postcode)),]

## Restaurants data (participants) ----
restaurants <- read.csv("restaurants.csv")
#Homogenise postcodes.
restaurants$Postcode <- toupper(gsub(" ", "", restaurants$Postcode))
#Keep only postcode district.
restaurants$PostcodeDistrict <- sub("...$", "", restaurants$Postcode)
#Keep only postcode area.
restaurants$PostcodeArea <- sub("[0-9]+(.*)", "", restaurants$PostcodeDistrict)

## NUTS ----
#Import NUTS conversion table.
ukNUTS <- vroom("pc2020_UK_NUTS.csv")
ukNUTS$CODE <- gsub("'", "", ukNUTS$CODE) %>% gsub(" ", "", .)
ukNUTS$NUTS3 <- gsub("'", "", ukNUTS$NUTS3)
ukNUTS <- ukNUTS %>% rename(Postcode=CODE)
ukNUTS$NUTS1 <- ukNUTS$NUTS3 %>% sub("..$", "", .)

#Add NUTS1 and NUTS3 columns to data.
repDEOTHO <- left_join(x=repDEOTHO, y=ukNUTS, by="Postcode")
repDBaseline <- left_join(x=repDBaseline, y=ukNUTS, by="Postcode")
restaurants <- left_join(x=restaurants, y=ukNUTS, by="Postcode")
nuts1_names <- data.frame(Name=c("North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland"), NUTS1=c("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", "UKM", "UKN"))

#Some postcodes were not in the NUTS dataset (probably due to incorrectly entered postcode).
sum(is.na(repDEOTHO$NUTS3))
sum(is.na(repDBaseline$NUTS3))

#Remove remaining entries with NA NUTS region.
repDEOTHO <- repDEOTHO[!(is.na(repDEOTHO$NUTS3)),]
repDBaseline <- repDBaseline[!(is.na(repDBaseline$NUTS3)),]

## Save data ----
#Full clean datasets.
full_data <- repDEOTHO[which(between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-08-31"))), c(3,16,57,58,59,60)]
full_data$EOTHO <- as.integer(full_data$Postcode %in% restaurants$Postcode)
full_data_baseline <- repDBaseline[which(between(repDBaseline$IncorporationDate, as.Date("2019-06-01"), as.Date("2019-08-31"))), c(3,16,57,58,59)]
full_data_baseline$EOTHO <- as.integer(full_data_baseline$Postcode %in% restaurants$Postcode)

#Save as csv.
fwrite(full_data, "Clean.csv", row.names=FALSE)
fwrite(full_data_baseline, "Clean_2019.csv", row.names=FALSE)

## Population data for regression ----
#Postcode district data.
pc_districts <- vroom("Postcode districts.csv")
pc_districts <- pc_districts[c(1,11)] %>% rename(PostcodeDistrict=Postcode)
full_data <- left_join(x=full_data, y=pc_districts, by="PostcodeDistrict")
full_data <- full_data %>% rename(DistrictPopulation=Population)
View(full_data[which(is.na(full_data$Population)),])

fwrite(full_data, "Clean_population.csv", row.names=FALSE)
