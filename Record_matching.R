#Load libraries.
library('lubridate')
library('dplyr')
library('tidyr')
library('stringdist')

#Load and prepare datasets.
#Incorporations data.
repD <- read.csv("BasicCompanyDataAsOneFile-2020-10-01.csv")
#Re-format incorporation date.
repD$IncorporationDate <- as.Date(repD$IncorporationDate, "%d/%m/%Y")
#Keep 5 and 2 digit SIC codes.
repD$SIC5dg1 <- as.numeric(gsub("([0-9]+).*$", "\\1", repD$SICCode.SicText_1))
repD$SIC2dg1 <- as.integer(repD$SIC5dg1/1000)
#Homogenise postcodes.
repD$Postcode <- toupper(gsub(" ", "", repD$RegAddress.PostCode))

#Restaurants data.
restaurants <- read.csv("restaurants.csv")
#Homogenise postcodes.
restaurants$Postcode <- toupper(gsub(" ", "", restaurants$Postcode))
#Keep only postcode district.
restaurants$PostcodeDistrict <- sub("...$", "", restaurants$Postcode)
#Keep only postcode area.
restaurants$PostcodeArea <- sub("[0-9]+(.*)", "", restaurants$PostcodeDistrict)

#Join CH and EO2HO registers by postcode and only companies with SIC 55 or 56.
all_companies <- merge(x=restaurants, y=repD[which(repD$SIC2dg1 %in% list(55,56)),], by="Postcode")
#Discard unused columns.
all_companies <- all_companies[c(1:8,10,14,15,24,65,66)]

#Function to clean the names.
name_clean <- function(cpy) {
  cpy <- toupper(cpy)
  cpy <- gsub("LTD|LIMITED|[[:punct:]]| ", "", cpy)
  return(cpy)
}

#Fuzzy match metric for addresses (concatenate first and second lines of address.)
all_companies$addressMatch <- stringdist(toupper(paste0(all_companies$Line.1,all_companies$Line.2)),paste0(all_companies$RegAddress.AddressLine1,all_companies$RegAddress.AddressLine2),method="jw")
#Select first number in each address (x=EO2HO restaurants, y=CH).
all_companies$number.x <- as.integer(sub("\\D*(\\d+).*", "\\1", paste0(all_companies$Line.1,all_companies$Line.2)))
all_companies$number.x <- all_companies$number.x %>% replace_na(0)
all_companies$number.y <- as.integer(sub("\\D*(\\d+).*", "\\1", paste0(all_companies$RegAddress.AddressLine1,all_companies$RegAddress.AddressLine2)))
all_companies$number.y <- all_companies$number.y %>% replace_na(0)
#Use name-cleaning function.
all_companies$Name.x <- name_clean(all_companies$Name)
all_companies$Name.y <- name_clean(all_companies$CompanyName)
#Fuzzy match metric for cleaned names.
all_companies$match.n <- stringdist(all_companies$Name.x, all_companies$Name.y, method="jw")
#Overall match metric using the above.
all_companies$match <- abs(all_companies$addressMatch + all_companies$match.n + all_companies$number.x - all_companies$number.y)

#Select records with a match metric above a threshold (0.45 seems to yield reasonable results without too many false positives).
all_companies_2 <- all_companies[which(all_companies$match < 0.45),]

#Aggregate records by SIC.
restaurant_sic <- all_companies_2 %>% group_by(SIC5dg1) %>% count()
#Calculate percentage of sample.
restaurant_sic$percent <- restaurant_sic$n / sum(restaurant_sic$n)
#Calculate 99% confidence intervals.
restaurant_sic$lo <- sapply(restaurant_sic$n, function(z) prop.test(z, sum(restaurant_sic$n), conf.level=0.99)$conf.int[1])
restaurant_sic$hi <- sapply(restaurant_sic$n, function(z) prop.test(z, sum(restaurant_sic$n), conf.level=0.99)$conf.int[2])

#Aggregate records by incorporation date.
#Select only incorporations in 2020 and aggregate by month.
restaurant_new <- all_companies_2[which(all_companies_2$IncorporationDate >= as.Date("2020-01-01")),] %>% group_by(month(IncorporationDate)) %>% count() %>% rename(Month = `month(IncorporationDate)`)
restaurant_new[10,] <- c(0, (all_companies_2[which(all_companies_2$IncorporationDate < as.Date("2020-01-01")),] %>% count()))
#Add a row for pre-2020.
restaurant_new$percent <- restaurant_new$n / sum(restaurant_new$n)
#Calculate 99% confidence intervals.
restaurant_new$lo <- sapply(restaurant_new$n, function(z) prop.test(z, sum(restaurant_new$n), conf.level=0.99)$conf.int[1])
restaurant_new$hi <- sapply(restaurant_new$n, function(z) prop.test(z, sum(restaurant_new$n), conf.level=0.99)$conf.int[2])


