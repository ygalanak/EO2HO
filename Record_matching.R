# ---
# Eat Out to Help Out
# Output: matches participants register `restaurants.csv from HMRC to companies incorporations register from Companies House
# June 2021
# Contact: jph37@kent.ac.uk; i.galanakis@kent.ac.uk
# ---

# Load libraries ----
library('lubridate')
library('dplyr')
library('tidyr')
library('stringdist')

# Load and prepare datasets ----
## Companies House data ----
repD <- read.csv("BasicCompanyDataAsOneFile-2020-10-01.csv")
#Re-format incorporation date.
repD$IncorporationDate <- as.Date(repD$IncorporationDate, "%d/%m/%Y")
#Keep 5 and 2 digit SIC codes.
repD$SIC5dg1 <- as.numeric(gsub("([0-9]+).*$", "\\1", repD$SICCode.SicText_1))
repD$SIC2dg1 <- as.integer(repD$SIC5dg1/1000)
#Homogenise postcodes.
repD$Postcode <- toupper(gsub(" ", "", repD$RegAddress.PostCode))
#Keep only postcode district.
repD$PostcodeDistrict <- sub("...$", "", repD$Postcode)
#Keep only postcode area.
repD$PostcodeArea <- sub("[0-9]+(.*)", "", repD$PostcodeDistrict)

## Restaurants data (participants) ----
restaurants <- read.csv("restaurants.csv")
#Homogenise postcodes.
restaurants$Postcode <- toupper(gsub(" ", "", restaurants$Postcode))
#Keep only postcode district.
restaurants$PostcodeDistrict <- sub("...$", "", restaurants$Postcode)
#Keep only postcode area.
restaurants$PostcodeArea <- sub("[0-9]+(.*)", "", restaurants$PostcodeDistrict)

##First match - by exact name and postcode ----
#Function to clean the names.
name_clean <- function(cpy) {
  cpy <- toupper(cpy)
  cpy <- gsub("LTD|LIMITED|[[:punct:]]| ", "", cpy)
  return(cpy)
}

#Apply name-cleaning function.
restaurants$Name <- name_clean(restaurants$Name)
repD$Name <- name_clean(repD$CompanyName)

#Matching by exact cleaned name.
companies <- merge(x=restaurants, y=repD, by.x="Name", by.y="Name")

#Cross-checking by postcode.
#companies_1 <- companies[which(companies$Postcode.x == companies$Postcode.y),]
#This excluded too many results, so we use postcode area instead.

#Cross-checking by postcode area.
companies_2 <- companies[which(companies$PostcodeArea.x == companies$PostcodeArea.y),]
#Check the list of excluded matches.
#companies_2_un <- companies[which(companies$PostcodeArea.x != companies$PostcodeArea.y),]

#Extract the list of remaining unmatched restaurants.
companies_un <- restaurants_1[which(!(restaurants_1$Name %in% companies_2$Name)),]

##Second match - by postcode, number and fuzzy match name and address ----
#Match first by postcode.
companies_3 <- merge(x=companies_un, y=repD, by="Postcode")

#Select first number in each address (x=EO2HO restaurants, y=CH).
companies_3$number.x <- as.integer(sub("\\D*(\\d+).*", "\\1", paste0(companies_3$Line.1,companies_3$Line.2)))
companies_3$number.x <- companies_3$number.x %>% replace_na(0)
companies_3$number.y <- as.integer(sub("\\D*(\\d+).*", "\\1", paste0(companies_3$RegAddress.AddressLine1,companies_3$RegAddress.AddressLine2)))
companies_3$number.y <- companies_3$number.y %>% replace_na(0)
#Keep only records with matching numbers.
companies_4 <- companies_3[which(companies_3$number.x == companies_3$number.y),]

#Jaro-Winkler distance for addresses (concatenating first and second lines of address.)
companies_4$addressMatch <- stringdist(toupper(paste0(companies_4$Line.1,companies_4$Line.2)),paste0(companies_4$RegAddress.AddressLine1,companies_4$RegAddress.AddressLine2),method="jw")
#Jaro-Winkler distance for cleaned names.
companies_4$match.n <- stringdist(companies_4$Name.x, companies_4$Name.y, method="jw")
#Keep records with both Jaro-Winkler distances less than 0.5.
companies_5 <- companies_4[which(companies_4$addressMatch<0.5 & companies_4$match.n<0.5),]

#Keep only businesses with main SIC codes.
companies_6 <- companies_5[which(companies_5$SIC2dg1 %in% list(56,55,47,93,1,10,46,11)),]
companies_6 <- companies_6 %>% rename(Name=Name.x)

##Bind the two subsets of matched records----
all_companies <- rbind(companies_2[intersect(colnames(companies_2), colnames(companies_6))], companies_6[intersect(colnames(companies_2), colnames(companies_6))])

#All remaining unmatched restaurants.
unmatched <- restaurants_1[which(!(restaurants_1$Name %in% all_companies$Name)),]

#Lower bound for number of duplicates.
nrow(restaurants_1) - nrow(all_companies) - nrow(unmatched)

#Aggregations
##Aggregate records by SIC----
restaurant_sic <- all_companies %>% group_by(SIC5dg1) %>% count()
#Calculate percentage of sample.
restaurant_sic$percent <- restaurant_sic$n / sum(restaurant_sic$n)
#Calculate 99% confidence intervals.
restaurant_sic$lo <- sapply(restaurant_sic$n, function(z) prop.test(z, sum(restaurant_sic$n), conf.level=0.99)$conf.int[1])
restaurant_sic$hi <- sapply(restaurant_sic$n, function(z) prop.test(z, sum(restaurant_sic$n), conf.level=0.99)$conf.int[2])
#Save table.
write.csv(restaurant_sic, "Matched_sic.csv")

##Investigate incorporation dates----
#Number of 'new' companies (incorporated after the registration deadline).
all_companies[which(all_companies$IncorporationDate > as.Date("2020-07-08")),] %>% count()
#Range of dates.
range(all_companies$IncorporationDate)

#Histogram of incorporation dates of matched companies.
library('ggplot2')
library('plotly')
plot_ly(data=all_companies, x=~IncorporationDate, type='histogram') %>% layout(title=list(text="Incorporation date of EO2HO restaurants"),
                                                                                 xaxis=list(title="Incorporation date", range=c(as.Date("1996-07-07"),as.Date("2021-07-07"))),
                                                                                 yaxis=list(title="Number"))


##Compare restaurants, matched and unmatched by postcode area----
restaurants_area <- restaurants_1 %>% group_by(PostcodeArea) %>% count()
restaurants_area <- all_companies %>% group_by(PostcodeArea.x) %>% count() %>% rename(PostcodeArea=PostcodeArea.x) %>% full_join(x=restaurants_area, y=., by="PostcodeArea") %>% rename(Restaurants=n.x, Matched=n.y)
restaurants_area <- unmatched %>% group_by(PostcodeArea) %>% count() %>% full_join(x=restaurants_area, y=., by="PostcodeArea") %>% rename(Unmatched=n)
#Percentage of matched restaurants.
restaurants_area$Percent_match <- replace_na(restaurants_area$Matched) / restaurants_area$Restaurants

#Map showing matched percentage by postcode area.
library('rgeos')
library('rgdal')
library('maptools')
library('scales')
shp1 <- readOGR('Distribution/Areas.shp')
shp1 <- fortify(shp1, region="name")
shp1 <- merge(shp1, restaurants_area[which(restaurants_area$PostcodeArea != "N"),], by.x='id', by.y='PostcodeArea', all.x=TRUE)
shp1 <- arrange(shp1, order)
p <- ggplot(data=shp1, aes(x=long, y=lat, group=group, fill=Percent_match)) + geom_polygon() + coord_map() + theme_void() + labs(title="Matched restaurants", fill="% matched")
p + scale_fill_gradient(low="grey",high="springgreen3", labels=percent)
