# ---
# Eat Out to Help Out
# Output: matches participants register `restaurants.csv from HMRC to companies incorporations register from Companies House; compares to representative random sample of relevant companies
# July 2021
# Contact: jph37@kent.ac.uk; i.galanakis@kent.ac.uk
# ---

# Load libraries ----
library('lubridate')
library('dplyr')
library('tidyr')
library('stringdist')
library('ggplot2')
library('plotly')

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
companies_un <- restaurants[which(!(restaurants$Name %in% companies_2$Name)),]

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

#Remove any restaurants matched to multiple companies.
companies_7 <- companies_6[which(!(companies_6$Name %in% companies_6$Name[which(duplicated(companies_6$Name))])),]
#Recover those restaurants.
companies_un <- companies_6[which(companies_6$Name %in% companies_6$Name[which(duplicated(companies_6$Name))]),]
#Order by Jaro-Winkler distance for names.
companies_un <- companies_un %>% arrange(Name, match.n)
#New dataframe with arbitrary first entry.
companies_8 <- companies_6[1,]
#For each restaurant, extract the one with the lowest Jaro-Winkler distance for names.
for (nm in unique(companies_un$Name)) {
  companies_8[nrow(companies_8)+1,] <- head(companies_un[which(companies_un$Name == nm),],1)
}
#Remove the arbitrary first entry.
companies_8 <- companies_8[-1,]

#Rejoin the separated datasets.
companies_9 <- rbind(companies_7, companies_8)

##Bind the two subsets of matched records----
all_companies <- rbind(companies_2[intersect(colnames(companies_2), colnames(companies_9))], companies_9[intersect(colnames(companies_2), colnames(companies_9))])
write.csv(all_companies, "matched_restaurants.csv")
#all_companies <- read.csv("matched_restaurants.csv")

#All remaining unmatched restaurants.
unmatched <- restaurants[which(!(restaurants$Name %in% all_companies$Name)),]


#Aggregations
##Aggregate records by SIC----
restaurant_sic <- all_companies %>% group_by(SIC2dg1) %>% count()
#Calculate percentage of sample.
restaurant_sic$percent <- restaurant_sic$n / sum(restaurant_sic$n)
#Calculate 99% confidence intervals.
restaurant_sic$lo <- sapply(restaurant_sic$n, function(z) prop.test(z, sum(restaurant_sic$n), conf.level=0.99)$conf.int[1])
restaurant_sic$hi <- sapply(restaurant_sic$n, function(z) prop.test(z, sum(restaurant_sic$n), conf.level=0.99)$conf.int[2])
#Save table.
#write.csv(restaurant_sic, "Matched_sic.csv")

##Investigate incorporation dates----
#Number of 'new' companies (incorporated after the registration deadline).
all_companies[which(all_companies$IncorporationDate > as.Date("2020-07-08")),] %>% count()
#Range of dates.
range(all_companies$IncorporationDate)

#Age at announcement date.
restaurants_age <- data.frame(AgeLower=c(-1, 0, 0.25, 0.5, 1, 2, 3, 5, 10), AgeUpper=c(0, 0.25, 0.5, 1, 2, 3, 5, 10, 200))
restaurants_age$n <- sapply(seq(1:nrow(restaurants_age)), function(i) nrow(all_companies[which(between(as.numeric(ymd("2020-07-08")-all_companies$IncorporationDate), restaurants_age$AgeLower[i]*365, restaurants_age$AgeUpper[i]*365-1)),]))
restaurants_age$Percent <- restaurants_age$n / sum(restaurants_age$n)
#Calculate 99% confidence intervals.
restaurants_age$lo <- sapply(restaurants_age$n, function(z) prop.test(z, sum(restaurants_age$n), conf.level=0.99)$conf.int[1])
restaurants_age$hi <- sapply(restaurants_age$n, function(z) prop.test(z, sum(restaurants_age$n), conf.level=0.99)$conf.int[2])
#Save table.
write.csv(restaurants_age, "Matched_age.csv")

##Compare restaurants, matched and unmatched by region----
#Postcode area
r_Area <- restaurants %>% group_by(PostcodeArea) %>% count()
restaurants_area <- all_companies %>% group_by(PostcodeArea.x) %>% count() %>% rename(PostcodeArea=PostcodeArea.x) %>% full_join(x=r_Area, y=., by="PostcodeArea") %>% rename(Restaurants=n.x, Matched=n.y)
restaurants_area <- unmatched %>% group_by(PostcodeArea) %>% count() %>% full_join(x=restaurants_area, y=., by="PostcodeArea") %>% rename(Unmatched=n)
#Percentage of matched restaurants.
restaurants_area$Percent_match <- replace_na(restaurants_area$Matched) / restaurants_area$Restaurants

#NUTS1 region.
all_companies$Postcode <- toupper(gsub(" ", "", all_companies$RegAddress.PostCode))
all_companies <- left_join(x=all_companies, y=pc2NUTS, by="Postcode")
r_region <- restaurants %>% group_by(NUTS1) %>% count()
restaurants_NUTS1 <- all_companies %>% group_by(NUTS1) %>% count() %>% rename(Matched=n) %>% full_join(x=r_region, y=., by="NUTS1")
#Percentage of matched restaurants.
restaurants_NUTS1$Percent_match <- restaurants_NUTS1$Matched / restaurants_NUTS1$n


#Map showing matched percentage by postcode area.
library('rgeos')
library('rgdal')
library('maptools')
library('scales')

shp1 <- fortify(shp, region="nuts118cd")
shp1 <- merge(shp1, restaurants_NUTS1, by.x='id', by.y='NUTS1', all.x=TRUE) %>% arrange(order)
shp1 <- arrange(shp1, order)
matchmap <- ggplot(data=shp1, aes(x=long, y=lat, group=group, fill=Percent_match)) + geom_polygon() + coord_equal() + theme_void() + labs(fill="% matched") + scale_fill_gradient(low="grey",high="springgreen3", labels=percent)

figurea1 <- ggplotly(matchmap, tooltip="fill") %>% layout(xaxis=list(showline=FALSE, showgrid=FALSE), 
                                              yaxis=list(showline=FALSE, showgrid=FALSE)) %>% hide_legend()


## Representative sample----
#Sector percentages from HMRC.
sector_weights <- data.frame(SIC2dg1=c(56,55,47,93,01,10,46,11), percent=c(78.6,8.7,4.4,2.8,0.7,0.5,0.5,0.2))
#Recalculate percent (as this excludes other sectors).
sector_weights$percent <- sector_weights$percent / sum(sector_weights$percent)

sample <- repD[which(repD$X == 1077398),]
for (i in seq(1,nrow(sector_weights))) {
  s <- sector_weights$SIC2dg1[i]
  n <- round(sector_weights$percent[i] * nrow(all_companies))
  sample[seq(from=nrow(sample)+1,length.out=n),] <- repD[which(repD$SIC2dg1 == s),] %>% slice_sample(n=n)
}
write.csv(sample, "sample.csv")
sample <- read.csv("sample.csv")
sample <- left_join(x=sample, y=pc2NUTS, by="Postcode")

##Incorporation date----
#Density plot
dateDensity <- density(as.numeric(as.Date(all_companies$IncorporationDate))[which(all_companies$IncorporationDate > ymd("1970-01-01"))], na.rm=TRUE, adjust=0.4)
sampleDensity <- density(as.numeric(as.Date(sample$IncorporationDate))[which(sample$IncorporationDate > ymd("1970-01-01"))], na.rm=TRUE, adjust=0.4)
ageplot <- plot_ly(x=~(as.Date("1970-01-01")+(dateDensity$x)), y=~dateDensity$y, type='scatter', mode='lines', fill='tozeroy', name="Matched EO2HO companies", color=I("#ff7f0e")) %>%
  add_trace(x=~(as.Date("1970-01-01")+(sampleDensity$x)), y=~sampleDensity$y, type='scatter', mode='lines', fill='tozeroy', opacity=0.4, name="Sample of all companies", color=I("#BCD0FF")) %>%
  layout(title=list(text="Incorporation date of companies in relevant sectors"),
         xaxis=list(title="Incorporation date", range=c(as.Date("1995-10-01"),as.Date("2020-10-01"))),
         yaxis=list(title="Density"),
         legend=list(x=0.1, y=0.9))

##Location----
#Aggregate by NUTS1 region.
r_region <- restaurants %>% group_by(NUTS1) %>% count()
r_region <- r_region[complete.cases(r_region),]
r_region$EO2HO <- r_region$n / sum(r_region$n)
r_region <- sample %>% group_by(NUTS1) %>% count() %>% rename(n.all=n) %>%
  left_join(x=r_region, y=., by="NUTS1")
r_region$All <- r_region$n.all / sum(r_region$n.all)
r_region <- r_region %>% ungroup()
r_region <- merge(x=nuts1_names, y=r_region[c(1:12),], by="NUTS1")

r_region <- r_region %>% arrange(desc(n))
r_region$Name <- factor(r_region$Name, levels=c(as.character(r_region$Name)))


#Excluding London
r_region$EO2HO.1 <- r_region$n / sum(r_region$n[-1])
r_region$All.1 <- r_region$n.all / sum(r_region$n.all[-1])
write.csv(r_region[c(2,4,6,7,8)], "Fig2.csv", row.names=FALSE)


#Combined plot
figure2 <- plot_ly(data=r_region, x=~Name, y=~EO2HO, name="Registered EO2HO companies", type='bar', color=I("#ff7f0e")) %>% 
  add_trace(data=r_region, x=~Name, y=~All, type='bar', name="Sample of all companies",  color=I("#BCD0FF")) %>%
  add_trace(data=r_region[-1,], x=~Name, y=~EO2HO.1, name="Registered EO2HO companies", type='bar', visible=FALSE, color=I("#ff7f0e")) %>% 
  add_trace(data=r_region[-1,], x=~Name, y=~All.1, type='bar', name="Sample of all companies", visible=FALSE, color=I("#BCD0FF")) %>%
  layout(title=list(text="Companies in relevant sectors by region"),
         xaxis=list(title="Region"),
         yaxis=list(title="Percent of total", tickformat="%"),
         legend=list(x=0.65, y=0.9),
         updatemenus=list(list(buttons=list(list(label="All regions", method="update",
                                                 args=list(list(visible=c(TRUE, TRUE, FALSE, FALSE)))),
                                            list(label="Excl. London", method="update",
                                                 args=list(list(visible=c(FALSE, FALSE, TRUE, TRUE))))))))

