# ---
# Eat Out to Help Out
# Output: analysis by sector of registrations
# July 2021
# Contact: jph37@kent.ac.uk; i.galanakis@kent.ac.uk
# ---

# Load libraries ----
library('lubridate')
library('dplyr')
library('tidyr')
library('stringr')
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

#Keep only the code for SIC code text 1.
repDEO2HO$SIC5dg1 <- as.numeric(gsub("([0-9]+).*$", "\\1", repDEO2HO$SICCode.SicText_1))
repDBaseline$SIC5dg1 <- as.numeric(gsub("([0-9]+).*$", "\\1", repDBaseline$SICCode.SicText_1))
#Keep only the first two digits.
repDEO2HO$SIC2dg1 <- as.integer(repDEO2HO$SIC5dg1/1000)
repDBaseline$SIC2dg1 <- as.integer(repDBaseline$SIC5dg1/1000)
#Reformat postcodes for matching and extract postcode district and area.
repDEO2HO$Postcode <- toupper(gsub(" ", "", repDEO2HO$RegAddress.PostCode))
repDEO2HO$PostcodeDistrict <- sub("...$", "", repDEO2HO$Postcode)
repDEO2HO$PostcodeArea <- sub("[0-9]+(.*)", "", repDEO2HO$PostcodeDistrict)
repDBaseline$Postcode <- toupper(gsub(" ", "", repDBaseline$RegAddress.PostCode))
repDBaseline$PostcodeDistrict <- sub("...$", "", repDBaseline$Postcode)
repDBaseline$PostcodeArea <- sub("[0-9]+(.*)", "", repDBaseline$PostcodeDistrict)

#Restaurants data.

restaurants <- read.csv("restaurants.csv")
restaurants$Postcode <- toupper(gsub(" ", "", restaurants$Postcode))
#Keep only postcode district.
restaurants$PostcodeDistrict <- sub("...$", "", restaurants$Postcode)
#Keep only postcode area.
restaurants$PostcodeArea <- sub("[0-9]+(.*)", "", restaurants$PostcodeDistrict)
restaurants$Name <- toupper(restaurants$Name)


#SIC to sector conversion table.
sic2sector <- read.csv("sic2007conversion.csv")
sic2sector <- sic2sector %>% distinct(Section, Section.name, Division) %>% rename(SIC2dg1=Division)
sic2sector$Section <- sic2sector$Section %>% gsub(" ","",.)

#Add sector columns to data.
repDEO2HO <- left_join(x=repDEO2HO, y=sic2sector, by="SIC2dg1")
repDBaseline <- left_join(x=repDBaseline, y=sic2sector, by="SIC2dg1")

## Registrations timeseries----
#Count incorporations by sector and week
#2020 all postcodes
n_sector <- repDEO2HO %>% group_by(Section.name, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`) %>% ungroup() %>% rename(All_2020=n)
#2020 EO2HO postcodes
n_sector <- repDEO2HO[which(repDEO2HO$Postcode %in% restaurants$Postcode),] %>% group_by(Section.name, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`) %>% ungroup() %>% rename(EO2HO_2020=n) %>% left_join(x=n_sector, y=., by=c("Section.name", "Week"))
#2019 all postcodes
n_sector <- repDBaseline %>% group_by(Section.name, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`) %>% ungroup() %>% rename(All_2019=n) %>% left_join(x=n_sector, y=., by=c("Section.name", "Week"))
#2019 EO2HO postcodes
n_sector <- repDBaseline[which(repDBaseline$Postcode %in% restaurants$Postcode),] %>% group_by(Section.name, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`) %>% ungroup() %>% rename(EO2HO_2019=n) %>% left_join(x=n_sector, y=., by=c("Section.name", "Week"))
#Replace NA with in EO2HO_2020 column. (Leave as NA in EO2HO_2019, to prevent /0 errors.)
n_sector$EO2HO_2020 <- n_sector$EO2HO_2020 %>% replace_na(0)

#Trim first and last weeks as they are incomplete.
n_sector <- n_sector[which(between(n_sector$Week, 23, 39)),]

#Remove NA rows.
n_sector <- n_sector[which(!(is.na(n_sector$Section.name))),]

#Calculate ratios.
n_sector$All_Postcodes <- n_sector$All_2020 / n_sector$All_2019
n_sector$EO2HO_Postcodes <- n_sector$EO2HO_2020 / n_sector$EO2HO_2019

#Index ratios.
index_sector <- function(s, c) {
  (n_sector[[s,c]] / n_sector[which(n_sector$Week==23 & n_sector$Section.name == n_sector[[s,1]]),c])[[1]]
}
n_sector$iAll <- vapply(seq(1,nrow(n_sector)), index_sector, c=7, FUN.VALUE=numeric(1))
n_sector$iEO2HO <- vapply(seq(1,nrow(n_sector)), index_sector, c=8, FUN.VALUE=numeric(1))

#Calculate difference between ratios. (Effect of EO2HO.)
n_sector$Diff <- n_sector$EO2HO_Postcodes - n_sector$All_Postcodes
#Index difference
n_sector$iDiff <- n_sector$iEO2HO - n_sector$iAll

#List all sectors except Accommodation and Food.
sectors <- distinct(sic2sector[which(sic2sector$Section != "I"),], Section.name)
#Truncate names if longer than 3 words.
sectors$Name <- sectors$Section.name %>% str_extract(., "[a-zA-Z]+( |, |; )[a-zA-Z]+( |, |; )[a-zA-Z]+") %>% 
  str_c("...") %>%  ifelse(is.na(.), sectors$Section.name, .)

#Create timeseries plots for each sector.
plot_sector <- function (sector) {
  plot_ly(data=n_sector[which(n_sector$Section.name==sector),], x=~Week, y=~iAll, type='scatter', mode='lines', name="All Postcodes", color=I("#1f77b4"), showlegend=FALSE) %>% 
    add_trace(y=~iEO2HO, type='scatter', mode='lines', name="EO2HO Postcodes", color=I("#ff7f0e"), showlegend=FALSE) %>%
    add_annotations(
      text = sectors$Name[which(sectors$Section.name==sector)],
      x = 31,
      y = ~(max(iEO2HO, na.rm=TRUE)+0.5),
      yref = "y",
      xref = "x",
      showarrow = FALSE) %>%
    layout(title=list(text="Ratio of registrations (2020/2019) in All Postcodes and EO2HO Postcodes"),
           xaxis=list(range=c(23,39)),
           shapes=list(list(type='line', x0=yday("2020-07-08")/7, x1=yday("2020-07-08")/7, y0=0, y1=~(max(iEO2HO, na.rm=TRUE)+0.3), opacity=0.2),
                       list(type='rect', x0=yday("2020-08-01")/7, x1=yday("2020-08-31")/7, y0=0, y1=~(max(iEO2HO, na.rm=TRUE)+0.3), fillcolor="grey", line = list(color = "grey"), opacity=0.2)))
}
sector_plots <- lapply(sectors$Section.name, plot_sector)
sectorplot <- subplot(sector_plots, nrows=4)


## Registrations in Period2----
#Count registrations in each sector between announcement and start of the scheme.
period2_sectors <- repDEO2HO[which(between(as.Date(repDEO2HO$IncorporationDate), as.Date("2020-07-08"), as.Date("2020-08-01"))),] %>% group_by(Section.name) %>% count() %>% rename(All_2020=n)
#EO2HO postcodes.
period2_sectors <- repDEO2HO[which(between(as.Date(repDEO2HO$IncorporationDate), as.Date("2020-07-08"), as.Date("2020-08-01")) & repDEO2HO$Postcode %in% restaurants$Postcode),] %>% group_by(Section.name) %>% count() %>%
  merge(period2_sectors, ., by="Section.name") %>% rename(EO2HO_2020=n)
#2019
period2_sectors <- repDBaseline[which(between(as.Date(repDBaseline$IncorporationDate), as.Date("2019-07-08"), as.Date("2019-08-01"))),] %>% group_by(Section.name) %>% count() %>%
  merge(period2_sectors, ., by="Section.name") %>% rename(All_2019=n)
period2_sectors <- repDBaseline[which(between(as.Date(repDBaseline$IncorporationDate), as.Date("2019-07-08"), as.Date("2019-08-01")) & repDBaseline$Postcode %in% restaurants$Postcode),] %>% group_by(Section.name) %>% count() %>%
  merge(period2_sectors, ., by="Section.name") %>% rename(EO2HO_2019=n)

#Ratios.
period2_sectors$All_Postcodes <- period2_sectors$All_2020 / period2_sectors$All_2019
period2_sectors$EO2HO_Postcodes <- period2_sectors$EO2HO_2020 / period2_sectors$EO2HO_2019

#Difference.
period2_sectors$Diff <- period2_sectors$EO2HO_Postcodes - period2_sectors$All_Postcodes

#Keep 20 Other sectors.
period2_sectors <- left_join(x=sectors, y=period2_sectors, by="Section.name")

#Bootstrap confidence interval function.
boot_ci <- function(All_2020, All_2019, EO2HO_2020, EO2HO_2019) {
  #Convert observations to parameters for resampling.
  N_1 <- All_2020 + All_2019
  P_1 <- All_2020 / N_1
  N_2b <- EO2HO_2020 + EO2HO_2019
  P_2a <- N_2b / N_1
  P_2b <- EO2HO_2020 / N_2b
  #Random resample for All_2020 from Bin(N,P).
  db_all <- rbinom(10000, N_1, P_1)
  #Convert All_2020 to ratio.
  db_all <- db_all / (N_1 - db_all)
  #Random number for EO2HO_2020 from binomial distribution with size ~Bin(N,(M/N)) and probability Q.
  db_eo2ho <- rbinom(100, rbinom(100,N_1,P_2a), P_2b)
  #Convert to ratio using N_2b for size.
  db_eo2ho <- db_eo2ho / (N_2b - db_eo2ho)
  #Calculate difference of ratios.
  db_diff <- db_eo2ho - db_all
  #Output confidence interval.
  list(Lo=quantile(db_diff,0.025)[[1]],Hi=quantile(db_diff,0.975)[[1]])
  #Optionally display histogram of the distribution.
  #hist(db_diff)
}

#Apply bootstrap CI.
boot_ci_sect <- function(i) {boot_ci(period2_sectors$All_2020[i], period2_sectors$All_2019[i], period2_sectors$EO2HO_2020[i], period2_sectors$EO2HO_2019[i])}

period2_sectors$CI <- lapply(seq(1:nrow(period2_sectors)), boot_ci_sect)
period2_sectors <- period2_sectors %>% unnest_wider(col=CI)

period2_sectors <- period2_sectors %>% arrange(desc(period2_sectors$Diff))
period2_sectors$Name <- factor(period2_sectors$Name, levels=c(as.character(period2_sectors$Name)))

significant_sect <- rep(0.5, nrow(period2_sectors))
significant_sect[which(period2_sectors$Lo > 0 | period2_sectors$Hi <0)] <- 1

#Barplot with confidence intervals.
figure13 <- plot_ly(data=period2_sectors) %>% add_bars(x=~Name, y=~Diff, showlegend=FALSE,
                                        marker=list(opacity=significant_sect)) %>% 
  add_segments(x=~Name, xend=~Name, y=~Lo, yend=~Hi, name="95% Confidence Interval", color=I("black"), opacity=0.6) %>%
  layout(barmode='group',
         title=list(text=paste0("Spillover effect between announcement and start of EO2HO",
                                '<br>',
                                '<sup>',
                                "Ratio of registrations in EO2HO Postcodes - All Postcodes",
                                '</sup>')),
         xaxis=list(title="Sector"),
         yaxis=list(title="Difference in ratio of registrations (2020/2019)", range=c(-1,2)))


##Another look at timeseries for significant sectors----
sector_plots2 <- lapply(period2_sectors$Section.name[which(significant_sect==1)], plot_sector)
sectorplot2 <- subplot(sector_plots2, nrows=2)

