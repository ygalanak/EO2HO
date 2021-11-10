# ---
# Eat Out to Help Out
# Output: analysis of registrations by sector
# November 2021
# Contact: jph37@kent.ac.uk; i.galanakis@kent.ac.uk
# ---

# Load libraries ----
library('lubridate')
library('dplyr')
library('tidyr')
library('plotly')
library('stringr')
library('vroom')

## Incorporations data (from Clean data.R)
repDEOTHO <- vroom("Clean.csv")
repDBaseline <- vroom("Clean_2019.csv")
#2 digit SIC code.
repDEOTHO$SIC2dg1 <- as.integer(repDEOTHO$SIC5dg1/1000)
repDBaseline$SIC2dg1 <- as.integer(repDBaseline$SIC5dg1/1000)

## Restaurants data (participants) ----
restaurants <- read.csv("restaurants.csv")
#Homogenise postcodes.
restaurants$Postcode <- toupper(gsub(" ", "", restaurants$Postcode))

#SIC to sector conversion table.
sic2sector <- read.csv("sic2007conversion.csv")
sic2sector <- sic2sector %>% distinct(Section, Section.name, Division) %>% rename(SIC2dg1=Division)
sic2sector$Section <- sic2sector$Section %>% gsub(" ","",.)

#Sector abbreviations.
abbrs <- vroom("SectionAbbreviation.csv")
abbrs$Section <- abbrs$Section %>% gsub(" ","",.)
sic2sector <- merge(x=sic2sector, y=abbrs, by="Section")

#Add sector columns to data.
repDEOTHO <- left_join(x=repDEOTHO, y=sic2sector, by="SIC2dg1")
repDBaseline <- left_join(x=repDBaseline, y=sic2sector, by="SIC2dg1")

## Registrations in Period2----
#Count registrations in each sector between announcement and start of the scheme.
period2_sectors <- repDEOTHO[which(between(as.Date(repDEOTHO$IncorporationDate), as.Date("2020-07-08"), as.Date("2020-08-01"))),] %>% group_by(SectionAbb) %>% count() %>% rename(All_2020=n)
#EOTHO postcodes.
period2_sectors <- repDEOTHO[which(between(as.Date(repDEOTHO$IncorporationDate), as.Date("2020-07-08"), as.Date("2020-08-01")) & repDEOTHO$EOTHO == 1),] %>% group_by(SectionAbb) %>% count() %>%
  merge(period2_sectors, ., by="SectionAbb") %>% rename(EOTHO_2020=n)
#2019
period2_sectors <- repDBaseline[which(between(as.Date(repDBaseline$IncorporationDate), as.Date("2019-07-08"), as.Date("2019-08-01"))),] %>% group_by(SectionAbb) %>% count() %>%
  merge(period2_sectors, ., by="SectionAbb") %>% rename(All_2019=n)
period2_sectors <- repDBaseline[which(between(as.Date(repDBaseline$IncorporationDate), as.Date("2019-07-08"), as.Date("2019-08-01")) & repDBaseline$EOTHO == 1),] %>% group_by(SectionAbb) %>% count() %>%
  merge(period2_sectors, ., by="SectionAbb") %>% rename(EOTHO_2019=n)

#Remove NA
period2_sectors <- period2_sectors[which(!(is.na(period2_sectors$SectionAbb))),]

#Ratios.
period2_sectors$All_Postcodes <- period2_sectors$All_2020 / period2_sectors$All_2019
period2_sectors$EOTHO_Postcodes <- period2_sectors$EOTHO_2020 / period2_sectors$EOTHO_2019

#Difference.
period2_sectors$Diff <- period2_sectors$EOTHO_Postcodes - period2_sectors$All_Postcodes

#Bootstrap confidence interval function.
boot_ci <- function(All_2020, All_2019, EOTHO_2020, EOTHO_2019) {
  #Convert observations to parameters for resampling.
  N_1 <- All_2020 + All_2019
  P_1 <- All_2020 / N_1
  N_2b <- EOTHO_2020 + EOTHO_2019
  P_2a <- N_2b / N_1
  P_2b <- EOTHO_2020 / N_2b
  #Random resample for All_2020 from Bin(N,P).
  db_all <- rbinom(10000, N_1, P_1)
  #Convert All_2020 to ratio.
  db_all <- db_all / (N_1 - db_all)
  #Random number for EOTHO_2020 from binomial distribution with size ~Bin(N,(M/N)) and probability Q.
  db_EOTHO <- rbinom(100, rbinom(100,N_1,P_2a), P_2b)
  #Convert to ratio using N_2b for size.
  db_EOTHO <- db_EOTHO / (N_2b - db_EOTHO)
  #Calculate difference of ratios.
  db_diff <- db_EOTHO - db_all
  #Output confidence interval.
  list(Lo=quantile(db_diff,0.025)[[1]],Hi=quantile(db_diff,0.975)[[1]])
  #Optionally display histogram of the distribution.
  #hist(db_diff)
}

#Apply bootstrap CI.
boot_ci_sect <- function(i) {boot_ci(period2_sectors$All_2020[i], period2_sectors$All_2019[i], period2_sectors$EOTHO_2020[i], period2_sectors$EOTHO_2019[i])}

period2_sectors$CI <- lapply(seq(1:nrow(period2_sectors)), boot_ci_sect)
period2_sectors <- period2_sectors %>% unnest_wider(col=CI)

period2_sectors <- period2_sectors %>% arrange(desc(period2_sectors$Diff))
period2_sectors$SectionAbb <- factor(period2_sectors$SectionAbb, levels=c(as.character(period2_sectors$SectionAbb)))

significant_sect <- rep(0.5, nrow(period2_sectors))
significant_sect[which(period2_sectors$Lo > 0 | period2_sectors$Hi <0)] <- 1

## Registrations timeseries----
#Count incorporations by sector and week
#2020 all postcodes
n_sector <- repDEOTHO %>% group_by(SectionAbb, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`) %>% ungroup() %>% rename(All_2020=n)
#2020 EOTHO postcodes
n_sector <- repDEOTHO[which(repDEOTHO$EOTHO == 1),] %>% group_by(SectionAbb, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`) %>% ungroup() %>% rename(EOTHO_2020=n) %>% left_join(x=n_sector, y=., by=c("SectionAbb", "Week"))
#2019 all postcodes
n_sector <- repDBaseline %>% group_by(SectionAbb, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`) %>% ungroup() %>% rename(All_2019=n) %>% left_join(x=n_sector, y=., by=c("SectionAbb", "Week"))
#2019 EOTHO postcodes
n_sector <- repDBaseline[which(repDBaseline$EOTHO == 1),] %>% group_by(SectionAbb, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`) %>% ungroup() %>% rename(EOTHO_2019=n) %>% left_join(x=n_sector, y=., by=c("SectionAbb", "Week"))
#Replace NA with in EOTHO_2020 column. (Leave as NA in EOTHO_2019, to prevent /0 errors.)
n_sector$EOTHO_2020 <- n_sector$EOTHO_2020 %>% replace_na(0)

#Trim first and last weeks as they are incomplete.
n_sector <- n_sector[which(between(n_sector$Week, 23, 35)),]

#Remove NA rows.
n_sector <- n_sector[which(!(is.na(n_sector$SectionAbb))),]

#Calculate ratios.
n_sector$All_Postcodes <- n_sector$All_2020 / n_sector$All_2019
n_sector$EOTHO_Postcodes <- n_sector$EOTHO_2020 / n_sector$EOTHO_2019

#Index ratios.
index_sector <- function(s, c) {
  (n_sector[[s,c]] / n_sector[which(n_sector$Week==23 & n_sector$SectionAbb == n_sector[[s,1]]),c])[[1]]
}
n_sector$iAll <- vapply(seq(1,nrow(n_sector)), index_sector, c=7, FUN.VALUE=numeric(1))
n_sector$iEOTHO <- vapply(seq(1,nrow(n_sector)), index_sector, c=8, FUN.VALUE=numeric(1))

## Figures ----
write.csv(period2_sectors[c(1,8,9,10)], "Figures/Fig13.csv", row.names=FALSE)
write.csv(n_sector[c(1,2,9,10)], "Figures/Fig12.csv", row.names=FALSE)

