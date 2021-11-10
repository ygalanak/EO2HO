# ---
# Eat Out to Help Out
# Output: calculates registrations of companies in hospitality and other sectors, daily weekly and relative to 2019
# November 2021
# Contact: jph37@kent.ac.uk; i.galanakis@kent.ac.uk
# ---

# Load libraries ----
library('lubridate')
library('dplyr')
library('tidyr')
library('plotly')
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

##Totals (all sectors)----
#01-06-2020 to 31-08-2020. All
sum(repDEOTHO$EOTHO)
# 36972/225749
# 0.1637748
sum(!(repDEOTHO$EOTHO))
# 188777/225749
# 0.8362252
#Pre-announcement
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-07-07")))
sum(repDEOTHO$EOTHO[between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-07-07"))])
# 15510/96382
# 0.1609222
sum(!(repDEOTHO$EOTHO[between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-07-07"))]))
# 80872/96382
# 0.8390778
#Post-announcement
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31")))
sum(repDEOTHO$EOTHO[between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31"))])
# 21462/129367
# 0.1659001
sum(!(repDEOTHO$EOTHO[between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31"))]))
# 107905/129367
# 0.8340999

##Weekly totals----
#Accommodation and Food.
#Weekly counts for new incorporations in SIC 55-56.
w_incorps <- repDEOTHO[which(repDEOTHO$SIC2dg1 %in% list(55,56)),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`)
w_incorps <- repDBaseline[which(repDBaseline$SIC2dg1 %in% list(55,56)),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`) %>% left_join(x=w_incorps, y=., by="Week") %>% 
  rename(All_2020=n.x,All_2019=n.y)
#Now the same for EOTHO postcodes only.
w_incorps <- repDEOTHO[which(repDEOTHO$SIC2dg1 %in% list(55,56) & repDEOTHO$EOTHO == 1),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`) %>% left_join(x=w_incorps, y=., by="Week")
w_incorps <- repDBaseline[which(repDBaseline$SIC2dg1 %in% list(55,56) & repDBaseline$EOTHO == 1),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`) %>% left_join(x=w_incorps, y=., by="Week") %>% 
  rename(EOTHO_2020=n.x,EOTHO_2019=n.y)
#Non-EOTHO postcodes only.
w_incorps <- repDEOTHO[which(repDEOTHO$SIC2dg1 %in% list(55,56) & repDEOTHO$EOTHO == 0),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`) %>% left_join(x=w_incorps, y=., by="Week")
w_incorps <- repDBaseline[which(repDBaseline$SIC2dg1 %in% list(55,56) & repDBaseline$EOTHO == 0),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`) %>% left_join(x=w_incorps, y=., by="Week") %>% 
  rename(Non_EOTHO_2020=n.x,Non_EOTHO_2019=n.y)
#The first week not full, so we remove it.
w_incorps <- w_incorps[c(2:14),]
#Ratio of registrations.
w_incorps$All_Postcodes <- w_incorps$All_2020 / w_incorps$All_2019
w_incorps$EOTHO_Postcodes <- w_incorps$EOTHO_2020 / w_incorps$EOTHO_2019
w_incorps$Non_EOTHO_Postcodes <- w_incorps$Non_EOTHO_2020 / w_incorps$Non_EOTHO_2019
w_incorps <- w_incorps %>% ungroup()

#Table of weekly averages.
#Pre-, post-announcement variable.
w_incorps$Period <- c(rep("Pre", 6), rep("Post", 7))
w_incorps %>% aggregate(by = list(w_incorps$Period), mean)
w_incorps %>% aggregate(by = list(w_incorps$Period), sd)
length(unique(restaurants$Postcode))
length(unique(repDEOTHO$Postcode)) - length(unique(restaurants$Postcode))

#Index against week 22.
w_incorps$iAll_2020 <- w_incorps$All_2020 / w_incorps$All_2020[1]
w_incorps$iAll_2019 <- w_incorps$All_2019 / w_incorps$All_2019[1]
w_incorps$iEOTHO_2020 <- w_incorps$EOTHO_2020 / w_incorps$EOTHO_2020[1]
w_incorps$iEOTHO_2019 <- w_incorps$EOTHO_2019 / w_incorps$EOTHO_2019[1]
w_incorps$iAll <- w_incorps$iAll_2020 / w_incorps$iAll_2019
w_incorps$iEOTHO <- w_incorps$iEOTHO_2020 / w_incorps$iEOTHO_2019

#Other sectors.
#Weekly counts, all postcodes.
w_incorps_all <- repDEOTHO[which(!(repDEOTHO$SIC2dg1 %in% list(55,56))),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, All_2020=n)
w_incorps_all <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56))),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, All_2019=n) %>% left_join(x=w_incorps_all, y=., by="Week")
#Now the same for EOTHO postcodes only.
w_incorps_all <- repDEOTHO[which(!(repDEOTHO$SIC2dg1 %in% list(55,56)) & repDEOTHO$EOTHO == 1),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, EOTHO_2020=n) %>% left_join(x=w_incorps_all, y=., by="Week")
w_incorps_all <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56)) & repDBaseline$EOTHO == 1),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, EOTHO_2019=n) %>% left_join(x=w_incorps_all, y=., by="Week")
#Non-EOTHO postcodes only.
w_incorps_all <- repDEOTHO[which(!(repDEOTHO$SIC2dg1 %in% list(55,56)) & repDEOTHO$EOTHO == 0),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, Non_EOTHO_2020=n) %>% left_join(x=w_incorps_all, y=., by="Week")
w_incorps_all <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56)) & repDBaseline$EOTHO == 0),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, Non_EOTHO_2019=n) %>% left_join(x=w_incorps_all, y=., by="Week")
w_incorps_all <- w_incorps_all[c(2:14),]
#Ratio of registrations.
w_incorps_all$All_Postcodes <- w_incorps_all$All_2020 / w_incorps_all$All_2019
w_incorps_all$EOTHO_Postcodes <- w_incorps_all$EOTHO_2020 / w_incorps_all$EOTHO_2019
w_incorps_all$Non_EOTHO_Postcodes <- w_incorps_all$Non_EOTHO_2020 / w_incorps_all$Non_EOTHO_2019
w_incorps_all <- w_incorps_all %>% ungroup()

#Table of weekly averages.
#Pre-, post-announcement variable.
w_incorps_all$Period <- c(rep("Pre", 6), rep("Post", 7))
w_incorps_all %>% aggregate(by = list(w_incorps_all$Period), mean)
w_incorps_all %>% aggregate(by = list(w_incorps_all$Period), sd)

#Index against week 22.
w_incorps_all$iAll_2020 <- w_incorps_all$All_2020 / w_incorps_all$All_2020[1]
w_incorps_all$iAll_2019 <- w_incorps_all$All_2019 / w_incorps_all$All_2019[1]
w_incorps_all$iEOTHO_2020 <- w_incorps_all$EOTHO_2020 / w_incorps_all$EOTHO_2020[1]
w_incorps_all$iEOTHO_2019 <- w_incorps_all$EOTHO_2019 / w_incorps_all$EOTHO_2019[1]
w_incorps_all$iAll <- w_incorps_all$iAll_2020 / w_incorps_all$iAll_2019
w_incorps_all$iEOTHO <- w_incorps_all$iEOTHO_2020 / w_incorps_all$iEOTHO_2019


##Totals----
#01-06-2020 to 31-08-2020. All
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-08-31")))
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-08-31")) & repDEOTHO$EOTHO == 1)
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-08-31")) & repDEOTHO$EOTHO == 0)
#Pre-announcement
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-07-07")))
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-06-01"), as.Date("2020-07-07")) & repDEOTHO$EOTHO == 1)
#Post-announcement
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31")))
sum(between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31")) & repDEOTHO$EOTHO == 1)

##Spillover effect----
#Difference between EO2HO and all postcodes ratios.
w_incorps$Diff <- w_incorps$EOTHO_Postcodes - w_incorps$All_Postcodes
w_incorps_all$Diff <- w_incorps_all$EOTHO_Postcodes - w_incorps_all$All_Postcodes

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
#Total spillover effect (08-07-2020 to 01-08-2020)
#Count the number of registrations on the given dates for EOTHO_2020, EOTHO_2019, All_2020, All_2019 and use the formula for 'Effect'.
(sum(repDEOTHO$EOTHO[between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31"))]) /
    sum(repDBaseline$EOTHO[between(repDBaseline$IncorporationDate, as.Date("2019-07-08"), as.Date("2019-08-31"))])) -
  (sum(between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31"))) /
     sum(between(repDBaseline$IncorporationDate, as.Date("2019-07-08"), as.Date("2019-08-31"))))

#Non-hospitality
#(sum(repDEOTHO$EOTHO[between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31")) & !(repDEOTHO$SIC2dg1 %in% list(55,56))]) /
#    sum(repDBaseline$EOTHO[between(repDBaseline$IncorporationDate, as.Date("2019-07-08"), as.Date("2019-08-31")) & !(repDBaseline$SIC2dg1 %in% list(55,56))])) -
#  (sum(between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31")) & !(repDEOTHO$SIC2dg1 %in% list(55,56))) /
#     sum(between(repDBaseline$IncorporationDate, as.Date("2019-07-08"), as.Date("2019-08-31")) & !(repDBaseline$SIC2dg1 %in% list(55,56))))
sum(w_incorps_all$EOTHO_2020[7:13]) / sum(w_incorps_all$EOTHO_2019[7:13]) - 
  sum(w_incorps_all$All_2020[7:13]) / sum(w_incorps_all$All_2019[7:13])

#Apply the bootstrap algorithm.
#boot_ci(sum(between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31")) & !(repDEOTHO$SIC2dg1 %in% list(55,56))),
#          sum(between(repDBaseline$IncorporationDate, as.Date("2019-07-08"), as.Date("2019-08-31")) & !(repDBaseline$SIC2dg1 %in% list(55,56))),
#        sum(repDEOTHO$EOTHO[between(repDEOTHO$IncorporationDate, as.Date("2020-07-08"), as.Date("2020-08-31")) & !(repDEOTHO$SIC2dg1 %in% list(55,56))]),
#          sum(repDBaseline$EOTHO[between(repDBaseline$IncorporationDate, as.Date("2019-07-08"), as.Date("2019-08-31")) & !(repDBaseline$SIC2dg1 %in% list(55,56))]))
boot_ci(sum(w_incorps_all$All_2020[7:13]), sum(w_incorps_all$All_2019[7:13]),
        sum(w_incorps_all$EOTHO_2020[7:13]), sum(w_incorps_all$EOTHO_2019[7:13]))

#Hypothetical registrations in EOTHO areas without the scheme.
w_incorps_all$EOTHOxNon <- w_incorps_all$EOTHO_2019 * w_incorps_all$Non_EOTHO_Postcodes
sum(w_incorps_all$EOTHO_2020[7:13]-w_incorps_all$EOTHOxNon[7:13])
#Until start of scheme
sum(w_incorps_all$EOTHO_2020[7:9]-w_incorps_all$EOTHOxNon[7:9])
#During/after scheme
sum(w_incorps_all$EOTHO_2020[10:13]-w_incorps_all$EOTHOxNon[10:13])

##Figures----
#Weekly registrations in non-hospitality.
fig5 <- w_incorps_all[c(1,8,9,16,17)]
fwrite(fig5, "Figures/Fig5.csv", row.names=F)

#Spillover in hospitality and non-hospitality.
fig6 <- w_incorps[c(1,18)] %>% rename(HospDiff = Diff)
fig6$OtherDiff <- w_incorps_all[[18]]
fwrite(fig6, "Figures/Fig6.csv", row.names=F)
