# ---
# Eat Out to Help Out
# Output: analysis by NUTS1 region of registrations in other sectors
# July 2021
# Contact: jph37@kent.ac.uk; i.galanakis@kent.ac.uk
# ---

# Load libraries ----
library('lubridate')
library('dplyr')
library('tidyr')
library('plotly')

## Import and prepare data----
#Incorporations data.

repD <- read.csv("BasicCompanyDataAsOneFile-2020-10-01.csv")
#Re-format incorporation date
repD$IncorporationDate <- as.Date(repD$IncorporationDate, "%d/%m/%Y")
#Select records from target period.
repDEO2HO <- repD[which(repD$IncorporationDate >= "2020-06-01" & repD$IncorporationDate <= "2020-10-01"),]
#Select records from comparison period.
repDBaseline <- repD[which(repD$IncorporationDate >= "2019-06-01" & repD$IncorporationDate <= "2019-10-01"),]
#Save files for future convenience.
write.csv(repDEO2HO,'EO2HOdata.csv')
write.csv(repDBaseline,'Baselinedata.csv')
#Read saved files.
repDEO2HO <- read.csv("EO2HOdata.csv")
repDBaseline <- read.csv("Baselinedata.csv")

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

#Participating restaurants data.
restaurants <- read.csv("restaurants.csv")
restaurants$Postcode <- toupper(gsub(" ", "", restaurants$Postcode))
#Keep only postcode district.
restaurants$PostcodeDistrict <- sub("...$", "", restaurants$Postcode)
#Keep only postcode area.
restaurants$PostcodeArea <- sub("[0-9]+(.*)", "", restaurants$PostcodeDistrict)
restaurants$Name <- toupper(restaurants$Name)

#Import NUTS conversion table.
pc2NUTS <- read.csv("UKPC2NUTS.csv")
pc2NUTS <- pc2NUTS[c(3,7)] %>% distinct() %>% rename(Postcode = postcodeUnit, NUTS1=NUTS118CD)
pc2NUTS$Postcode <- toupper(gsub(" ", "", pc2NUTS$Postcode))

#Add NUTS1 column to data.
repDEO2HO <- left_join(x=repDEO2HO, y=pc2NUTS, by="Postcode")
repDBaseline <- left_join(x=repDBaseline, y=pc2NUTS, by="Postcode")
restaurants <- left_join(x=restaurants, y=pc2NUTS, by="Postcode")
nuts1_names <- data.frame(Name=c("North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland"), NUTS1=c("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", "UKM", "UKN"))


## Timeseries----
#Count incorporations by week and NUTS1
#2020
#All postcodes.
i_NUTS1 <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56))),] %>% group_by(NUTS1, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`, All_2020=n) %>% ungroup() %>% filter(between(Week, 23, 39))
#EO2HO postcodes.
i_NUTS1 <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56)) & repDEO2HO$Postcode %in% restaurants$Postcode),] %>% group_by(NUTS1, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`, EO2HO_2020=n) %>% ungroup() %>% filter(between(Week, 23, 39)) %>% merge(i_NUTS1, ., by=c("Week", "NUTS1"))
#2019
#All postcodes.
i_NUTS1 <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56))),] %>% group_by(NUTS1, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`, All_2019=n) %>% ungroup() %>% filter(between(Week, 23, 39)) %>% merge(i_NUTS1, ., by=c("Week", "NUTS1"))
#EO2HO postcodes.
i_NUTS1 <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56)) & repDBaseline$Postcode %in% restaurants$Postcode),] %>% group_by(NUTS1, week(IncorporationDate)) %>% count() %>%
  rename(Week=`week(IncorporationDate)`, EO2HO_2019=n) %>% ungroup() %>% filter(between(Week, 23, 39)) %>% merge(i_NUTS1, ., by=c("Week", "NUTS1"))

#Add names for NUTS1 regions.
i_NUTS1 <- i_NUTS1 %>% left_join(nuts1_names, ., by="NUTS1")

#Calculate ratios.
i_NUTS1$All_Postcodes <- i_NUTS1$All_2020 / i_NUTS1$All_2019
i_NUTS1$EO2HO_Postcodes <- i_NUTS1$EO2HO_2020 / i_NUTS1$EO2HO_2019

#Index ratios.
index_NUTS1 <- function(r, c) {
  (i_NUTS1[r,c] / i_NUTS1[which(i_NUTS1$Week==23 & i_NUTS1$NUTS1 == i_NUTS1[r,2]),c])[[1]]
}
i_NUTS1$iAll <- sapply(seq(1,nrow(i_NUTS1)), index_NUTS1, c=8)
i_NUTS1$iEO2HO <- sapply(seq(1,nrow(i_NUTS1)), index_NUTS1, c=9)

#Calculate difference between ratios. (Effect of EO2HO.)
i_NUTS1$Diff <- i_NUTS1$EO2HO_Postcodes - i_NUTS1$All_Postcodes
#Index difference
i_NUTS1$iDiff <- i_NUTS1$iEO2HO - i_NUTS1$iAll

## Plot timeseries----
#Create timeseries plots for each region.
plot_region <- function (region) {
  plot_ly(data=i_NUTS1[which(i_NUTS1$Name==region),], x=~Week, y=~iAll, type='scatter', mode='lines', name="All Postcodes", color=I("#1f77b4"), showlegend=FALSE) %>% 
    add_trace(y=~iEO2HO, type='scatter', mode='lines', name="EO2HO Postcodes", color=I("#ff7f0e"), showlegend=FALSE) %>%
    add_annotations(
      text = region,
      x = 31,
      y = ~(max(iEO2HO, na.rm=TRUE)+0.5),
      yref = "y",
      xref = "x",
      showarrow = FALSE) %>%
    layout(title=list(text=paste0("Ratio of registrations (2020/2019) in All Postcodes and EO2HO Postcodes",
                                  '<br>',
                                  '<sup>',
                                  "Indexed from week 23",
                                  '</sup>')),
           xaxis=list(range=c(23,39)),
           shapes=list(list(type='line', x0=yday("2020-07-08")/7, x1=yday("2020-07-08")/7, y0=0, y1=~(max(iEO2HO, na.rm=TRUE)+0.3), opacity=0.2),
                       list(type='rect', x0=yday("2020-08-01")/7, x1=yday("2020-08-31")/7, y0=0, y1=~(max(iEO2HO, na.rm=TRUE)+0.3), fillcolor="grey", line = list(color = "grey"), opacity=0.2)))
}
region_plots <- lapply(nuts1_names$Name, plot_region)
regionplot <- subplot(region_plots, nrows=3)


#Plot for difference of ratios (EO2HO effect).
plot_ly(i_NUTS1, x=~Week) %>% add_lines(color=~NUTS1, y=~Diff, name=~Name, colors="Set3") %>% 
  add_lines(color=~NUTS1, y=~iDiff, name=~Name, colors="Set3", visible=FALSE) %>% 
  layout(title=list(text = paste0("Effect of EO2HO on Other sectors (excl. Accommodation and Food) by NUTS1 region",
                                  '<br>',
                                  '<sup>',
                                  '(Difference in ratio of registrations (2020/2019) between EO2HO postcodes and all postcodes)',
                                  '</sup>')), 
         yaxis=list(title="Difference in ratio of weekly registrations"), 
         xaxis=list(title="Week"),
         shapes=list(list(type='line', x0=yday("2020-07-08")/7, x1=yday("2020-07-08")/7, y0=-1, y1=3, opacity=0.5),
                     list(type='rect', x0=yday("2020-08-01")/7, x1=yday("2020-08-31")/7, y0=-1, y1=3, fillcolor="grey", line = list(color = "grey"), opacity=0.3)),
         updatemenus=list(list(buttons=list(
           list(label="Actual difference", method="update",
                args=list(list(visible=c(rep(TRUE,12), rep(FALSE,12))))),
           list(label="Indexed difference", method="update",
                args=list(list(visible=c(rep(FALSE,12), rep(TRUE,12)))))
         )))) %>% 
  add_annotations(data=dateLabels, x=~(yday(x)/7), y=~(y-1), text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)

##Totals for Period2: 8th July to 1st August----
#Total registrations for each region in 2020, All postcodes.
period2_NUTS1 <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56)) & between(as.Date(repDEO2HO$IncorporationDate), as.Date("2020-07-08"), as.Date("2020-08-01"))),] %>% group_by(NUTS1) %>% count() %>% rename(All_2020=n)
#EO2HO postcodes.
period2_NUTS1 <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56)) & between(as.Date(repDEO2HO$IncorporationDate), as.Date("2020-07-08"), as.Date("2020-08-01")) & repDEO2HO$Postcode %in% restaurants$Postcode),] %>% group_by(NUTS1) %>% count() %>%
  merge(period2_NUTS1, ., by="NUTS1") %>% rename(EO2HO_2020=n)
#2019
period2_NUTS1 <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56)) & between(as.Date(repDBaseline$IncorporationDate), as.Date("2019-07-08"), as.Date("2019-08-01"))),] %>% group_by(NUTS1) %>% count() %>%
  merge(period2_NUTS1, ., by="NUTS1") %>% rename(All_2019=n)
period2_NUTS1 <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56)) & between(as.Date(repDBaseline$IncorporationDate), as.Date("2019-07-08"), as.Date("2019-08-01")) & repDBaseline$Postcode %in% restaurants$Postcode),] %>% group_by(NUTS1) %>% count() %>%
  merge(period2_NUTS1, ., by="NUTS1") %>% rename(EO2HO_2019=n)
#Calculate ratios.
period2_NUTS1$All_Postcodes <- period2_NUTS1$All_2020 / period2_NUTS1$All_2019
period2_NUTS1$EO2HO_Postcodes <- period2_NUTS1$EO2HO_2020 / period2_NUTS1$EO2HO_2019
#Calculate difference.
period2_NUTS1$Diff <- period2_NUTS1$EO2HO_Postcodes - period2_NUTS1$All_Postcodes

#Remove NA region.
period2_NUTS1 <- period2_NUTS1[which(!(is.na(period2_NUTS1$NUTS1))),]
#Add names.
period2_NUTS1 <- period2_NUTS1 %>% left_join(nuts1_names, ., by="NUTS1")

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

#Apply bootstrap CI function.
boot_ci_nuts1 <- function(i) {boot_ci(period2_NUTS1$All_2020[i], period2_NUTS1$All_2019[i], period2_NUTS1$EO2HO_2020[i], period2_NUTS1$EO2HO_2019[i])}
period2_NUTS1$CI <- lapply(seq(1,12), boot_ci_nuts1)
period2_NUTS1 <- period2_NUTS1 %>% unnest_wider(col=CI)

#Order by effect.
period2_NUTS1 <- period2_NUTS1 %>% arrange(desc(period2_NUTS1$Diff))
period2_NUTS1$Name <- factor(period2_NUTS1$Name, levels=c(as.character(period2_NUTS1$Name)))

#Identify significant effects.
significant_NUTS1 <- rep(0.5, nrow(period2_NUTS1))
significant_NUTS1[which(period2_NUTS1$Lo > 0 | period2_NUTS1$Hi <0)] <- 1

#Barplot for effect.
regionbar <- plot_ly(data=period2_NUTS1) %>% add_bars(x=~Name, y=~Diff, showlegend=FALSE,
                                         marker=list(opacity=significant_NUTS1)) %>%
  add_segments(x=~Name, xend=~Name, y=~Lo, yend=~Hi, name="95% Confidence Interval", color=I("black"), opacity=0.6) %>%
  layout(title=list(text=paste0("Spillover effect between announcement and start of EO2HO by region",
                                '<br>',
                                '<sup>',
                                "Ratio of registrations in EO2HO Postcodes minus All Postcodes",
                                '</sup>')),
         yaxis=list(title="Difference in ratio of registrations (2020/2019)"),
         xaxis=list(title="Region"))


##Maps----
#Map of difference (effect of EO2HO).
library('raster')
library('rgeos')
library('rgdal')
library('maptools')
shp <- readOGR('Map/NUTS_Level_1_(January_2018)_Boundaries.shp')
shp1 <- fortify(shp, region="nuts118cd")
shp1 <- merge(shp1, period2_NUTS1, by.x='id', by.y='NUTS1', all.x=TRUE) %>% arrange(order)
#Add column for significant regions.
shp1$Significant <- FALSE
shp1$Significant[which(shp1$Lo > 0 | shp1$Hi < 0)] <- TRUE
shp1 <- shp1 %>% rename(Effect=Diff)
p <- ggplot(data=shp1, aes(x=long, y=lat, group=group, fill=Effect, alpha=Significant)) + geom_polygon() + coord_equal() + theme_void() + 
  labs(fill="Difference in ratio of registrations")  + 
  scale_fill_gradient2(low="#a2c5f5", mid="#e1e1e1", high="#f7b231", midpoint=0) + scale_alpha_manual(values=c(0.4, 1), guide="none")

figure8 <- ggplotly(p, tooltip="fill") %>% layout(xaxis=list(showline=FALSE, showgrid=FALSE), 
                       yaxis=list(showline=FALSE, showgrid=FALSE)) %>% hide_legend()


#Count number of participating restaurants in each region.
r_region <- restaurants %>% group_by(NUTS1) %>% count()
write.csv(r_region[c(1,2,3)], "Fig1.csv", row.names=FALSE)
shp2 <- merge(shp1, r_region, by.x='id', by.y='NUTS1', all.x=TRUE)
#Map of restaurants.
map_rest <- ggplot(data=shp1, aes(x=long, y=lat, group=group, fill=n)) + geom_polygon() + coord_equal() + 
  theme_void() + labs(fill="Number") + scale_fill_gradient(low="#e1e1e1", high="#f7b231")
map_rest

#Map of ratio of registrations.
map_ratio <- ggplot(data=shp2, aes(x=long, y=lat, group=group, fill=All_Postcodes)) + geom_polygon() + coord_equal() + theme_void() + 
  labs(title="Ratio of registrations in Other sectors", subtitle="(Between announcement and start of EO2HO)", fill="Relative to 2019") + scale_fill_gradient(low="#dff2cb", high="#f7b231")
map_ratio
#Show map of restaurants and map of ratio of registrations.
library('gridExtra')
grid.arrange(map_rest, map_ratio, ncol=2)
figure1 <- ggplotly(map_rest, tooltip="fill") %>% layout(xaxis=list(showline=FALSE, showgrid=FALSE), 
                                             yaxis=list(showline=FALSE, showgrid=FALSE)) %>% hide_legend()


##Linear relationship----
#Is there a linear relationship between the number of restaurants and the ratio of registrations in each region?
#NUTS1 level.
r_spillover_lm <- lm(arrange(period2_NUTS1, period2_NUTS1$NUTS1)$All_Postcodes ~ arrange(r_region, r_region$NUTS1)$n)
summary(r_spillover_lm)

eo2hoVratio <- plot_ly(x=arrange(r_region, r_region$NUTS1)$n, y=arrange(period2_NUTS1, period2_NUTS1$NUTS1)$All_Postcodes, text=nuts1_names$Name, type='scatter', mode='text+markers', textposition="middle right", name="") %>% 
  add_trace(y=(arrange(r_region, r_region$NUTS1)$n*r_spillover_lm$coefficients[2]+r_spillover_lm$coefficients[1]), type='scatter', mode='lines', showlegend=FALSE, name="") %>% 
  layout(title=list(text = "EO2HO restaurants vs Ratio of registrations in Other sectors"), 
         yaxis=list(title="Ratio of registrations (2020/2019)"), 
         xaxis=list(title="Number of EO2HO restaurants"))


#Postcode area level.
#Start with list of real postcode areas.
areas <- read.csv("Postcodes summary.csv")
#Keep postcodes and population.
areas <- areas[c(1,3)]
#Count registrations per PC area.
period2_Area <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56)) & between(as.Date(repDEO2HO$IncorporationDate), as.Date("2020-07-08"), as.Date("2020-08-01"))),] %>% group_by(PostcodeArea) %>% count() %>% rename(n2020=n)
period2_Area <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56)) & between(as.Date(repDBaseline$IncorporationDate), as.Date("2019-07-08"), as.Date("2019-08-01"))),] %>% group_by(PostcodeArea) %>% count() %>%
  merge(period2_Area, ., by="PostcodeArea") %>% rename(n2019=n)
period2_Area$Ratio <- period2_Area$n2020 / period2_Area$n2019
#Count restaurants per area.
period2_Area <- restaurants %>% group_by(PostcodeArea) %>% count() %>% merge(period2_Area, ., by="PostcodeArea") %>% rename(R=n)
#Merge with list of real postcodes to remove incorrect ones.
period2_Area <- merge(x=areas, y=period2_Area, by.x="Postcode.area", by.y="PostcodeArea") %>% rename(PostcodeArea=Postcode.area)

eo2hoVratio2 <- plot_ly(data=period2_Area, x=~R, y=~Ratio, type='scatter', mode='markers', text=~PostcodeArea) %>%
  layout(title=list(text="EO2HO restaurants vs Ratio of registrations in Other sectors"),
         yaxis=list(title="Ratio of registrations (2020/2019)"), 
         xaxis=list(title="Number of EO2HO restaurants"))

plot_ly(data=period2_Area[which(between(period2_Area$R, quantile(period2_Area$R, 0.05)[[1]], quantile(period2_Area$R, 0.95)[[1]])),], x=~R, y=~Ratio, type='scatter', mode='markers', text=~PostcodeArea) %>%
  layout(title=list(text="EO2HO restaurants vs Ratio of registrations in Other sectors"),
         yaxis=list(title="Ratio of registrations (2020/2019)"), 
         xaxis=list(title="Number of EO2HO restaurants"))

#1. Ratio of registrations ~ Number of restaurants
rest_lm1 <- lm(data=period2_Area, Ratio~R)
summary(rest_lm1)

rest_lm1.1 <- lm(data=period2_Area[which(between(period2_Area$R, quantile(period2_Area$R, 0.05), quantile(period2_Area$R, 0.95))),], Ratio~R)
summary(rest_lm1.1)

#2. Ratio of registrations ~ Restaurants per capita
period2_Area$Density <- period2_Area$R / period2_Area$Population * 1000
rest_lm2 <- lm(data=period2_Area[which(period2_Area$Density < 4),], Ratio~Density)
summary(rest_lm2)

plot_ly(data=period2_Area[which(period2_Area$Density < 4),], x=~Density, y=~Ratio, type='scatter', mode='markers', text=~PostcodeArea) %>%
  layout(title=list(text="EO2HO restaurants vs Ratio of registrations in Other sectors"),
         yaxis=list(title="Ratio of registrations (2020/2019)"), 
         xaxis=list(title="Number of EO2HO restaurants per 1000 residents"))

#3. Ratio of registrations ~ Restaurants per company
c_area <- repD %>% group_by(PostcodeArea) %>% count()
c_area <- merge(x=areas, y=c_area, by.x="Postcode.area", by.y="PostcodeArea") %>% rename(PostcodeArea=Postcode.area, Companies=n) 
#c_area <- read.csv("Companies per PC area.csv")
period2_Area <- merge(period2_Area, c_area[c(2,4)], by="PostcodeArea")
period2_Area$PerCo <- period2_Area$R / period2_Area$Companies
rest_lm3 <- lm(data=period2_Area, Ratio~PerCo)
summary(rest_lm3)
