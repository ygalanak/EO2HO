# ---
# Eat Out to Help Out
# Output: calculates registrations of companies in hospitality and other sectors, daily weekly and relative to 2019
# July 2021
# Contact: jph37@kent.ac.uk; i.galanakis@kent.ac.uk
# ---

# Load libraries ----
library('lubridate')
library('dplyr')
library('tidyr')
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


##Daily totals and rolling averages----
#Accommodation and food
#Get daily counts for all registrations in 2020.
n_incorps <- repDEO2HO[which(repDEO2HO$SIC2dg1 %in% list(55,56)),] %>% group_by(as.Date(IncorporationDate)) %>% count() %>% 
  rename(Date=`as.Date(IncorporationDate)`, All_2020=n)
total_days <- tibble(seq(as.Date("2020-06-01"), as.Date("2020-10-01"), by = "days")) %>% rename(Date = `seq(as.Date("2020-06-01"), as.Date("2020-10-01"), by = "days")`)
n_incorps <- full_join(x=total_days, y=n_incorps, by="Date") %>% mutate(Day=yday(Date))
#Add baseline counts from 2019.
#2020 was a leap year so add 1 to yday to match dates.
n_incorps <- repDBaseline[which(repDBaseline$SIC2dg1 %in% list(55,56)),] %>% group_by(as.Date(IncorporationDate)) %>% count() %>% 
  rename(Date=`as.Date(IncorporationDate)`, All_2019=n) %>% mutate(Day=yday(Date) + 1) %>% left_join(x=n_incorps, y=., by="Day")
#Now the same for EO2HO postcodes only.
n_incorps <- repDEO2HO[which(repDEO2HO$SIC2dg1 %in% list(55,56) & repDEO2HO$Postcode %in% restaurants$Postcode),] %>% 
  group_by(as.Date(IncorporationDate)) %>% count() %>% 
  rename(Date=`as.Date(IncorporationDate)`, EO2HO_2020=n) %>% mutate(Day=yday(Date)) %>% left_join(x=n_incorps, y=., by="Day")
#And those postcodes in 2019.
n_incorps <- repDBaseline[which(repDBaseline$SIC2dg1 %in% list(55,56) & repDBaseline$Postcode %in% restaurants$Postcode),] %>% 
  group_by(as.Date(IncorporationDate)) %>% count() %>% 
  rename(Date=`as.Date(IncorporationDate)`, EO2HO_2019=n) %>% mutate(Day=yday(Date) + 1) %>% left_join(x=n_incorps, y=., by="Day")
#Discard the Date columns.
n_incorps <- n_incorps[c(3,2,5,7,9)]
#Pivot table so that missing values and calculations can be added more easily.
n_incorps <- n_incorps %>% pivot_longer(cols=!Day, names_to="Subset", values_to="N") %>% arrange(Subset)
#Replace missing values with 0.
n_incorps$N <- replace_na(n_incorps$N, 0)
#Calculate 7-day rolling average.
n_incorps$RA <- stats::filter(n_incorps$N, rep(1/7,7), sides=1)
#This has introduced nonsense for the first 6 days due to overlapping with other subsets, so clip this off.
n_incorps <- n_incorps[which(n_incorps$Day >= yday("2020-06-07")),]
#Pivot back.
n_incorps <- n_incorps %>% pivot_wider(names_from=Subset, values_from=RA, id_cols=Day)
#Ratio for all postcodes.
n_incorps$All_Postcodes <- n_incorps$All_2020 / n_incorps$All_2019
#Ratio for EO2HO postcodes.
n_incorps$EO2HO_Postcodes <- n_incorps$EO2HO_2020 / n_incorps$EO2HO_2019

#Other sectors (excluding Accommodation and Food).
#Get daily counts for all registrations in 2020.
n_incorps_all <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56))),] %>% group_by(as.Date(IncorporationDate)) %>% count() %>% 
  rename(Date=`as.Date(IncorporationDate)`, All_2020=n)
n_incorps_all <- full_join(x=total_days, y=n_incorps_all, by="Date") %>% mutate(Day=yday(Date))
#Add baseline counts from 2019.
#2020 was a leap year so add 1 to yday to match dates.
n_incorps_all <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56))),] %>% group_by(as.Date(IncorporationDate)) %>% count() %>% 
  rename(Date=`as.Date(IncorporationDate)`, All_2019=n) %>% mutate(Day=yday(Date) + 1) %>% left_join(x=n_incorps_all, y=., by="Day")
#Now the same for EO2HO postcodes only.
n_incorps_all <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56)) & repDEO2HO$Postcode %in% restaurants$Postcode),] %>% 
  group_by(as.Date(IncorporationDate)) %>% count() %>% 
  rename(Date=`as.Date(IncorporationDate)`, EO2HO_2020=n) %>% mutate(Day=yday(Date)) %>% left_join(x=n_incorps_all, y=., by="Day")
#And those postcodes in 2019.
n_incorps_all <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56)) & repDBaseline$Postcode %in% restaurants$Postcode),] %>% 
  group_by(as.Date(IncorporationDate)) %>% count() %>% 
  rename(Date=`as.Date(IncorporationDate)`, EO2HO_2019=n) %>% mutate(Day=yday(Date) + 1) %>% left_join(x=n_incorps_all, y=., by="Day")
#Discard the Date columns.
n_incorps_all <- n_incorps_all[c(3,2,5,7,9)]
#Pivot table so that missing values and calculations can be added more easily.
n_incorps_all <- n_incorps_all %>% pivot_longer(cols=!Day, names_to="Subset", values_to="N") %>% arrange(Subset)
#Replace missing values with 0.
n_incorps_all$N <- replace_na(n_incorps_all$N, 0)
#Calculate 7-day rolling average.
n_incorps_all$RA <- stats::filter(n_incorps_all$N, rep(1/7,7), sides=1)
#This has introduced nonsense for the first 6 days due to overlapping with other subsets, so clip this off.
n_incorps_all <- n_incorps_all[which(n_incorps_all$Day >= yday("2020-06-07")),]
#Pivot back.
n_incorps_all <- n_incorps_all %>% pivot_wider(names_from=Subset, values_from=RA, id_cols=Day)
#Ratio for all postcodes.
n_incorps_all$All_Postcodes <- n_incorps_all$All_2020 / n_incorps_all$All_2019
#Ratio for EO2HO postcodes.
n_incorps_all$EO2HO_Postcodes <- n_incorps_all$EO2HO_2020 / n_incorps_all$EO2HO_2019

##Weekly totals----
#Accommodation and Food.
#Weekly counts for new incorporations in SIC 55-56.
w_incorps <- repDEO2HO[which(repDEO2HO$SIC2dg1 %in% list(55,56)),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`)
w_incorps <- repDBaseline[which(repDBaseline$SIC2dg1 %in% list(55,56)),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`) %>% left_join(x=w_incorps, y=., by="Week") %>% 
  rename(All_2020=n.x,All_2019=n.y)
#Now the same for EO2HO postcodes only.
w_incorps <- repDEO2HO[which(repDEO2HO$SIC2dg1 %in% list(55,56) & repDEO2HO$Postcode %in% restaurants$Postcode),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`) %>% left_join(x=w_incorps, y=., by="Week")
w_incorps <- repDBaseline[which(repDBaseline$SIC2dg1 %in% list(55,56) & repDBaseline$Postcode %in% restaurants$Postcode),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`) %>% left_join(x=w_incorps, y=., by="Week") %>% 
  rename(EO2HO_2020=n.x,EO2HO_2019=n.y)
#The first and last weeks are not full, so we clip them off.
w_incorps <- w_incorps[c(2:18),]
#Ratio of registrations.
w_incorps$All_Postcodes <- w_incorps$All_2020 / w_incorps$All_2019
w_incorps$EO2HO_Postcodes <- w_incorps$EO2HO_2020 / w_incorps$EO2HO_2019
w_incorps <- w_incorps %>% ungroup()

#Index against week 23.
w_incorps$iAll_2020 <- w_incorps$All_2020 / w_incorps$All_2020[1]
w_incorps$iAll_2019 <- w_incorps$All_2019 / w_incorps$All_2019[1]
w_incorps$iEO2HO_2020 <- w_incorps$EO2HO_2020 / w_incorps$EO2HO_2020[1]
w_incorps$iEO2HO_2019 <- w_incorps$EO2HO_2019 / w_incorps$EO2HO_2019[1]
w_incorps$iAll <- w_incorps$iAll_2020 / w_incorps$iAll_2019
w_incorps$iEO2HO <- w_incorps$iEO2HO_2020 / w_incorps$iEO2HO_2019

#Other sectors.
#Weekly counts, all postcodes.
w_incorps_all <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56))),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, All_2020=n)
w_incorps_all <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56))),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, All_2019=n) %>% left_join(x=w_incorps_all, y=., by="Week")
#Now the same for EO2HO postcodes only.
w_incorps_all <- repDEO2HO[which(!(repDEO2HO$SIC2dg1 %in% list(55,56)) & repDEO2HO$Postcode %in% restaurants$Postcode),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, EO2HO_2020=n) %>% left_join(x=w_incorps_all, y=., by="Week")
w_incorps_all <- repDBaseline[which(!(repDBaseline$SIC2dg1 %in% list(55,56)) & repDBaseline$Postcode %in% restaurants$Postcode),] %>% group_by(week(as.Date(IncorporationDate))) %>% count() %>% 
  rename(Week=`week(as.Date(IncorporationDate))`, EO2HO_2019=n) %>% left_join(x=w_incorps_all, y=., by="Week")
w_incorps_all <- w_incorps_all[c(2:18),]
#Ratio of registrations.
w_incorps_all$All_Postcodes <- w_incorps_all$All_2020 / w_incorps_all$All_2019
w_incorps_all$EO2HO_Postcodes <- w_incorps_all$EO2HO_2020 / w_incorps_all$EO2HO_2019
w_incorps_all <- w_incorps_all %>% ungroup()
#Index against week 23.
w_incorps_all$iAll_2020 <- w_incorps_all$All_2020 / w_incorps_all$All_2020[1]
w_incorps_all$iAll_2019 <- w_incorps_all$All_2019 / w_incorps_all$All_2019[1]
w_incorps_all$iEO2HO_2020 <- w_incorps_all$EO2HO_2020 / w_incorps_all$EO2HO_2020[1]
w_incorps_all$iEO2HO_2019 <- w_incorps_all$EO2HO_2019 / w_incorps_all$EO2HO_2019[1]
w_incorps_all$iAll <- w_incorps_all$iAll_2020 / w_incorps_all$iAll_2019
w_incorps_all$iEO2HO <- w_incorps_all$iEO2HO_2020 / w_incorps_all$iEO2HO_2019


##Plots----
dateLabels <- data.frame(x=c(as.Date("2020-07-08"), as.Date("2020-08-16")), y=c(0, 0), t=c("Announcement", "EO2HO Period"))

#Plot 7-day rolling average and weekly registrations in Accommodation and Food.
plot_ly(data=n_incorps, x=~(as.Date("2019-12-31")+Day), y=~All_2019, name="2019 All Postcodes", type='scatter', mode='lines', color=I("#d62728")) %>%
  add_trace(data=n_incorps, x=~(as.Date("2019-12-31")+Day), y=~All_2020, name="2020 All Postcodes", type='scatter', mode='lines', color=I("#1f77b4")) %>%
  add_trace(data=n_incorps, x=~(as.Date("2019-12-31")+Day), y=~EO2HO_2019, name="2019 EO2HO Postcodes", type='scatter', mode='lines', color=I("#2ca02c")) %>%
  add_trace(data=n_incorps, x=~(as.Date("2019-12-31")+Day), y=~EO2HO_2020, name="2020 EO2HO Postcodes", type='scatter', mode='lines', color=I("#ff7f0e")) %>%
  add_trace(data=w_incorps, x=~(as.Date("2019-12-31")+Week*7), y=~All_2019, name="2019 All Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#d62728")) %>%
  add_trace(data=w_incorps, x=~(as.Date("2019-12-31")+Week*7), y=~All_2020, name="2020 All Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#1f77b4")) %>%
  add_trace(data=w_incorps, x=~(as.Date("2019-12-31")+Week*7), y=~EO2HO_2019, name="2019 EO2HO Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#2ca02c")) %>%
  add_trace(data=w_incorps, x=~(as.Date("2019-12-31")+Week*7), y=~EO2HO_2020, name="2020 EO2HO Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#ff7f0e")) %>%
  add_trace(data=w_incorps, x=~(as.Date("2019-12-31")+Week*7), y=~iAll_2019, name="2019 All Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#d62728")) %>%
  add_trace(data=w_incorps, x=~(as.Date("2019-12-31")+Week*7), y=~iAll_2020, name="2020 All Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#1f77b4")) %>%
  add_trace(data=w_incorps, x=~(as.Date("2019-12-31")+Week*7), y=~iEO2HO_2019, name="2019 EO2HO Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#2ca02c")) %>%
  add_trace(data=w_incorps, x=~(as.Date("2019-12-31")+Week*7), y=~iEO2HO_2020, name="2020 EO2HO Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#ff7f0e")) %>%
  layout(title=list(text="Registrations in Accommodation and Food sector"),
         xaxis=list(title="Date", type='date', tickformat="%d %b"),
         yaxis=list(title="Rolling average", range=c(-0.1,1)*ceiling(max(n_incorps[c(2:5)])/50)*50),
         updatemenus=list(list(buttons=list(list(method="update", label="7-day RA",
                                                 args=list(list(visible=c(rep(TRUE,4), rep(FALSE,8))),
                                                           list(yaxis=list(title="Rolling average", range=c(-0.1,1)*ceiling(max(n_incorps[c(2:5)])/50)*50)))),
                                            list(method="update", label="Weekly total",
                                                 args=list(list(visible=c(rep(FALSE,4), rep(TRUE,4), rep(FALSE,4))),
                                                           list(yaxis=list(title="Weekly total", range=c(-0.1,1)*ceiling(max(w_incorps[c(2:5)])/500)*500)))),
                                            list(method="update", label="Weekly total (indexed)",
                                                 args=list(list(visible=c(rep(FALSE,8), rep(TRUE,4))),
                                                           list(yaxis=list(title="Weekly total (Index week 23)", range=c(-0.1,1)*ceiling(max(w_incorps[c(8:11)])/0.2)*0.2))))
         ))),
         shapes=list(list(type='line', x0=as.Date("2020-07-08"), x1=as.Date("2020-07-08"), y0=0, y1=2000, opacity=0.5),
                     list(type='rect', x0=as.Date("2020-08-01"), x1=as.Date("2020-08-31"), y0=0, y1=2000, fillcolor="grey", line = list(color = "grey"), opacity=0.3))) %>% 
  add_annotations(data=dateLabels, x=~x, y=0, text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)

#Ratio of registrations in Accommodation and Food
figure4 <- plot_ly(data=w_incorps, x=~Week) %>% add_trace(y=~iAll, name="All Postcodes", type='scatter', mode='lines', color=I("#1f77b4")) %>% 
  add_trace(y=~iEO2HO, name="EO2HO Postcodes", type='scatter', mode='lines', color=I("#ff7f0e")) %>%
  add_trace(y=~All_Postcodes, name="All Postcodes", type='scatter', mode='lines', color=I("#1f77b4"), visible=FALSE) %>%
  add_trace(y=~EO2HO_Postcodes, name="EO2HO Postcodes", type='scatter', mode='lines', color=I("#ff7f0e"), visible=FALSE) %>%
  layout(title=list(text = paste0("Ratio of registrations in Accommodation and Food sector",
                                  '<br>',
                                  '<sup>',
                                  '(Weekly registrations 2020 / 2019)',
                                  '</sup>')),
         yaxis=list(title="Ratio of registrations (Index Week 23)", range=c(0.4,1.6)),
         xaxis=list(title="Week"),
         legend=list(x=0.7, y=0.9),
         shapes=list(list(type='line', x0=yday("2020-07-08")/7, x1=yday("2020-07-08")/7, y0=0, y1=2, opacity=0.5),
                     list(type='rect', x0=yday("2020-08-01")/7, x1=yday("2020-08-31")/7, y0=0, y1=2, fillcolor="grey", line = list(color = "grey"), opacity=0.3)),
         updatemenus=list(list(buttons=list(list(label="Indexed ratios", method="update",
                                                 args=list(list(visible=c(rep(TRUE,2), rep(FALSE,2))),
                                                           list(yaxis=list(title="Ratio of registrations (Index Week 23)", range=c(0.4,1.6))))),
                                            list(label="Actual ratios", method="update",
                                                 args=list(list(visible=c(rep(FALSE,2), rep(TRUE,2))),
                                                           list(yaxis=list(title="Ratio of registrations", range=c(0.4,2))))))
                               
         ))) %>%
  add_annotations(data=dateLabels, x=~(yday(x)/7), y=0.55, text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)


#Plot 7-day rolling average and weekly registrations in Other sectors.
plot_ly(data=n_incorps_all, x=~(as.Date("2019-12-31")+Day), y=~All_2019, name="2019 All Postcodes", type='scatter', mode='lines', color=I("#d62728")) %>%
  add_trace(data=n_incorps_all, x=~(as.Date("2019-12-31")+Day), y=~All_2020, name="2020 All Postcodes", type='scatter', mode='lines', color=I("#1f77b4")) %>%
  add_trace(data=n_incorps_all, x=~(as.Date("2019-12-31")+Day), y=~EO2HO_2019, name="2019 EO2HO Postcodes", type='scatter', mode='lines', color=I("#2ca02c")) %>%
  add_trace(data=n_incorps_all, x=~(as.Date("2019-12-31")+Day), y=~EO2HO_2020, name="2020 EO2HO Postcodes", type='scatter', mode='lines', color=I("#ff7f0e")) %>%
  add_trace(data=w_incorps_all, x=~(as.Date("2019-12-31")+Week*7), y=~All_2019, name="2019 All Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#d62728")) %>%
  add_trace(data=w_incorps_all, x=~(as.Date("2019-12-31")+Week*7), y=~All_2020, name="2020 All Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#1f77b4")) %>%
  add_trace(data=w_incorps_all, x=~(as.Date("2019-12-31")+Week*7), y=~EO2HO_2019, name="2019 EO2HO Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#2ca02c")) %>%
  add_trace(data=w_incorps_all, x=~(as.Date("2019-12-31")+Week*7), y=~EO2HO_2020, name="2020 EO2HO Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#ff7f0e")) %>%
  add_trace(data=w_incorps_all, x=~(as.Date("2019-12-31")+Week*7), y=~iAll_2019, name="2019 All Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#d62728")) %>%
  add_trace(data=w_incorps_all, x=~(as.Date("2019-12-31")+Week*7), y=~iAll_2020, name="2020 All Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#1f77b4")) %>%
  add_trace(data=w_incorps_all, x=~(as.Date("2019-12-31")+Week*7), y=~iEO2HO_2019, name="2019 EO2HO Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#2ca02c")) %>%
  add_trace(data=w_incorps_all, x=~(as.Date("2019-12-31")+Week*7), y=~iEO2HO_2020, name="2020 EO2HO Postcodes", type='scatter', mode='lines', visible=FALSE, color=I("#ff7f0e")) %>%
  layout(title=list(text="Registrations in Other sectors (excl. Accommodation and Food)"),
         xaxis=list(title="Date", type='date', tickformat="%d %b"),
         yaxis=list(title="Rolling average", range=c(-0.1,1)*ceiling(max(n_incorps_all[c(2:5)])/50)*50),
         updatemenus=list(list(buttons=list(list(method="update", label="7-day RA",
                                                 args=list(list(visible=c(rep(TRUE,4), rep(FALSE,8))),
                                                           list(yaxis=list(title="Rolling average", range=c(-0.1,1)*ceiling(max(n_incorps_all[c(2:5)])/50)*50)))),
                                            list(method="update", label="Weekly total",
                                                 args=list(list(visible=c(rep(FALSE,4), rep(TRUE,4), rep(FALSE,4))),
                                                           list(yaxis=list(title="Weekly total", range=c(-0.1,1)*ceiling(max(w_incorps_all[c(2:5)])/500)*500)))),
                                            list(method="update", label="Weekly total (indexed)",
                                                 args=list(list(visible=c(rep(FALSE,8), rep(TRUE,4))),
                                                           list(yaxis=list(title="Weekly total (Index week 23)", range=c(-0.1,1)*ceiling(max(w_incorps_all[c(8:11)])/0.2)*0.2))))
         ))),
         shapes=list(list(type='line', x0=as.Date("2020-07-08"), x1=as.Date("2020-07-08"), y0=0, y1=30000, opacity=0.5),
                     list(type='rect', x0=as.Date("2020-08-01"), x1=as.Date("2020-08-31"), y0=0, y1=30000, fillcolor="grey", line = list(color = "grey"), opacity=0.3))) %>% 
  add_annotations(data=dateLabels, x=~x, y=0, text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)

#Ratio of registrations in Other sectors.
figure5 <- plot_ly(data=w_incorps_all, x=~Week) %>% add_trace(y=~iAll, name="All Postcodes", type='scatter', mode='lines', color=I("#1f77b4")) %>% 
  add_trace(y=~iEO2HO, name="EO2HO Postcodes", type='scatter', mode='lines', color=I("#ff7f0e")) %>%
  add_trace(y=~All_Postcodes, name="All Postcodes", type='scatter', mode='lines', color=I("#1f77b4"), visible=FALSE) %>%
  add_trace(y=~EO2HO_Postcodes, name="EO2HO Postcodes", type='scatter', mode='lines', color=I("#ff7f0e"), visible=FALSE) %>%
  layout(title=list(text = paste0("Ratio of registrations in Other sectors",
                                  '<br>',
                                  '<sup>',
                                  '(Weekly registrations 2020 / 2019)',
                                  '</sup>')),
         yaxis=list(title="Ratio of registrations (Index Week 23)", range=c(0.4,1.6)),
         xaxis=list(title="Week"),
         legend=list(x=0.75, y=0.9),
         shapes=list(list(type='line', x0=yday("2020-07-08")/7, x1=yday("2020-07-08")/7, y0=0, y1=2, opacity=0.5),
                     list(type='rect', x0=yday("2020-08-01")/7, x1=yday("2020-08-31")/7, y0=0, y1=2, fillcolor="grey", line = list(color = "grey"), opacity=0.3)),
         updatemenus=list(list(buttons=list(list(label="Indexed ratios", method="update",
                                                 args=list(list(visible=c(rep(TRUE,2), rep(FALSE,2))),
                                                           list(yaxis=list(title="Ratio of registrations (Index Week 23)", range=c(0.4,1.6))))),
                                            list(label="Actual ratios", method="update",
                                                 args=list(list(visible=c(rep(FALSE,2), rep(TRUE,2))),
                                                           list(yaxis=list(title="Ratio of registrations", range=c(0.4,2))))))
                               
         ))) %>%
  add_annotations(data=dateLabels, x=~(yday(x)/7), y=0.55, text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)


##Spillover effect----
#Difference between EO2HO and all postcodes ratios.
w_incorps$Diff <- w_incorps$EO2HO_Postcodes - w_incorps$All_Postcodes
w_incorps_all$Diff <- w_incorps_all$EO2HO_Postcodes - w_incorps_all$All_Postcodes

figure6 <- plot_ly(data=w_incorps, x=~Week) %>% add_trace(y=~Diff, name="Accommodation and Food sector", type='scatter', mode='lines', color=I("#a0eb78")) %>%
  add_trace(data=w_incorps_all, x=~Week, y=~Diff, name="All other sectors", type='scatter', mode='lines', color=I("#c27f76")) %>%
  layout(title=list(text = paste0("Difference between EO2HO Postcodes and All Postcodes",
                                  '<br>',
                                  '<sup>',
                                  '(Weekly registrations 2020 / 2019)',
                                  '</sup>')),
         yaxis=list(title="Difference", range=c(-0.8,0.8)),
         xaxis=list(title="Week"),
         legend=list(x=0.65, y=0.9),
         shapes=list(list(type='line', x0=yday("2020-07-07")/7, x1=yday("2020-07-07")/7, y0=-1, y1=1, opacity=0.5),
                     list(type='rect', x0=yday("2020-08-01")/7, x1=yday("2020-08-31")/7, y0=-0.8, y1=0.78, fillcolor="grey", line = list(color = "grey"), opacity=0.3))) %>% 
  add_annotations(data=dateLabels, x=~(yday(x)/7), y=~(y-1), text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)


##Registrations due to the scheme in Other sectors----
#Compare registrations in EO2HO postcodes 2020 to registrations in 2019 x Non-EO2HO Postcodes ratio.
#Non-EO2HO Postcodes ratio.
w_incorps_all$NonEO2HO <- (w_incorps_all$All_2020 - w_incorps_all$EO2HO_2020) / (w_incorps_all$All_2019 - w_incorps_all$EO2HO_2019)
w_incorps_all$EO2HOxNon <- w_incorps_all$EO2HO_2019 * w_incorps_all$NonEO2HO

#Timeseries
plot_ly(data=w_incorps_all, x=~Week) %>% add_trace(y=~EO2HO_2020, type='scatter', mode='lines', color=I("#ff7f0e"), name="With EO2HO") %>%
  add_trace(y=~EO2HOxNon, type='scatter', mode='lines', color=I("#1f77b4"), name="Without EO2HO") %>%
  layout(title=list(text="Registrations in Other sectors in EO2HO Postcodes vs hypothetical registrations without the scheme"),
         yaxis=list(title="Number of registrations", range=c(-300,3000)),
         shapes=list(list(type='line', x0=yday("2020-07-08")/7, x1=yday("2020-07-08")/7, y0=0, y1=4500, opacity=0.5),
                     list(type='rect', x0=yday("2020-08-01")/7, x1=yday("2020-08-31")/7, y0=0, y1=4500, fillcolor="grey", line = list(color = "grey"), opacity=0.3))) %>% 
  add_annotations(data=dateLabels, x=~(yday(x)/7), y=0, text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)

library('pracma')
#Total Period1: Announcement to End of scheme
trapz(seq(27,35), w_incorps_all$EO2HO_2020[which(between(w_incorps_all$Week, 27, 35))] - w_incorps_all$EO2HOxNon[which(between(w_incorps_all$Week, 27, 35))])
#Total Period2: Announcement to Start of scheme
trapz(seq(27,31), w_incorps_all$EO2HO_2020[which(between(w_incorps_all$Week, 27, 31))] - w_incorps_all$EO2HOxNon[which(between(w_incorps_all$Week, 27, 31))])
#Total Period3: Start to End of scheme
trapz(seq(31,35), w_incorps_all$EO2HO_2020[which(between(w_incorps_all$Week, 31, 35))] - w_incorps_all$EO2HOxNon[which(between(w_incorps_all$Week, 31, 35))])

