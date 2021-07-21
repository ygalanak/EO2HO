##Furlough timeseries----
#Furlough dataset.
furlough <- read.csv("CJRS number of employments furloughed by sector.csv")
furlough$Date <- as.Date(furlough$Date, "%d/%m/%Y")
furlough$Total <- rowSums(furlough[-1])

#Plot overall furlough and furlough in accommodation with scaled total
#Scaling ratio, to anchor for the week before the announcement.
scale <- mean(furlough$Accommodation.and.food.services[c(101:107)]) / mean(furlough$Total[c(101:107)])
furlough$Net <- furlough$Accommodation.and.food.services - scale * furlough$Total


alldateLabels <- data.frame(x=c("2020-07-08", "2020-08-16", "2020-10-31"), t=c("Announcement", "EO2HO Period", "2nd Lockdown Announced"))
figure16 <- plot_ly(data=furlough, x=~Date, y=~Total, type='scatter', mode='lines', name="Total", color=I("#1f77b4")) %>%
  add_trace(y=~Accommodation.and.food.services, type='scatter', mode='lines', name="Accommodation and Food sector", visible=FALSE, color=I("#ff7f0e")) %>%
  add_trace(y=~(Total*scale), type='scatter', mode='lines', name="Scaled Total", visible=FALSE, color=I("#1f77b4")) %>%
  layout(title=list(text="Furloughed employees on CJRS in the whole economy"),
         xaxis=list(range=c(as.Date("2020-03-23"), as.Date("2020-12-31"))),
         yaxis=list(title="Number of employees", range=c(-0.1,1)*ceiling(max(furlough$Total)/500000)*500000),
         legend=list(x=0.6, y=0.8),
         shapes=list(list(type='line', x0=("2020-07-08"), x1=("2020-07-08"), y0=0, y1=~(max(Total, na.rm=TRUE)), opacity=0.5),
                     list(type='rect', x0=("2020-08-01"), x1=("2020-08-31"), y0=0, y1=~(max(Total, na.rm=TRUE)), fillcolor="grey", line = list(color = "grey"), opacity=0.3)),
         updatemenus=list(list(buttons=list(list(label="Whole economy", method="update",
                                                 args=list(list(visible=c(TRUE, FALSE, FALSE)),
                                                           list(yaxis=list(title="Number of employees", range=c(-0.1,1)*ceiling(max(furlough$Total)/500000)*500000),
                                                                title=list(text="Furloughed employees on CJRS in the whole economy")))),                                    
                                            list(label="Accommodation and Food", method="update",
                                                 args=list(list(visible=c(FALSE, TRUE, TRUE)),
                                                           list(yaxis=list(title="Number of employees", range=c(-0.1,1)*ceiling(max(c(furlough$Accommodation.and.food.services, furlough$Total*scale))/500000)*500000),
                                                                title=list(text="Furloughed employees on CJRS in Accommodation and Food sector"))))
                                            )))) %>% 
  add_annotations(data=alldateLabels, x=~x, y=0, text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)


## Estimating the saving in furlough monies----
#Actual number of furloughed employee-days from 8th July to 31st October
library('pracma')
f1 <- trapz(as.numeric(seq(as.Date("2020-07-08"), as.Date("2020-10-31"), by='day')), furlough$Accommodation.and.food.services[which(between(furlough$Date, as.Date("2020-07-08"), as.Date("2020-10-31")))])
#Predicted number of furloughed employees from scaled trend.
f2 <- trapz(as.numeric(seq(as.Date("2020-07-08"), as.Date("2020-10-31"), by='day')), scale*furlough$Total[which(between(furlough$Date, as.Date("2020-07-08"), as.Date("2020-10-31")))])
#Estimated saving in employee-days.
f2-f1

#Source for pay deciles in the sector:
#https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/regionbyindustry2digitsicashetable5

#As at 31st August 2020, there were 2216300 employments in the sector eligible for furlough.
#FTE employees.
(f2-f1) / 115
(f2-f1) / (1669000 * 115)
#The saving in employee-days is roughly 5% of the sector's employment.
max(furlough$Total[which(between(furlough$Date, as.Date("2020-06-29"), as.Date("2020-10-30")))] * 0.21778 - furlough$Accommodation.and.food.services[which(between(furlough$Date, as.Date("2020-06-29"), as.Date("2020-10-30")))], na.rm=TRUE) / 1077000
#First decile
(f2-f1)*60.3*0.8/7
#Median
(f2-f1)*208.6*0.8/7
#Tenth decile
(f2-f1)*504.8*0.8/7

#Government contributions changed during this period, from 70% in July to 60% from August.
f1.1 <- trapz(as.numeric(seq(as.Date("2020-07-08"), as.Date("2020-07-31"), by='day')), furlough$Accommodation.and.food.services[which(between(furlough$Date, as.Date("2020-07-08"), as.Date("2020-07-31")))])
f1.2 <- trapz(as.numeric(seq(as.Date("2020-07-31"), as.Date("2020-10-31"), by='day')), furlough$Accommodation.and.food.services[which(between(furlough$Date, as.Date("2020-07-31"), as.Date("2020-10-31")))])
f2.1 <- trapz(as.numeric(seq(as.Date("2020-07-08"), as.Date("2020-07-31"), by='day')), scale*furlough$Total[which(between(furlough$Date, as.Date("2020-07-08"), as.Date("2020-07-31")))])
f2.2 <- trapz(as.numeric(seq(as.Date("2020-07-31"), as.Date("2020-10-31"), by='day')), scale*furlough$Total[which(between(furlough$Date, as.Date("2020-07-31"), as.Date("2020-10-31")))])
#First decile
(f2.1-f1.1)*60.3*0.7/7 + (f2.2-f1.2)*60.3*0.6/7
#Median
(f2.1-f1.1)*208.6*0.7/7 + (f2.2-f1.2)*208.6*0.6/7
#Tenth decile
(f2.1-f1.1)*504.8*0.7/7 + (f2.2-f1.2)*504.8*0.6/7

#Economy-wide reduction.
mean(furlough$Accommodation.and.food.services[c(101:107)])*115 - f2

##Employment----
#https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employmentbyindustryemp13
employment_hospitality <- data.frame(Date=as.Date(c("2018-09-01", "2018-12-01", "2019-03-01", "2019-06-01", "2019-09-01", "2019-12-01", "2020-03-01", "2020-06-01", "2020-09-01", "2020-12-01", "2021-03-01")), Employment=c(1775, 1739, 1740, 1773, 1821, 1745, 1800, 1669, 1560, 1472, 1437))
figure15 <- plot_ly(data=employment_hospitality, x=~Date, y=~(Employment)*1000, type='scatter', mode='lines') %>%
  layout(title=list(text="Employment in Accommodation and Food sector"),
         yaxis=list(title="Number of employees", range=c(1400000, 1800000)),
         xaxis=list(range=c(as.Date("2020-03-23"), as.Date("2020-12-31"))),
         shapes=list(list(type='line', x0=("2020-07-08"), x1=("2020-07-08"), y0=1436000, y1=2500000, opacity=0.5),
                     list(type='rect', x0=("2020-08-01"), x1=("2020-08-31"), y0=1436000, y1=2500000, fillcolor="grey", line = list(color = "grey"), opacity=0.3))) %>%
  add_annotations(data=alldateLabels, x=~x, y=1436000, text=~t, showarrow=TRUE, showlegend=FALSE, bgcolor="white", ax=0, ay=25)
