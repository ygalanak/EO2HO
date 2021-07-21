# README

This folder includes the code necessary for our analysis of Eat Out to Help Out project.
Code runs in R.

1. `Registrations.R`: calculates registrations of companies in (a) Accommodation and food services and (b) other sectors. Daily, 7-day rolling average, weekly, indexed weekly totals, ratio of registrations (2020 over same week in 2019) and comparing EO2HO areas with all areas. 
    1.1. **Outputs**: Plots including figures **4**, **5** and **6** from the report.
2. `Record matching.R`: matches records from the list of participating restaurants in the scheme and Companies House basic data. It generates a representative random sample of relevant companies for comparison. 
    2.1. **Outputs**: Plots including figures **2**, **3**, **A1**.
3. `Regional analysis.R`: analysis of weekly registrations in other sectors by NUTS1 region. Quick investigation into whether a linear relationship exists between number of participating restaurants and ratio of registrations. 
    3.1. **Outputs**: Plots including figures **1**, **7**, **8**, **9**, **10**, **11**.
4. `Sectoral analysis.R`: analysis of weekly registrations in non-hospitality sectors by sector. 
    4.1. **Outputs**: Plots includng figures **12**, **13**, **14**.
5. `Furlough.R`: Analysis of number of employees on furlough in Accommodation and food services sector and in the whole economy. Calculates the reduction of employee-days and estimates savings. 
    5.1. **Outputs**: Plots including figures **15**, **16**.
