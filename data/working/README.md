# ReadMe

- `matched_restaurants.csv`: matched EO2HO-participating restaurants (as released by HMRC) to Companies House basic data.
For variables documentation:

| Variable                | Description                      |
|-------------------------|----------------------------------|
|     2-6                 |     See restaurants              |
|     8-64                |     See EO2HO data               |
|     SIC5dg1             |     Primary SIC code, 5-digit    |
|     SIC2dg1             |     Primary SIC code, 2-digit    |
|     PostcodeDistrict    |     Postcode district            |
|     PostcodeArea        |     Postcode area                |

- `sample.csv`: a random sample of size 20,249 is taken from the Companies House basic dataset of all existing (as of October 2020) companies in the UK. To sample relevant companies, it is weighted by 2-digit SIC code according to the proportions of EO2HO-participating businesses, as in Table 2 of our report.




