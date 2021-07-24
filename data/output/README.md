# Graphs

## tl;dr

This folder stores all our figures values. This is a documentation file.

## Documentation

1. `Fig1.csv`

   | Variable | Description                                        |
   | -------- | -------------------------------------------------- |
   | NUTS1    | NUTS1 code                                         |
   | Name     | Name of NUTS1 region                               |
   | n        | Number of registered establishments in each region |

2. `Fig2.csv`: comparing registered EO2HO establishments to a sample of relevant companies by region

   | Variable | Description                                                  |
   | -------- | ------------------------------------------------------------ |
   | Name     | Name of NUTS1 region                                         |
   | EO2HO    | Proportion of EO2HO establishments in each region            |
   | All      | Proportion of (sample of) relevant companies in each region  |
   | EO2HO.1  | Proportion of EO2HO establishments in each region (excluding London) |
   | All.1    | Proportion of (sample of) relevant companies in each region (excluding London) |

3. `Fig3`: Comparing registered EO2HO establishments to a sample of relevant companies by age
   `Fig3a.csv` - matched EO2HO companies; `Fig3b.csv`- Sample of relevant companies 

   | Variable | Description       |
   | -------- | ----------------- |
   | x        | Date (yyyy-mm-dd) |
   | y        | Density4.         |

4. `Fig4.csv`: ratio of registrations

   | Variable        | Description                                      |
   | --------------- | ------------------------------------------------ |
   | Week            | number of week (based on the incorporation date) |
   | All_Postcodes   | Ratio of registrations in all postcodes          |
   | EO2HO_Postcodes | Ratio of registrations in EO2HO postcodes        |
   | iAll            | Ratio in all postcodes indexed at week 23        |
   | iEO2HO          | Ratio in EO2HO postcodes indexed at week 23      |

5. `Fig5.csv`

   | Variable        | Description                                      |
   | --------------- | ------------------------------------------------ |
   | Week            | number of week (based on the incorporation date) |
   | All_Postcodes   | Ratio of registrations in all postcodes          |
   | EO2HO_Postcodes | Ratio of registrations in EO2HO postcodes        |
   | iAll            | Ratio in all postcodes indexed at week 23        |
   | iEO2HO          | Ratio in EO2HO postcodes indexed at week 23      |

6. `Fig6.csv`: effect of EO2HO: the difference between EO2HO-participating postcodes and all, relative to their 2019 levels

   | Variable  | Description                                                  |
   | --------- | ------------------------------------------------------------ |
   | Week      | number of week (based on the incorporation date)             |
   | HospDiff  | Ratio of registrations in EO2HO postcodes minus ratio in all postcodes for Accommodation and Food |
   | OtherDiff | Ratio of registrations in EO2HO postcodes minus ratio in all postcodes for other sectors |

7. `Fig7.csv`

   | Variable        | Description                                      |
   | --------------- | ------------------------------------------------ |
   | Week            | number of week (based on the incorporation date) |
   | All_Postcodes   | Ratio of registrations in all postcodes          |
   | EO2HO_Postcodes | Ratio of registrations in EO2HO postcodes        |

8. `Fig9.csv`

   | Variable | Description                                                  |
   | -------- | ------------------------------------------------------------ |
   | Name     | Name of NUTS1 region                                         |
   | Diff     | Ratio of registrations in EO2HO postcodes minus ratio   in all postcodes for other sectors |
   | Lo       | Lower bound of 95% confidence interval for Diff              |
   | Hi       | Upper bound of 95% confidence interval for Diff              |

9. `Fig10.csv`

   | Variable    | Description                                                  |
   | ----------- | ------------------------------------------------------------ |
   | Name        | Name of NUTS1 region                                         |
   | Restaurants | Number of registered restaurants                             |
   | Ratio       | Ratio of registrations (all postcodes)                       |
   | Predict     | Predicted ratio of registrations from linear best fit   model |

10. `Fig11.csv`

    | Variable     | Description                            |
    | ------------ | -------------------------------------- |
    | PostcodeArea | Postcode area                          |
    | R            | Number of registered restaurants       |
    | Ratio        | Ratio of registrations (all postcodes) |

11. `Fig12.csv`

    | Variable | Description                                                  |
    | -------- | ------------------------------------------------------------ |
    | Name     | Truncated (to three words maximum) name of sector            |
    | Week     | number of week (based on the incorporation date)             |
    | iAll     | Ratio of registrations in all postcodes indexed at   week 23 |
    | iEO2HO   | Ratio of registrations in EO2HO postcodes indexed at   week 23 |

12. `Fig13.csv`

    | Variable | Description                                                  |
    | -------- | ------------------------------------------------------------ |
    | Name     | Truncated (to three words maximum) name of sector            |
    | Diff     | Ratio of registrations in EO2HO postcodes minus ratio   in all postcodes for other sectors |
    | Lo       | Lower bound of 95% confidence interval for Diff              |
    | Hi       | Upper bound of 95% confidence interval for Diff              |

13. `Fig14.csv`

    | Variable | Description                                                  |
    | -------- | ------------------------------------------------------------ |
    | Name     | Truncated (to three words maximum) name of sector            |
    | Week     | number of week (based on the incorporation date)             |
    | iAll     | Ratio of registrations in all postcodes indexed at   week 23 |
    | iEO2HO   | Ratio of registrations in EO2HO postcodes indexed at   week 23 |

14. `Fig15.csv`

    | Variable                        | Description                                                  |
    | ------------------------------- | ------------------------------------------------------------ |
    | Date                            | Date (yyyy-mm-dd)                                            |
    | Accommodation.and.food.services | Number of employees in hospitality sector on furlough   through CJRS |
    | Total                           | Number of employees on furlough through CJRS in total        |





