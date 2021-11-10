# ---
# The impact of EOTHO on firm creation
# Output: Regressions for EOTHO paper
# Galanakis, Hobbs & Savagar (2021)
# Contact: <i.galanakis@kent.ac.uk>
# ---


# Packages ----
packages <- c(
  "tidyverse", "naniar", "haven", "survey", "ggpubr", "latex2exp",
  "data.table", "lubridate", "ggalt", "cowplot", "animation",
  "patchwork", "sp", "scales", "raster", "rgeos", "mapproj", "zoo",
  "rgdal", "maptools", "emojifont", "nord", "paletteer", "stringr", "plotly",
  "effects", "ggeffects", "margins"
)
pkg_notinstall <- packages[!(packages %in% installed.packages()[, "Package"])]

lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# read data----
## Data on restaurants as announced by HMRC ----
restaurants <- read_csv("https://raw.githubusercontent.com/hmrc/eat-out-to-help-out-establishments/master/data/participating-establishments/restaurants.csv")
## Company registrations with postcodes, NUTS3 and NUTS 1, and district population ----
clean <- read_csv("data/Clean_population.csv")

# DFs for regressions ----
## count number of restaurants in each EOTHO postcode ----
n_restaurants <- restaurants %>%
  group_by(Postcode) %>%
  count()
n_restaurants <- n_restaurants %>% rename(EOTHOrestaurants = n)
n_restaurants$Postcode <- gsub("\\s+", "", n_restaurants$Postcode)

## count number of incorporations ----
n_incorp <- clean %>%
  # identify the period of EOTHO
  mutate(
    EOTHOperiod = ifelse(IncorporationDate >= "2020-08-03" &
      IncorporationDate <= "2020-08-30", 1, 0),
    # create 2 digit SIC code
    SIC2dg1 = as.integer(SIC5dg1 / 1000)
  ) %>%
  # exclude 55 and 56 (Accommodation and Hospitality)
  filter(!(SIC2dg1 %in% list(55, 56))) %>%
  group_by(
    IncorporationDate, Postcode, EOTHO,
    EOTHOperiod, SIC5dg1, SIC2dg1, PostcodeDistrict,
    NUTS3, NUTS1, DistrictPopulation
  ) %>%
  count()
n_incorp <- n_incorp %>% rename(registrations = n)

clean2 <- left_join(n_incorp, n_restaurants, by = "Postcode") %>%
  distinct(IncorporationDate, Postcode, EOTHO, .keep_all = T)

# replace EOTHOrestaurants NA to 0
clean2$EOTHOrestaurants[is.na(clean2$EOTHOrestaurants)] <- 0


# regression ----
library(SciViews)
# make ln(registration) & ln(population)
clean2$lnregistrations <- ln(clean2$registrations)
clean2$lnpopulation <- ln(clean2$DistrictPopulation)
# add the number of the week
clean2$week <- week(clean2$IncorporationDate)
# daily linear pre-trend for each NUTS3 + SIC5dig
clean2_trend <- clean2 %>%
  # pre-trend period
  filter(IncorporationDate < "2020-08-03") %>%
  # group by NUTS3
  group_by(NUTS3) %>%
  # add slopes of regressions
  mutate(trend = coef(lm(lnregistrations ~ IncorporationDate,
    data = clean2
  ))[2]) %>%
  # bring back the rest of df
  right_join(clean2) %>%
  # if there we are in the post-EOTHO period, replace trend with zero (so that it's not dropped in the regression)
  mutate(trend = replace_na(trend, 0))


## baseline ----
didreg1 <- lm(lnregistrations ~ EOTHOperiod * EOTHO
  # baseline FE
  + week * SIC2dg1 * NUTS3,
data = clean2
)

didreg1_trend <- lm(lnregistrations ~ EOTHOperiod * EOTHO
  # baseline FE
  + week * SIC2dg1 * NUTS3
  # trend
  + trend,
data = clean2_trend
)

## FE + trends + chi_i x eta_w ----
didreg2 <- lm(lnregistrations ~ EOTHOperiod * EOTHO
  # baseline FE
  + week * SIC2dg1 * NUTS3
  # time-invariant characteristics x week
  + week * lnpopulation,
data = clean2
)

didreg2_trend <- lm(lnregistrations ~ EOTHOperiod * EOTHO
  # baseline FE
  + week * SIC2dg1 * NUTS3
  # time-invariant characteristics
  + week * lnpopulation
  + trend,
data = clean2_trend
)

## marginal effect
margins::margins(didreg2_trend, variable = "EOTHO", at = list(EOTHOperiod = 0:1))
# present all models as a table ----
library(huxtable)
print_latex(huxreg(didreg1, didreg1_trend, didreg2, didreg2_trend,
  # choose coeff
  coefs = c("EOTHO", "EOTHOperiod:EOTHO"),
  # keep 2 decimal digits
  number_format = 4,
  # show only n and R2
  statistics = c("# observations" = "nobs", "R squared" = "r.squared"),
  # set stars
  stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
  note = "Estimates for the natural logarithm of the companies registrations.
       Baseline fixed effects refer to fixed effect for the week, NUTS3, 2-digit SIC code and their interaction.
       Robust standard errors in parentheses.
       %stars%."
))



# graphs ----

library(ggeffects)
library(effects)

summary(margins(didreg1, variables = c("EOTHO")))
summary(margins(didreg1_trend, variables = c("EOTHO")))

## Plot 1: illustration ----
plot_data <- clean2 %>%
  mutate(
    EOTHO = factor(EOTHO,
      levels = c(0, 1),
      labels = c("not treated", "treated")
    ),
    EOTHOperiod = factor(EOTHOperiod,
      levels = c(0, 1),
      labels = c("Before Aug3", "After Aug3")
    )
  ) %>%
  group_by(EOTHO, EOTHOperiod) %>%
  summarize(
    meanLNRegistrations = mean(lnregistrations),
    se_reg = sd(lnregistrations) / sqrt(n()),
    upper = meanLNRegistrations + (-1.96 * se_reg),
    lower = meanLNRegistrations + (1.96 * se_reg)
  )

ggplot(plot_data, aes(x = EOTHOperiod, y = meanLNRegistrations, color = EOTHO)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  # The group = highearn here makes it so the lines go across categories
  geom_line(aes(group = EOTHO)) +
  ylab("Average ln(registrations)") +
  xlab("EOTHO period")

## Diff-in-Diff by hand ----
eotho_diff <- clean2 %>%
  group_by(EOTHOperiod, EOTHO) %>%
  summarize(mean_lnRegistrations = mean(lnregistrations))
eotho_diff

before_treatment <- eotho_diff %>%
  filter(EOTHOperiod == 0, EOTHO == 1) %>%
  pull(mean_lnRegistrations)

before_control <- eotho_diff %>%
  filter(EOTHOperiod == 0, EOTHO == 0) %>%
  pull(mean_lnRegistrations)

after_treatment <- eotho_diff %>%
  filter(EOTHOperiod == 1, EOTHO == 1) %>%
  pull(mean_lnRegistrations)

after_control <- eotho_diff %>%
  filter(EOTHOperiod == 1, EOTHO == 0) %>%
  pull(mean_lnRegistrations)

diff_treatment_before_after <- after_treatment - before_treatment
diff_control_before_after <- after_control - before_control
diff_diff <- diff_treatment_before_after - diff_control_before_after

diff_before_treatment_control <- before_treatment - before_control
diff_after_treatment_control <- after_treatment - after_control
other_diff_diff <- diff_after_treatment_control - diff_before_treatment_control

### Plot 2: Graph of diff-in-diff by hand----
ggplot(eotho_diff, aes(
  x = as.factor(EOTHOperiod),
  y = mean_lnRegistrations,
  color = as.factor(EOTHO)
)) +
  geom_point() +
  geom_line(aes(group = as.factor(EOTHO))) +
  annotate(
    geom = "segment", x = "0", xend = "1",
    y = before_treatment, yend = after_treatment - diff_diff,
    linetype = "dashed", color = "grey50"
  ) +
  annotate(
    geom = "segment", x = "1", xend = "1",
    y = after_treatment, yend = after_treatment - diff_diff,
    linetype = "dotted", color = "blue"
  ) +
  annotate(
    geom = "label", x = "1", y = after_treatment - (diff_diff / 2),
    label = "EOTHO", size = 3
  )
