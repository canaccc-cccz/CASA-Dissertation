library(spatialreg)
library(spgwr)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(janitor)
library(RColorBrewer)
library(sp)
library(spdep)
library(magrittr)

Londonborough <-
  st_read(
    here::here(
      'statistical-gis-boundaries-london',
      'ESRI',
      'London_Borough_Excluding_MHW.shp'
    )
  ) %>%
  st_transform(., 27700)

regression_data <-
  read_csv('summary_regression_data1.csv',
           na = c("NA", "n/a")
  ) %>%
  clean_names()

names(regression_data)
names(Londonborough)

Londonborough_merged <-
  Londonborough %>% left_join(regression_data, by = c('GSS_CODE' = 'area_code')) %>%
  distinct(
    GSS_CODE,
    NAME,small_businesses_density,
    cases_density_number_per_km2,
    median_income_2012_thousand,
    proportion_of_population_of_working_age_2015,
    employment_rate_2020,
    average_house_price_2020_thousand,
    population_thousand
  )

Londonborough_merged_omit <- na.omit(Londonborough_merged)

neibour <- poly2nb(Londonborough_merged_omit, queen = TRUE)
neibour[[1]]

lw <- nb2listw(neibour, style = "W", zero.policy = TRUE)
lw$weights[1]


slag_dv_model2_queen <-
  lagsarlm(
    small_businesses_density ~ cases_density_number_per_km2 + median_income_2012_thousand +
      proportion_of_population_of_working_age_2015 +
      employment_rate_2020 +
      average_house_price_2020_thousand + population_thousand,
    data = Londonborough_merged_omit,
    nb2listw(neibour, style = "C"),
    method = "eigen"
  )


summary(slag_dv_model2_queen)

#spatial error model
fit.err <- errorsarlm(
  small_businesses_density ~ cases_density_number_per_km2 + median_income_2012_thousand +
    proportion_of_population_of_working_age_2015 +
    employment_rate_2020 +
    average_house_price_2020_thousand + population_thousand,
  data = Londonborough_merged_omit,
  nb2listw(neibour, style = "C"),
  method = "eigen"
)

summary(fit.err)

moran.test(Londonborough_merged_omit$small_businesses_density, lw)




