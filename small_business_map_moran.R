#import libriaries that need to be used
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

small_business <-
  read_csv('summary_regression_data.csv',
           na = c("NA", "n/a")
  ) %>%
  clean_names()

names(small_business)
names(Londonborough)

Londonborough_merged <-
  Londonborough %>% left_join(small_business, by = c('GSS_CODE' = 'area_code')) %>%
  distinct(GSS_CODE, NAME, small_business_number_2020_thousand) #choose which column to be used in the following analysis

Londonborough_merged_omit <- na.omit(Londonborough_merged)

tmap_mode('plot')

map_small_business <- tm_shape(Londonborough_merged_omit) +
  tm_polygons(
    'small_business_number_2020_thousand',
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',title='Number of small businesses',legend.format = list(scientific = TRUE, format = "f")
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = TRUE,
    legend.title.size = 1.25,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(position = c('left', 'bottom'), text.size = 0.75)

#plot a map of small business distribution
map_small_business

#global moran test for small business number
neibour <- poly2nb(Londonborough_merged_omit, queen = TRUE)
neibour[[1]]

lw <- nb2listw(neibour, style = "W", zero.policy = TRUE)
lw$weights[1] #each neighbour is assigned a quarter of total weight

moran.test(Londonborough_merged_omit$small_business_number_2020_thousand, lw)

