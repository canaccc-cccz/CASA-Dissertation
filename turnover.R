#import libraries
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

turnover <-
  read_csv('turnover_trend.csv',
           na = c("NA", "n/a")
  ) %>%
  clean_names()

Londonborough_merged_turnover <-
  Londonborough %>% left_join(turnover, by = c('GSS_CODE' = 'area_code')) %>%
  distinct(GSS_CODE, NAME, positive_turnover_trend_2020,positive_turnover_trend_2019) #choose which column to be used in the following analysis

Londonborough_merged_omit_turnover <- na.omit(Londonborough_merged_turnover)

tmap_mode('plot')

breaks = c(0, 500, 1000, 1500, 2000, 2500,3000) 

turnover_1 <- tm_shape(Londonborough_merged_omit_turnover) +
  tm_polygons(
    'positive_turnover_trend_2020',
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',
    title = 'Year 2020',breaks=breaks,
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_credits('(A) Positive trend in 2020', position = c('left', 'top'), size = 1.2) + 
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position =c('right', 'top'), text.size = 0.6)

turnover_1

turnover_2 <- tm_shape(Londonborough_merged_omit_turnover) +
  tm_polygons(
    'positive_turnover_trend_2019',
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',
    title = 'Year 2019',breaks=breaks
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_credits('(B) Positive trend in 2019', position = c('left', 'top'), size = 1.2) + 
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position =c('right', 'top'), text.size = 0.6)

turnover_2

turnover=tmap_arrange(turnover_1,turnover_2,nrow=1)

tmap_save(turnover,'turnover.png')



