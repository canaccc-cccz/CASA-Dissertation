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


#read london borough shapefile
Londonborough <-
  st_read(
    here::here(
      'statistical-gis-boundaries-london',
      'ESRI',
      'London_Borough_Excluding_MHW.shp'
    )
  ) %>%
  st_transform(., 27700)

#check whether shapefile is in the correct projection
st_crs(Londonborough)

#read small businesses csv file
small_business <-
  read_csv('small_businesses.csv',
    na = c("NA", "n/a")
  ) %>%
  clean_names()

#check the column names in csv file
names(small_business)
names(Londonborough)

#join the shapefile and CSV file
Londonborough_merged <-
  Londonborough %>% left_join(small_business, by = c('GSS_CODE' = 'area_code')) %>%
  distinct(GSS_CODE, NAME, x2020,x2019,x2018,x2017,x2016,x2015) #choose which column to be used in the following analysis

#omit na value(city of london)
Londonborough_merged_omit <- na.omit(Londonborough_merged)

#rename the column

st_crs(Londonborough_merged_omit)

#making map
tmap_mode('plot')

#setting breaks
#breaks = c(10, 15, 20, 25, 30, 35) 

#plotting map for 2020 small businesses
tm1 <- tm_shape(Londonborough_merged) +
  tm_polygons(
    'x2020',
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',
    title = 'Year 2020'
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_credits('(A) Small businesses in 2020', position = c('left', 'top'), size = 1.2) + 
  tm_compass(type = "arrow", position = c("right", "bottom")) +
  tm_scale_bar(position =c('right', 'bottom'), text.size = 0.6)

tm1

#plotting small businesses in 2019
tm2 <- tm_shape(Londonborough_merged) +
  tm_polygons(
    'x2019',
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',
    title = 'Year 2019'
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_credits('(B) Small businesses in 2019', position = c('left', 'top'), size = 1.2) + 
  tm_compass(type = "arrow", position = c("right", "bottom")) +
  tm_scale_bar(position =c('right', 'bottom'), text.size = 0.6)
tm2

#combine two maps
t=tmap_arrange(tm1,tm2,nrow=1)
#saving map
tmap_save(t,'small_business.png')

#plotting map for COVID-19 distribution
COVID <-
  read_csv('summary_regression_data.csv',
           na = c("NA", "n/a")
  ) %>%
  clean_names()

Londonborough_merged_covid <-
  Londonborough %>% left_join(COVID, by = c('GSS_CODE' = 'area_code')) %>%
  distinct(GSS_CODE, NAME, total_cases) 
#choose which column to be used in the following analysis

Londonborough_merged_omit_covid <- na.omit(Londonborough_merged_covid)

map_covid <- tm_shape(Londonborough_merged_omit_covid) +
  tm_polygons(
    'total_cases',
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',title='COVID-19 cases',legend.format = list(scientific = TRUE, format = "f")
  ) + tm_layout(
    legend.position = c('right', 'bottom'),
    title.position = c('left', 'top'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(position = c('left', 'bottom'), text.size = 0.6)

map_covid

tmap_save(map_covid,'total_cases.png')

#map for case density
regression <-
  read_csv('summary_regression_data1.csv',
           na = c("NA", "n/a")
  ) %>%
  clean_names()

names(regression)

Londonborough_merged <-
  Londonborough %>% left_join(regression, by = c('GSS_CODE' = 'area_code')) %>%
  distinct(GSS_CODE, NAME,cases_density_number_per_km2 ) #choose which column to be used in the following analysis


Londonborough_merged_omit_density <- na.omit(Londonborough_merged)

map_density <- tm_shape(Londonborough_merged_omit_density) +
  tm_polygons(
    'cases_density_number_per_km2',
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',title='COVID-19 cases density',legend.format = list(scientific = TRUE, format = "f")
  ) + tm_layout(
    legend.position = c('right', 'bottom'),
    title.position = c('left', 'top'),
    legend.outside = FALSE,
    legend.title.size = 1.0,
    legend.text.size = 0.6,
    frame = FALSE
  ) + tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(position = c('left', 'bottom'), text.size = 0.6)

map_density

tmap_save(map_density,'COVID-density.png')

#########################

#spatial autocorrelation

#creating polygon for dataset
neibour <- poly2nb(Londonborough_merged_omit, queen = TRUE)
neibour[[1]]

#assign weight matrix for each neighbouring polygon using row standardisation
lw <- nb2listw(neibour, style = "W", zero.policy = TRUE)
lw$weights[1] #each neighbour is assigned a quarter of total weight

#computing global Moran's I for small business in 2020
moran.test(Londonborough_merged_omit$x2020, lw)

#local Moran's I
local_moran <- localmoran(Londonborough_merged_omit$x2020, lw)
summary(local_moran)



#plot local moran map
#There are 5 columns of data.
#copy some of the columns (the I score (column 1) and the z-score standard deviation (column 4))
#the z-score (standardised value relating to whether high values or low values are clustering together)
#change local_moran type

local_moran_tibble <- as_tibble(local_moran) #change to dataframe

Londonborough_merged_omit <-
  Londonborough_merged_omit %>% 
  mutate(local_moran_I = as.numeric(local_moran_tibble$Ii)) %>% 
  mutate(local_moran_z =as.numeric(local_moran_tibble$Z.Ii))


#setting color
MoranColours <- rev(brewer.pal(8, "RdBu"))

#plot a map
tmap_mode('plot')

tm_moran_small_businesses <- tm_shape(Londonborough_merged_omit) +
  tm_polygons(
    'local_moran_I',
    alpha = 0.5,
    palette = MoranColours,
    title = 'Local Moran I',midpoint=NA
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_compass(type = "arrow", position = c("right", "top")) +tm_scale_bar(position =
                                                                                  c('right', 'top'), text.size = 0.6)

tm_moran_small_businesses

tmap_save(tm_moran_small_businesses,'local_moran_small_business.png')

#local moran's I

quadrant_small_businesses <- vector(mode='numeric',length=nrow(Londonborough_merged_omit))

#centers the variable of interest around its mean
m.small_businesses <- Londonborough_merged_omit$x2020-mean(Londonborough_merged_omit$x2020)

#centers the local moran's around the mean
m.local_moran <- local_moran[,1]-mean(local_moran[,1])

#significance threshold
signif <- 0.05

#builds a data quadrant
quadrant_small_businesses[m.small_businesses>0 & m.local_moran>0] <- 4
quadrant_small_businesses[m.small_businesses<0 & m.local_moran<0] <- 1
quadrant_small_businesses[m.small_businesses<0 & m.local_moran>0] <- 2
quadrant_small_businesses[m.small_businesses>0 & m.local_moran<0] <- 3
quadrant_small_businesses[local_moran[,5]>signif] <- 0

#plotting in r
brks <- c(0,1,2,3,4)
colors <- c('white','blue',rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),'red')

plot(
  Londonborough_merged_omit,
  border = 'lightgray',
  main = 'LISA cluster',
  col = colors[findInterval(quadrant_small_businesses, brks, all.inside = FALSE)],
  max.plot = 1
)

legend('bottomright',legend=c('insignificant',
                              'low-low','low-high',
                              'high-low','high-high'),fill=colors,bty='n')
#local moran's I

quadrant <- vector(mode='numeric',length=nrow(Londonborough_merged_omit))

#centers the variable of interest around its mean
m.resturants_number <- Londonborough_merged_omit$resturants_number-mean(Londonborough_merged_omit$resturants_number)

#centers the local moran's around the mean
m.local_moran <- local_moran[,1]-mean(local_moran[,1])

#significance threshold
signif <- 0.05

#builds a data quadrant
quadrant[m.resturants_number>0 & m.local_moran>0] <- 4
quadrant[m.resturants_number<0 & m.local_moran<0] <- 1
quadrant[m.resturants_number<0 & m.local_moran>0] <- 2
quadrant[m.resturants_number>0 & m.local_moran<0] <- 3
quadrant[local_moran[,5]>signif] <- 0

#plotting in r
brks <- c(0,1,2,3,4)
colors <- c('white','blue',rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),'red')

plot(
  Londonborough_merged_omit,
  border = 'lightgray',
  main = 'LISA cluster',
  col = colors[findInterval(quadrant, brks, all.inside = FALSE)],
  max.plot = 1
)

legend('bottomright',legend=c('insignificant',
                              'low-low','low-high',
                              'high-low','high-high'),fill=colors,bty='n')






