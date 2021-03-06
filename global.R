library(rgdal) # plot map
library(leaflet) # plot map
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # plot
library(plotly) # interactive plot
library(shiny) # shiny
library(shinydashboard) # shiny dashboard

# general data
footprint <- read.csv("countries.csv")

## data cleaning
footprint <- footprint %>% 
  mutate(Country = as.character(Country),
         GDP.per.Capita = as.numeric(gsub("[$,]","", footprint$GDP.per.Capita)),
         HDI = round(HDI, 2),
         Countries.Required = round(Countries.Required, 2),
         Biocapacity.Deficit = as.factor(ifelse(Biocapacity.Deficit>0, "Reserve", "Deficit"))) %>%
  rename(Status = Biocapacity.Deficit) %>% 
  select(-c(Data.Quality)) %>% 
  drop_na()

# data for scatterplot
scat_plot_data <- footprint %>% 
  select(Country, Population.millions, GDP.per.Capita, HDI, Total.Ecological.Footprint, Status) %>% 
  rename(Population.in.millions = Population.millions,
         Human.Development.Index = HDI,
         Ecological.Footprint = Total.Ecological.Footprint) %>% 
  mutate(text = paste0("Country: ", Country,"<br>",
                       "HDI: ", Human.Development.Index, "<br>",
                       "Ecological Footprint: ", Ecological.Footprint, "<br>",
                       "GDP per Capita: ", "$", GDP.per.Capita))

# data for display in table
datadis <- footprint %>% 
  mutate(Country = as.factor(Country)) %>% 
  select(Region, everything()) 
  # rename("Popullation in Millions")

colnames(datadis) <- gsub(pattern = "[.]", replacement = " ", colnames(datadis))

# data for leaflet

## tabular data
leaflet <- footprint %>% 
  dplyr::select(-c(2, 6:10, 12:16, 19:20))

## prepare `diff` data for coloring in leaflet
leaf <- leaflet %>% 
  mutate(diff = Total.Biocapacity - Total.Ecological.Footprint) %>% 
  select(-Population.millions) %>% 
  rename(NAME = Country) # renaming column for joining ID

## shape file
shape <- raster::shapefile("TM_WORLD_BORDERS_SIMPL-0.3.shp")

## combining tabular data into shape data
shape@data <- shape@data %>% dplyr::left_join(leaf, by = "NAME")

## replacing column manually for rows with different country name
shape@data[shape@data$NAME=="United States",c(12:17)] <- leaf[leaf$NAME=="United States of America", c(2:7)]
shape@data[shape@data$NAME=="Russia",c(12:17)] <- leaf[leaf$NAME=="Russian Federation",c(2:7)]
shape@data[shape@data$NAME=="Venezuela",c(12:17)] <- leaf[leaf$NAME=="Venezuela, Bolivarian Republic of",c(2:7)]
shape@data[shape@data$NAME=="Republic of Moldova",c(12:17)] <- leaf[leaf$NAME=="Moldova",c(2:7)]
shape@data[shape@data$NAME=="The former Yugoslav Republic of Macedonia",c(12:17)] <- leaf[leaf$NAME=="Macedonia TFYR",c(2:7)]
shape@data[shape@data$NAME=="Iran (Islamic Republic of)",c(12:17)] <- leaf[leaf$NAME=="Iran, Islamic Republic of",c(2:7)]
shape@data[shape@data$NAME=="Democratic Republic of the Congo",c(12:17)] <- leaf[leaf$NAME=="Congo, Democratic Republic of",c(2:7)]
shape@data[shape@data$NAME=="United Republic of Tanzania",c(12:17)] <- leaf[leaf$NAME=="Tanzania, United Republic of",c(2:7)]
shape@data[shape@data$NAME=="Burma",c(12:17)] <- leaf[leaf$NAME=="Myanmar",c(2:7)]

## create a color palette with handmade bins
library(RColorBrewer)
mybins <- c(-Inf,-5,0,5,Inf)
mypalette <- colorBin(palette="RdYlGn", 
                      domain=shape@data$diff, 
                      na.color="transparent", 
                      bins=mybins)

## prepare label
mytext <- paste(shape@data$NAME) %>%
  lapply(htmltools::HTML)

## prepare pop-up
popup_shape <- paste("<h3><b>", shape@data$NAME, "</b></h3>", 
                     "Status: ", shape@data$Status, "<br>", 
                     "Ecological Footprint: ", shape@data$Total.Ecological.Footprint, " gha <br>",
                     "Biocapacity: ", shape@data$Total.Biocapacity, " gha <br>",
                     "HDI: ", shape@data$HDI, "<br>",
                     "GDP per Capita: ", "$", shape@data$GDP.per.Capita, "<br>", 
                     sep="")

# Inputs

selectRegion <- unique(footprint$Region)
selectCountry <- unique(footprint$Country)


