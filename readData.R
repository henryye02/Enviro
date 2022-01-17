library(rgdal)
library(sp)
library(dplyr)
library(magrittr)

#   ____________________________________________________________________________
#   Shapes                                                                  ####

suppressMessages({
  CTshapes <- readOGR("Data/Polygons/MYS_adm1.shp", 
                      layer = "MYS_adm1",
                      stringsAsFactors = FALSE)
})
print("Shapefiles complete")
state <- read.csv("Data/State_2019.csv")

state_data <- state %>%
  group_by(St)

state_data$St[16] <- "Trengganu"
is.element(state_data$St, CTshapes$NAME_1)
state_data < state_data[order(match(state_data$St,CTshapes$NAME_1)),]

pal <- colorBin(palette='Dark2', domain=state_data$Wat_Sup,bins=8)
cols <- c('Mean_Temp')



state %<>% 
  dplyr::select(one_of(cols)) %>% 
  mutate_each(
    funs(./100)
  ) %>% bind_cols(state[names(state) != cols])

# join prediction data with shapefile
# join prediction data with shapefile
GeoDF <- sp::merge(CTshapes,state,by.x = "NAME_1", by.y = "St", duplicateGeoms = TRUE,no.dups = TRUE)

p <- read.csv("Data/State_Data.csv")
p %<>% filter(Year <= 2018)


print("Zillow data complete")
vars <- read.csv("Data/VariableDescriptions.csv",
                  stringsAsFactors = FALSE)