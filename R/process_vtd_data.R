library(rgdal)
library(maptools)
library(tigris)
library(rgeos)

## Read in the shape data
vtds <- readOGR(dsn = "data-raw", layer = "VTDs", 
                stringsAsFactors = FALSE)

tarrant_vtds <- vtds[vtds$CNTY == "439", ]

dallas_vtds <- vtds[vtds$CNTY == "113", ]

denton_vtds <- vtds[vtds$CNTY == "121", ]

collin_vtds <- vtds[vtds$CNTY == "85", ]

rockwall_vtds <- vtds[vtds$CNTY == "397", ]

### Tarrant County

source("R/functions.R")

tarrant <- process_tarrant("data-raw/tarrant/tarrant_nov9.xlsx")

# Need to dissolve some precincts with the same name
tarrant_vtds$CNTYVTD <- str_sub(tarrant_vtds$CNTYVTD, 1, 7)

tarrant_dissolved <- gUnaryUnion(tarrant_vtds, id = tarrant_vtds$CNTYVTD)

# De-dup the original data slot, then give back to the dissolved precincts
tvtd_data <- tarrant_vtds@data %>%
  group_by(CNTYVTD) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

row.names(tarrant_dissolved) <- as.character(1:694)

tarrant_inter <- SpatialPolygonsDataFrame(tarrant_dissolved, 
                                         tvtd_data)

tarrant_shape <- geo_join(tarrant_inter, tarrant, "CNTYVTD", 
                          "precinct", how = "inner")

tarrant_dots <- votes_to_xy(tarrant_shape, 10)


### Dallas County

dallas <- process_dallas("data-raw/dallas/dallas_nov9.xlsx")

# Need to dissolve some precincts with the same name
dallas_vtds$CNTYVTD <- str_sub(dallas_vtds$CNTYVTD, 1, 7)

dallas_dissolved <- gUnaryUnion(dallas_vtds, id = dallas_vtds$CNTYVTD)

# De-dup the original data slot, then give back to the dissolved precincts
dvtd_data <- dallas_vtds@data %>%
  group_by(CNTYVTD) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

row.names(dallas_dissolved) <- as.character(1:797)

dallas_inter <- SpatialPolygonsDataFrame(dallas_dissolved, 
                                         dvtd_data)

dallas_shape <- geo_join(dallas_inter, dallas, "CNTYVTD", "precinct", 
                          how = "inner")

dallas_dots <- votes_to_xy(dallas_shape, 10)

## Denton County

denton <- process_denton("data-raw/denton/denton_nov9.xlsx")

denton_shape <- geo_join(denton_vtds, denton, "CNTYVTD", "precinct", 
                         how = "inner")

denton_dots <- votes_to_xy(denton_shape, 10)

## Collin County

collin <- process_collin()

collin_shape <- geo_join(collin_vtds, collin, "CNTYVTD", "precinct")

collin_dots <- votes_to_xy(collin_shape, 10)

## Rockwall County

rockwall <- process_rockwall()

rockwall_shape <- geo_join(rockwall_vtds, rockwall, "CNTYVTD", "precinct")

rockwall_dots <- votes_to_xy(rockwall_shape, 10)


## Put 'em together and export!

dfw <- bind_rows(tarrant_dots, dallas_dots, denton_dots, 
                 collin_dots, rockwall_dots)

write_csv(dfw, "data/dfw_dots.csv")

## Generate a 1 dot = 25 as well for zoomed-out representation

dot_list = list(tarrant_shape, dallas_shape, denton_shape, 
                collin_shape, rockwall_shape)

dfw25 <- bind_rows(
  lapply(dot_list, function(x) {
    votes_to_xy(x, 25)
  })
)

write_csv(dfw25, "data/dfw_dots25.csv")








##############
# Let's test some code
##############

# set.seed(1983)
# 
# source("R/process_precinct_data.R")
# 
# tarrant <- process_data("data-raw/tarrant/tarrant.xlsx", "439")
# 
# tarrant$trump <- sample(50:100, nrow(tarrant), replace = TRUE)
# 
# tarrant$clinton <- sample(50:100, nrow(tarrant), replace = TRUE)
# 
# tarrant$johnson <- sample(10:30, nrow(tarrant), replace = TRUE)
# 
# tarrant$stein <- sample(5:15, nrow(tarrant), replace = TRUE)
# 
# tarrant$other <- sample(5:15, nrow(tarrant), replace = TRUE)
# 
# ## Make some dots
# 
# tarrant_joined <- geo_join(tarrant_vtds, tarrant, 
#                            "CNTYVTD", "precinct", how = "inner")
# 
# trump_dots <- dotsInPolys(tarrant_joined, tarrant_joined$trump)
# 
# proj4string(trump_dots) <- proj4string(tarrant_joined)
# 
# trump_dots_xy <- spTransform(trump_dots, 
#                              CRS("+proj=longlat +datum=WGS84"))
# 
# trump_dots$longitude <- coordinates(trump_dots_xy)[,1]
# trump_dots$latitude <- coordinates(trump_dots_xy)[,2]
# 
# trump_out <- select(trump@data, precinct)
# 
# ## Get a CSV-generating function
# 
# votes_to_xy <- function(vtds, scaling_factor = 1) {
#   
#   values <- c("trump", "clinton", "johnson", 
#               "stein", "other")
#   
#   xys <- bind_rows(
#     lapply(values, function(value) {
#       
#       vtds[[value]] <- vtds[[value]] / scaling_factor
#       
#       dots <- dotsInPolys(vtds, vtds[[value]])
#       
#       proj4string(dots) <- proj4string(vtds)
#       
#       dots_xy <- spTransform(dots, CRS("+proj=longlat +datum=WGS84"))
#       
#       dots_xy$longitude <- coordinates(dots_xy)[,1]
#       dots_xy$latitude <- coordinates(dots_xy)[,2]
#       
#       dots_xy$candidate <- value
#       
#       return(dots_xy@data)
#       
#     })
#   )
#   
# }
# 
# # Test the function
# 
# tarr_df <- votes_to_xy(tarrant_joined, 3)
# 
# write_csv(tarr_df, "data/test_data.csv")
# 
# tarr_df <- votes_to_xy(tarrant_joined, 10)
# 
# write_csv(tarr_df, "data/test_data_10.csv")
