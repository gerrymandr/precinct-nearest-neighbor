library(tidyverse)
library(sf)
library(nngeo)
library(patchwork)

# read in shapefiles and voter file

# edit this section

epsg <- 102323

voterfile_sf <- read_sf(dsn = "noble", layer = "noble_geo_done") %>%
  select(-c(PRECINCT_N, PRECINCT_C)) %>%
  rename(PRECINCT_NAME = new_PRECIN, PRECINCT_CODE = new_PREC_1) %>%
  st_transform(epsg)

precinct_name <- 'PRECINCT_NAME'

blocks <- read_sf(dsn = "noble", layer = "census_blocks_noble") %>%
  st_transform(epsg)

random_points <- read_sf(dsn = "noble", layer = "random_points_5") %>%
  mutate(global_id = row_number()) %>%
  st_transform(epsg)

k <- 5 # neighbors

# calculate nearest neighbor

# DO NOT EDIT 

neighbors <- st_nn(random_points, voterfile_sf, k = k)

random_nn <- random_points %>%
  mutate(nn = neighbors) # dataframe that includes the neighbors as a variable

voterfile <- voterfile_sf %>%
  st_set_geometry(NULL)

# neighbors_precinct <- list()
# for (j in 1:nrow(random_nn)){ # finds the precincts of the closest neighbors
#   i_neighbors <- c()
#   for (i in random_nn$nn[j]){
#     i_neighbors <- c(i_neighbors, voterfile[i, precinct_name])
#   }
#   neighbors_precinct[j] <- i_neighbors
# }

lookup_precinct <- function(index, precinct_name_var = "PRECINCT_NAME"){
  precinct <- voterfile[index, precinct_name_var]
  return(precinct)
}

create_precinct_vector <- function(){
  prec <- map(random_nn$nn, lookup_precinct)
  prec_vec <- map(prec, pull)
  return(prec_vec)
}

precinct_vector <- create_precinct_vector()


random_nn <- random_nn %>% # attach names of neighboring precincts
  mutate(precinct_nn = precinct_vector)



random_nn$precinct_class_nn <- NA
for (i in 1:nrow(random_nn)){
  random_nn$precinct_class_nn[i] <- names(which.max(table(random_nn$precinct_nn[i])))
} # assign a precinct as the max of the vector of precincts


