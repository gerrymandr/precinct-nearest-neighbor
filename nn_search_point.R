library(tidyverse)
library(sf)
library(nngeo)
library(randomcoloR)

# read in shapefiles and voter file

# edit this section

epsg <- 102323

voterfile_sf <- read_sf(dsn = "vinton", layer = "vinton_geo_done") %>%
  select(-c(PRECINCT_N, PRECINCT_C)) %>%
  rename(PRECINCT_NAME = new_PRECIN, PRECINCT_CODE = new_PREC_1) %>%
  st_transform(epsg)

precinct_name <- 'PRECINCT_NAME'

blocks <- read_sf(dsn = "vinton", layer = "census_blocks_vinton") %>%
  st_transform(epsg)

random_points <- read_sf(dsn = "vinton", layer = "random_points5") %>%
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

palette <- distinctColorPalette(20)

ggplot(voterfile_sf) +
  geom_sf(data = blocks, fill = "#EDF1F7", color = "white") +
  geom_sf(aes(color = PRECINCT_NAME)) +
  scale_fill_manual(values = palette, na.value="#edf1f7", name = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title = element_text(size = 18),
        text = element_text(family = "Corbel", color = "#6c727c")) +
  labs(title = "Precinct assignments for voterfile addresses in Vinton County, Ohio")

# assign a block to a precinct

random_nn_block <- st_join(blocks, random_nn) %>%
  select(names(random_nn), GEOID10) %>%
  drop_na(global_id)

block_classification <- random_nn_block %>%
  group_by(GEOID10) %>%
  summarise(precinct = tail(names(sort(table(precinct_class_nn))), 1))

precinct_plot <- ggplot(block_classification) + geom_sf(aes(fill = precinct))

precinct_plot

blocks_dissolve <- block_classification %>%
  group_by(precinct) %>%
  summarise(n_blocks = n())

palette <- distinctColorPalette(20)

ggplot(blocks_dissolve) +
  geom_sf(aes(fill = precinct), color = "white") +
  scale_fill_manual(values = palette, na.value="#edf1f7", name = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.position=c(-.1, .4),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title = element_text(size = 18),
        text = element_text(family = "Corbel", color = "#6c727c"),
        legend.direction  = "vertical",
        legend.background = element_rect(size=0.25, linetype="solid", 
                                         colour ="#6c727c")) +
  labs(title = "Precinct assignments for census blocks in Vinton County, Ohio") +
  guides(color = guide_legend(override.aes = list(size=2), byrow = TRUE, ncol = 3)) 

# save the block classification shapefile with

# st_write(block_classification, "block_classification.shp")