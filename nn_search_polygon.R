library(tidyverse)
library(tigris)
library(extrafont)
library(tidycensus)
library(patchwork)
library(randomcoloR)
library(gstat)
library(sf)
library(lwgeom)

epsg <- 102323

url <- "https://www6.sos.state.oh.us/ords/f?p=VOTERFTP:DOWNLOAD::FILE:NO:2:P2_PRODUCT_NUMBER:82" # this is Vinton county

block_pop <- get_decennial(geography = "block", variables = "P0010001", state = "OH", county = "Vinton")

blocks <- read_sf(dsn = "vinton", layer = "census_blocks_vinton") %>%
  st_transform(epsg)



#######################################################################

voterfile <- read_csv(url)

get_block <- function(x){
  block <- call_geolocator(voterfile$RESIDENTIAL_ADDRESS1[x], voterfile$RESIDENTIAL_CITY[x], voterfile$RESIDENTIAL_STATE[x])
}

# # start_time <- Sys.time()
# # 
# # voterfile_blocks <- map_chr(1:nrow(voterfile), get_block) # geocodes 89% 
# # 
# # end_time <- Sys.time()
# # 
# # end_time - start_time
# 
# 
# voterfile <- voterfile %>%
#   mutate(BLOCK_GEOID = voterfile_blocks)

voterfile <- read_csv("vinton_voterfile.csv")

# find the number of points in each block

blocks_count <- voterfile %>%
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  group_by(BLOCK_GEOID) %>%
  summarise(n_blocks = n()) %>%
  drop_na() # covers 47% of total blocks


blocks <- blocks %>%
  left_join(blocks_count, by = c("GEOID10" = "BLOCK_GEOID"))



voters <- ggplot(blocks) +
  geom_sf(aes(fill = n_blocks), color = "white", stroke = .9) +
  scale_fill_continuous(low="#fff7f3", high="#7a0177", 
                        guide="colorbar",na.value="#edf1f7", name = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.position=c(.2, .1),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title = element_text(size = 18),
        text = element_text(family = "Corbel", color = "#6c727c"),
        legend.direction  = "horizontal",
        legend.background = element_rect(size=0.25, linetype="solid", 
                                         colour ="#6c727c")) +
  labs(title = "Valid addresses per census block in Vinton County, Ohio",
       subtitle = "Geocoded with tigris & transport foundry") +
  guides(color = guide_legend(override.aes = list(size=5)))

# plot population

blocks_join <- blocks %>%
  left_join(block_pop, by = c("GEOID10" = "GEOID"))

ppl <- ggplot(blocks_join) +
  geom_sf(aes(fill = value ), color = "white", stroke = .9) +
  scale_fill_continuous(low="#f7fcf0", high="#084081", 
                        guide="colorbar",na.value="#edf1f7", name = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.position=c(.2, .1),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title = element_text(size = 18),
        text = element_text(family = "Corbel", color = "#6c727c"),
        legend.direction  = "horizontal",
        legend.background = element_rect(size=0.25, linetype="solid", 
                                         colour ="#6c727c")) +
  labs(title = "Population by census block (raw numbers)") +
  guides(color = guide_legend(override.aes = list(size=7)))

ppl + voters

precincts <- voterfile %>% 
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  group_by(BLOCK_GEOID, PRECINCT_NAME) %>%
  summarise(c=n()) %>% 
  filter(row_number(desc(c))==1)

precincts_geo <- blocks %>%
  mutate(GEOID10 = as.character(GEOID10)) %>%
  left_join(precincts, by = c("GEOID10" = "BLOCK_GEOID")) %>%
  mutate(dimension = st_dimension(.)) %>%
  filter(!(is.na(dimension))) # take out empty polygons

palette <- distinctColorPalette(20)

ggplot(precincts_geo) +
  geom_sf(aes(fill = PRECINCT_NAME), color = "white", stroke = .9) +
  scale_fill_manual(values = palette, na.value="#edf1f7", name = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.position=c(.2, .2),
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
  guides(color = guide_legend(override.aes = list(size=4))) +
  guides(col = guide_legend(nrow = 8))

# find rook contiguity neighbors

st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")

precincts_geo_nb <- precincts_geo %>% mutate(NB_ROOK = st_rook(.))


precincts_nb <- precincts_geo_nb %>%
  st_set_geometry(NULL)

lookup_precinct <- function(index, precinct_name_var = "PRECINCT_NAME"){
  precinct <- precincts_nb[index, precinct_name_var]
  return(precinct)
}

create_precinct_vector <- function(){
  prec <- map(precincts_nb$NB_ROOK, lookup_precinct)
  prec_vec <- map(prec, pull)
  return(prec_vec)
}

precinct_vector <- create_precinct_vector()


precincts_nb <- precincts_nb %>%
  mutate(precinct_nn = precinct_vector)

# precincts_nb$NB_ROOK <- lapply(precincts_nb$NB_ROOK, function(x) if(identical(x, integer(0))) NA_integer_ else x) # convert zero length
# 
precincts_nb_sub <- precincts_nb %>%
  filter(!is.na(precinct_nn),
         !identical(precinct_nn, character(0))) 


clean_precincts_fun <- function(i){
  y <- precincts_nb_sub$precinct_nn[[i]]
  print(y)
  new_y <- y[!is.na(y)]
  return(new_y)
}

p_new <- map(1:nrow(precincts_nb_sub), clean_precincts_fun)

p_lengths <- map(1:length(p_new), function(x) length(p_new[[x]]))

precincts_nb_sub <- precincts_nb_sub %>%
  mutate(precinct_nn_clean = p_new,
         precinct_nn_length = p_lengths) %>%
  filter(precinct_nn_length > 0) # take out the blocks without meaningful neighbors

# for (i in 1:nrow(precincts_nb_sub)){
#   clean_precincts[[i]]<- ifelse(all(is.na(precincts_nb_sub$precinct_nn[[i]])), NA, precincts_nb_sub$precinct_nn[[i]][!is.na(precincts_nb_sub$precinct_nn[[i]])])
  #lengths[i] <- length(clean_precincts[[i]])
#}

for (i in 1:nrow(precincts_nb_sub)){
  precincts_nb_sub$PRECINCT_NAME[i] <- ifelse(is.na(precincts_nb_sub$PRECINCT_NAME[i]), names(which.max(table(precincts_nb_sub$precinct_nn[[i]]))), precincts_nb_sub$PRECINCT_NAME[i])
} # assign a precinct as the max of the vector of precincts


########################################################

# starting only with unassigned precincts

precincts_nb_sub_2 <- precincts_nb %>%
  filter(is.na(PRECINCT_NAME)) 


clean_precincts_fun <- function(i){
  y <- precincts_nb_sub_2$precinct_nn[[i]]
  print(y)
  new_y <- y[!is.na(y)]
  return(new_y)
}

p_new <- map(1:nrow(precincts_nb_sub_2), clean_precincts_fun)

p_lengths <- map(1:length(p_new), function(x) length(p_new[[x]]))

precincts_nb_sub_2 <- precincts_nb_sub_2 %>%
  mutate(precinct_nn_clean = p_new,
         precinct_nn_length = p_lengths) %>%
  filter(precinct_nn_length > 0) # take out the blocks without meaningful neighbors


for (i in 1:nrow(precincts_nb_sub_2)){
  precincts_nb_sub_2$PRECINCT_NAME[i] <- ifelse(is.na(precincts_nb_sub_2$PRECINCT_NAME[i]), names(which.max(table(precincts_nb_sub_2$precinct_nn[[i]]))), precincts_nb_sub_2$PRECINCT_NAME[i])
} # assign a precinct as the max of the vector of precincts

precincts_nb_full <- precincts_nb %>%
  left_join(precincts_nb_sub_2 %>% select(GEOID10, PRECINCT_NAME), by = "GEOID10") %>%
  mutate(PRECINCT_NAME.x = if_else(is.na(PRECINCT_NAME.x), PRECINCT_NAME.y, PRECINCT_NAME.x)) %>%
  select(-PRECINCT_NAME.y) %>%
  rename(PRECINCT_NAME = PRECINCT_NAME.x)

precincts_nb_full <- precincts_nb_full %>%
  left_join(precincts_geo %>% select(GEOID10, geometry))


palette <- distinctColorPalette(20)

ggplot(precincts_nb_full) +
  geom_sf(aes(fill = PRECINCT_NAME), color = "white", stroke = .9) +
  scale_fill_manual(values = palette, na.value="#edf1f7", name = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.position=c(.2, .2),
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
  guides(color = guide_legend(override.aes = list(size=4))) +
  guides(col = guide_legend(nrow = 8))






