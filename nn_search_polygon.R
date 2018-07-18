library(tidyverse)
library(tigris)
library(extrafont)
library(tidycensus)
library(patchwork)
library(randomcoloR)

epsg <- 102323

url <- "https://www6.sos.state.oh.us/ords/f?p=VOTERFTP:DOWNLOAD::FILE:NO:2:P2_PRODUCT_NUMBER:61" # this is Noble county

block_pop <- get_decennial(geography = "block", variables = "P0010001", state = "OH", county = "Noble")

blocks <- read_sf(dsn = "vinton", layer = "census_blocks_vinton") %>%
  st_transform(epsg)



#######################################################################

voterfile <- read_csv(url)

get_block <- function(x){
  block <- call_geolocator(voterfile$RESIDENTIAL_ADDRESS1[x], voterfile$RESIDENTIAL_CITY[x], voterfile$RESIDENTIAL_STATE[x])
}

voterfile_blocks <- map_chr(1:nrow(voterfile), get_block) # geocodes 89% 

voterfile_blocks

voterfile <- voterfile %>%
  mutate(BLOCK_GEOID = voterfile_blocks)

# find the number of points in each block

blocks_count <- voterfile %>%
  group_by(BLOCK_GEOID) %>%
  count() %>%
  drop_na() # covers 47% of total blocks


blocks <- blocks %>%
  left_join(blocks_count, by = c("GEOID10" = "BLOCK_GEOID"))



voters <- ggplot(blocks) +
  geom_sf(aes(fill = n), color = "white", stroke = .9) +
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
  left_join(block_pop, by = c("GEOID10" = "GEOID")) %>%
  mutate(sf_area = st_area(blocks_join),
         density = as.numeric(( value  / sf_area)))

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
  labs(title = "People per square mile in Vinton County, Ohio") +
  guides(color = guide_legend(override.aes = list(size=5)))

ppl + voters

precincts <- voterfile %>%
  group_by(BLOCK_GEOID, PRECINCT_NAME) %>%
  summarise(c=n()) %>% 
  filter(row_number(desc(c))==1)

precincts_geo <- blocks %>%
  left_join(precincts, by = c("GEOID10" = "BLOCK_GEOID"))

palette <- distinctColorPalette(20)

ggplot(precincts_geo) +
  geom_sf(aes(fill = PRECINCT_NAME), color = "white", stroke = .9) +
  scale_fill_manual(values = palette, na.value="#edf1f7", name = NULL) +
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
  labs(title = "Precincts in Vinton County, Ohio") +
  guides(color = guide_legend(override.aes = list(size=5)))
