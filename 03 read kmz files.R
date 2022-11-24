

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, rmapshaper, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load libraries ----------------------------------------------------------
fles <- dir_ls('./kml') %>% as.character()

srze <- st_read(fles[1])
srzn <- st_read(fles[2])

# Convert to table  -------------------------------------------------------
srze
unique(srze$Name)
srze_tble <- st_coordinates(srze) %>% as_tibble() %>% mutate(name = srze$Name) %>% setNames(c('x', 'y', 'z', 'name')) %>% dplyr::select(-z)
srze_tble
srzn_tble <- st_coordinates(srzn) %>% as_tibble() %>% mutate(name = srzn$Name) %>% setNames(c('x', 'y', 'z', 'name')) %>% dplyr::select(-z)

unique(srzn_tble$name)
unique(srze_tble$name)

# World shapefile ---------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)

# Fonts -------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto Condensed')
showtext_auto()

# Maps --------------------------------------------------------------------

# To make the map
gmp_sirez <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.3) + 
  geom_sf(data = srze, col = '#943B3B', size = 0.3) + 
  coord_sf(crs = st_crs("ESRI:54030")) + 
  ggtitle(label = 'Sirez noctilio - Evans') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 40, hjust = 0.5)) 

gmp_sirez_noctilio <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.3) + 
  geom_sf(data = srzn, col = '#943B3B', size = 0.3) + 
  coord_sf(crs = st_crs("ESRI:54030")) + 
  ggtitle(label = 'Sirez noctilio') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 40, hjust = 0.5)) 

ggsave(plot = gmp_sirez, filename = './png/speciesLocation/Sirez noctilio evans.png', units = 'in', width = 9, height = 5.3, dpi = 300)
ggsave(plot = gmp_sirez_noctilio, filename = './png/speciesLocation/Sirez noctilio.png', units = 'in', width = 9, height = 5.3, dpi = 300)

srze_tble
srzn_tble
srzn_alld <- rbind(srze_tble, srzn_tble)
colnames(srzn_alld) <- c('decimalLongitude', 'decimalLatitude', 'scientificName')
srzn_rawd <- read_csv('./tbl/speciesGBIF/Sirex noctilio.csv')
fnal <- bind_rows(srzn_rawd, srzn_alld)

write.csv(fnal, './tbl/speciesGBIF/Sirex noctilio fnal.csv', row.names = FALSE)
