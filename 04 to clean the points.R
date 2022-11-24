
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, usdm, dismo, rmapshaper, outliers, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Fonts -------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto Condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
path <- './tbl/speciesGBIF/Sirex noctilio fnal.csv'
path <- './tbl/speciesGBIF/Cryphonectria parasitica.csv'
tble <- read_csv(path)
tble

tble <- dplyr::select(tble, scientificName, lon = decimalLongitude, lat = decimalLatitude)

# World
wrld <- ne_countries(returnclass = 'sf', scale = 50, type = 'countries')
wrld <- filter(wrld, name != 'Antarctica')

# Climate -----------------------------------------------------------------
fles <- dir_ls('./tif/climate/wc21/10km')
bioc <- terra::rast(grep('bioc', fles, value = TRUE))

mask <- bioc[[1]] * 0 + 1
names(bioc) <- glue('bio_{1:19}')

# To remove duplicated by cell --------------------------------------------
cell <- terra::extract(mask, tble[,c('lon', 'lat')], cells = TRUE)
cell <- as_tibble(cell)
dupv <- duplicated(cell$cell)
tble <- mutate(tble, duplicated = dupv)
tble <- tble[!dupv,]

dir_create('./tbl/speciesInput')
write.csv(tble, glue('./tbl/speciesInput/{basename(path)}'), row.names = FALSE)

# To extract the climate --------------------------------------------------
vles <- terra::extract(bioc, tble[,c('lon', 'lat')]) %>% as_tibble()
vles <- cbind(tble, vles[,-1])
vles <- as_tibble(vles)
vles <- dplyr::select(vles, scientificName, lon, lat, bio_1:bio_19)
vles <- drop_na(vles)
write.csv(vles, glue('./tbl/speciesInput/{basename(path)}'), row.names = F)

# To make the outliers analysis -------------------------------------------
norm <- scores(vles[,c(4:ncol(vles))], 'z')
norm_na <- norm
norm_na[abs(norm_na) > 3.5] <- NA
normpoints <- cbind(vles[,c('lon', 'lat')], norm_na) %>% na.omit() %>% as_tibble()
normpoints <- normpoints[c('lon', 'lat')]

# To extract the values again ---------------------------------------------
vles <- as_tibble(cbind(normpoints, terra::extract(bioc, normpoints[,c('lon', 'lat')])))
vles <- vles[,-3]
name <- basename(path) %>% gsub('.csv', '', .) %>% glue('_swd_otl.csv')
write.csv(vles, glue('./tbl/speciesInput/{name}'), row.names = FALSE)

# To make the VIF analysis ------------------------------------------------
vif.res <- vif(x = as.data.frame(vles[,4:ncol(vles)]))
vif.step <- vifstep(x = as.data.frame(vles[,4:ncol(vles)]), th = 10)
vrs <- as.character(vif.step@results$Variables)
vles <- dplyr::select(vles, lon, lat, all_of(vrs))
name <- basename(path) %>% gsub('.csv', '', .) %>% glue('_swd_otl_vars.csv')
write.csv(vles, glue('./tbl/speciesInput/{name}.csv'), row.names = FALSE)

# Points to shapefile -----------------------------------------------------
shpf <- st_as_sf(x = vles, coords = c('lon', 'lat'), crs = 4326)
shpf <- st_transform(shpf, crs = 'ESRI:54030')

# To make the final map of presences --------------------------------------
nme <- basename(path) %>% gsub('.csv$', '', .)
gmap <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.4) + 
  geom_sf(data = shpf, col = '#943B3B', fill = NA, size = 0.3) + 
  ggtitle(label = nme) +
  coord_sf(crs = st_crs("ESRI:54030")) + 
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 40, hjust = 0.5)) 

ggsave(plot = gmap, filename = glue('./png/mapsInput/{basename(path) %>% gsub(".csv", ".png", .)}'), units = 'in', width = 9, height = 7, dpi = 300)

# To crate the pseudoabsences ---------------------------------------------
clss <- terra::extract(mask, vles[,c('lon', 'lat')], cells = T) %>% as_tibble() %>% setNames(c('ID', 'val', 'cell')) %>% pull(cell)
plot(mask)

plot(st_geometry(st_transform(shpf, 4326)), add = TRUE, col = 'red', pch = 16)

# To remove the cells with presences
mask <- terra::crop(mask, terra::vect(wrld)) %>% terra::mask(., terra::vect(wrld))
back <- mask
back[clss] <- NA
writeRaster(x = back, filename = glue('./tif/input/{gsub(".csv", "", basename(path))}/mask_back.tif'), overwrite = TRUE)

back <- randomPoints(mask = raster::raster(back), n = nrow(shpf) * 2)
back <- as_tibble(back)

plot(mask)
points(back$x, back$y, pch = 16, col = 'red')

# Extract the values for the presences 
back_vles <- terra::extract(bioc, back[,c('x', 'y')])[,-1]
back_vles <- as_tibble(cbind(back, back_vles))
back_vles <- mutate(back_vles, pb = 0) 
back_vles <- relocate(back_vles, pb, .before = x)

# To join both tables into only one ---------------------------------------
colnames(back_vles)
colnames(vles)
back_vles <- dplyr::select(back_vles, pb, lon = x, lat = y, colnames(dplyr::select(vles, starts_with('bio'))))
vles <- mutate(vles, pb = 1)
vles <- relocate(vles, pb, .before = lon)

fnal <- rbind(vles, back_vles)
name <- gsub('.csv', '', basename(path))

write.csv(fnal, glue('./tbl/speciesInput/{name}_swd_otl_vrs_back.csv'), row.names = F)

