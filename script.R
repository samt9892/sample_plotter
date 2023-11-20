#load libraries ----
library(maps)
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)
library(raster)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#SET: boundings of plots ----
boundaries <- list()
boundaries[["All"]] <- list(ODR_Xlim = c(90, 160), ODR_Ylim = c(-50, -5))
boundaries[["EEZ"]] <- list(ODR_Xlim = c(93, 109), ODR_Ylim = c(-17, -7))
boundaries[["CI"]] <- list(ODR_Xlim = c(105.3, 105.8), ODR_Ylim = c(-10.7, -10.4))
boundaries[["CKI"]] <- list(ODR_Xlim = c(96.7, 97), ODR_Ylim = c(-12.3, -12))

#SET: Voyage name ----
voyage <- "IOT"

#load background maps ----

#basic map ----
map <- ne_load(scale=10, category="physical", type='land', returnclass = 'sf') #if already downloaded
#map <- ne_download(scale=10, category="physical", type='land', returnclass = 'sf', load=T) #if need to downloaded

st_crs(map) #check crs


#colbys map ----
raster_extent <- extent(boundaries[["All"]]$ODR_Xlim[1], boundaries[["All"]]$ODR_Xlim[2],  #Define raster extent from the largest image 'all'
                      boundaries[["All"]]$ODR_Ylim[1], boundaries[["All"]]$ODR_Ylim[2])
raster_map <- raster("in_background_map/NE1_HR_LC_SR_W.tif") # Read the raster datafile
cropped_raster <- crop(raster_map, new_extent) # Crop the raster to match the defined extent

raster_polygons <- rasterToPolygons(cropped_raster, dissolve = TRUE) # Convert raster to polygons (each pixel as a polygon)
sf_raster <- st_as_sf(raster_polygons) # Convert polygons to sf object

rm(raster_map, raster_polygons, raster_extent) #cleanup variables

#load sampling sites ----
file_list <- list.files("in_sampling_sites", pattern = "\\.csv$", full.names = TRUE) # Get all CSV files
data_list <- list()  # Create an empty list to store data frames
for (file in file_list) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Extract file name without extension
  data <- read.csv(file)                                  # Read CSV file
  data <- data[complete.cases(data[c("long", "lat")]), ] # Filter out rows where long or lat are missing
  data <- st_as_sf(data, coords = c("long", "lat"))
  data_list[[file_name]] <- data  # Store data in the list with file name as key
}
rm(data)


file_list <- list.files("in_sampling_sites", pattern = "\\.csv$", full.names = TRUE) # Get all CSV files
data_list <- list()  # Create an empty list to store data frames
for (file in file_list) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Extract file name without extension
  data <- read.csv(file)  # Read CSV file
  data_list[[file_name]] <- data  # Store data in the list with file name as key
}
rm(data)


#load EEZ ----
EEZ<-st_read("in_eez/eez_boundaries_v12.shp")
EEZ <- subset(EEZ, EEZ1 == 'Australian Exclusive Economic Zone' | EEZ2 == 'Australian Exclusive Economic Zone' | 
                SOVEREIGN1 == 'Australia')

#load Marine Parks ----
file_list <- list.files("in_marine_parks", pattern = "\\.shp$", full.names = TRUE)
mp_list <- list()
for (file in file_list) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Extract file name without extension
  data <- st_read(file)  # Read CSV file
  mp_list[[file_name]] <- data  # Store data in the list with file name as key
}
rm(data)

st_crs(mp_list[[1]])


#load colour palette ----
# Colour blind friendly palette :
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# build plots ----

for (i in seq_along(boundaries)) {
  boundary <- boundaries[[i]]
  output_filename <- paste(voyage, names(boundaries[i]), ".png")
  # Prepare geom_point layers outside ggplot()
  geom_point_layers <- lapply(seq_along(data_list), function(j) {
    geom_point(data = data_list[[j]], mapping = aes(x = long, y = lat, fill = names(data_list)[j]), shape = 24, size = 2, stroke = 1)
  })
  # Combine geom_point layers into a single list
  all_layers <- c(
    list(
      geom_sf(fill = "grey60", lwd = 0.25),
      coord_sf(xlim = boundary$ODR_Xlim, ylim = boundary$ODR_Ylim),
      theme_bw(20),
      xlab(""),
      ylab(""),
      scale_fill_manual(name = "Sampling Campaign", values = cbPalette)  # Set legend title and colors
    ),
    geom_point_layers
  )
  
  map %>% 
    ggplot() +
    #geom_sf(data = EEZ, aes(), color = "red", linetype = "dashed", show.legend = "EEZ") +
    lapply(mp_list, function(mp_shapefile) {
      geom_sf(data = mp_shapefile, aes(), color = "darkgreen", fill = "darkgreen", alpha = 0.5)
    }) +
    all_layers +  # Use the combined list of layers form the sites
    theme(
      axis.text = element_text(size = 8),  # Adjust text size for axis ticks
      legend.position = "bottom",  # Position legend at the bottom outside the plot
      legend.box = "horizontal",  # Arrange legend items horizontally
      legend.key.size = unit(0.1, "lines"),  # Set smaller legend key size
      legend.text = element_text(size = 9, margin = margin(b = 5)),
      legend.title = element_text(size = 10)
    ) 
    ggsave(filename = output_filename, width = 15, height = 14, units = "cm", dpi = 300)
}
  