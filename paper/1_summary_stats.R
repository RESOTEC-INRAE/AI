library(data.table)
library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(elevatr)
library(xtable)
library(stringr)
sf::sf_use_s2(FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Table of site characteristics [Table 1 - tab_main_sites]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Raster of land cover
rast_landcover = terra::rast("data/gis/environmental/U2018_CLC2018_V2020_20u1.tif")

# Raster of precipitation
rast_prec = terra::rast("data/gis/environmental/CHELSA_bio12_1981-2010_V.2.1.tif")

# Raster of mean annual temperature
rast_temp = terra::rast("data/gis/environmental/CHELSA_bio01_1981-2010_V.2.1.tif")

# Sites name
sites_name <- c("Carpathians", 
                "Danube",
                "Dovre",
                "French Alps", 
                "Sierra Nevada",
                "Stubai Valley",
                "Vinschgau")

# List sites
sites <- list.files(path = "data/gis")
sites <- sites[sites != "environmental"]
sites <- substr(sites, 1, nchar(sites)-5)
n <- length(sites)

# Reorder sites
order_sites <- c(1,4,6,7,2,3,5)
sites <- sites[order_sites]
sites_name <- sites_name[order_sites]

# Loop on all sites
for(i in 1:length(sites)){
  
  # Extract boundaries of the site
  boundaries.site.in = terra::vect(
    paste0("data/gis/", sites[i], ".gpkg"), layer = "boundaries")
  
  # Extract elevation data for this site
  # - Get raster
  elevation_raster = get_elev_raster(
    locations = sf::st_as_sf(boundaries.site.in), z = 9,  clip = "locations")
  # - Convert to SpatRaster
  elevation_raster = rast(elevation_raster)
  # Extract statistics
  elevation.i = terra::extract(
    elevation_raster, boundaries.site.in, 
    fun = function(x) c(min = min(x, na.rm = TRUE),
                        mean = mean(x, na.rm = TRUE),
                        max = max(x, na.rm = TRUE)))
  # Extract land cover for the site
  land.cover.i = extract(rast_landcover, boundaries.site.in, fun = table) %>%
    #pivot_longer(cols = colnames(.)[2:dim(.)[2]], names_to = "landcover_cat", 
    #             values_to = "landcover_npixel")  %>%
    rename(landcover_cat = LABEL3) %>%
    mutate(
      synthesized_class = case_when(
        # Urban
        landcover_cat %in% c(
          "Continuous urban fabric",
          "Discontinuous urban fabric",
          "Green urban areas", 
          "Industrial or commercial units",
          "Road and rail networks and associated land",
          "Port areas",
          "Airports",
          "Mineral extraction sites",
          "Dump sites",
          "Construction sites",
          "Sport and leisure facilities"
        ) ~ "Urban",
        
        # Agriculture
        landcover_cat %in% c(
          "Non-irrigated arable land",
          "Permanently irrigated land",
          "Rice fields",
          "Vineyards",
          "Fruit trees and berry plantations",
          "Olive groves",
          "Pastures",
          "Annual crops associated with permanent crops",
          "Complex cultivation patterns",
          "Land principally occupied by agriculture, with significant areas of natural vegetation",
          "Agro-forestry areas"
        ) ~ "Agriculture",
        
        # Forest
        landcover_cat %in% c(
          "Broad-leaved forest",
          "Coniferous forest",
          "Mixed forest"
        ) ~ "Forest",
        
        # Grassland/Natural Vegetation
        landcover_cat %in% c(
          "Natural grasslands",
          "Moors and heathland",
          "Sclerophyllous vegetation",
          "Transitional woodland-shrub"
        ) ~ "Grassland/Shrubland",
        
        # Water
        landcover_cat %in% c(
          "Water courses",
          "Water bodies",
          "Coastal lagoons",
          "Estuaries",
          "Sea and ocean"
        ) ~ "Water",
        
        # Wetlands
        landcover_cat %in% c(
          "Inland marshes",
          "Peat bogs",
          "Salt marshes",
          "Salines",
          "Intertidal flats"
        ) ~ "Wetlands",
        
        # Glacier / Snow
        landcover_cat %in% c(
          "Glaciers and perpetual snow"
        ) ~ "Snow",
        
        # Barren/Sparse Vegetation
        landcover_cat %in% c(
          "Beaches, dunes, sands",
          "Bare rocks",
          "Sparsely vegetated areas",
          "Burnt areas"
        ) ~ "Barren/Sparse Vegetation",
        
        # Other/No Data
        landcover_cat == "NODATA" ~ "Other",
        
        TRUE ~ NA_character_)) %>%
    group_by(synthesized_class) %>%
    summarize(sumpixel = sum(count, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(prop = sumpixel/sum(sumpixel, na.rm = TRUE), 
           perc = paste0(round(prop, digits = 3)*100, " %")) %>%
    dplyr::select(synthesized_class, perc) %>%
    filter(synthesized_class != "Other") %>%
    mutate(site = sites[i])
  
  # Extract temperature and precipitations
  data.clim.i = data.frame(
    site = sites[i], 
    temperature = as.numeric(extract(rast_temp, boundaries.site.in, fun = "mean")[2]), 
    precipitation = as.numeric(extract(rast_prec, boundaries.site.in, fun = "mean")[2]), 
    elevation = as.numeric(extract(elevation_raster, boundaries.site.in, fun = "mean")[2])) %>%
    mutate(temperature = paste0(round(temperature, digits = 1), " °C"), 
           precipitation = paste0(round(precipitation, digits = 0), ' mm'), 
           elevation = paste0(round(elevation, digits = 0), ' m'))
  
  # Assemble data
  if(i == 1){
    data.clim = data.clim.i
    land.cover = land.cover.i
  }else{
    data.clim = rbind(data.clim, data.clim.i)
    land.cover = rbind(land.cover, land.cover.i)
  } 
  
}

# Final formatting
out = land.cover  %>%
  pivot_wider(names_from = "synthesized_class", values_from = "perc") %>%
  replace(is.na(.), "0.0%") %>%
  distinct() %>%
  left_join(data.clim, by = "site") %>%
  mutate(site = gsub("\\_", "\\ ", site)) %>%
  pivot_longer(cols = colnames(.)[2:dim(.)[2]], names_to = "col", values_to = "val") %>%
  mutate(col = case_when(col == "temperature" ~ "Mean temperature", 
                         col == "precipitation" ~ "Annual rainfall", 
                         col == "elevation" ~ "Mean elevation", 
                         TRUE ~ col)) %>%
  pivot_wider(names_from = "site", values_from = "val") %>%
  rename(" " = "col")
colnames(out) = c("", sites_name)

# Make latex table
dir.create("paper/tables")
print(xtable(out, type = "latex", label = "table_sites",
             caption = paste0("Environmental conditions (mean annual temperature", 
                              ", annual rainfall and mean elevation) and percentage", 
                              " of land cover of the seven study sites")), 
      include.rownames = FALSE, include.colnames = TRUE, size = "\\small",
      hline.after = c(0, 3, dim(out)[1]), caption.placement = "top", 
      align = c("c","c","c","c","c|","c","c","c"),
      file = "paper/tables/table_sites.tex")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Table of support per site [Table 2 - tab_main_support_site]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Import table [seed 14 as example]
support <- read.csv("outputs/tables/support_dataset_14.csv")
support_test <- read.csv(paste0("outputs/test/prediction_revue_balanced.csv"))
support_ext <- read.csv(paste0("outputs/external/prediction_revue_balanced.csv"))

# Loop over site for test and external
test <- rep(0,n)
ext <- rep(0,n)
for(k in 1:n){
  if(k <= 4){
    test[k] <- sum(support_test$site == sites[k])
  }else{
    ext[k] <- sum(support_ext$site == sites[k])
  }
}
support$test <- test
support$external <- ext
support <- rbind(support, c("Total", apply(support[,-1],2,sum)))
support[1:7,1] <- sites_name

# Print in tex format
tex=paste0("\textbf{", support[,1], "}", " & ",
           support[,2], " & ",
           support[,3], " & ",
           support[,4], " & ",
           support[,5], " & ",
           support[,6], " & ",
           support[,7], " \\")
print(data.frame(tex), row.names = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Table of support per label [Table 3 - tab_main_support_label]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Import table [seed 14 as example]
support <- read.csv("outputs/tables/support_label_14.csv")
support_test <- read.csv(paste0("outputs/test/classification_report_balanced.csv"))
support_ext <- read.csv(paste0("outputs/external/classification_report_balanced.csv"))

# Extract supports
support$test <- support_test$support[1:5]
support$external <- support_ext$support[1:5]

# Reorder
support <- support[c(2,1,4,5,3),]

# Print in tex format
tex=paste0(support[,1], " & ",
           support[,2], " & ",
           support[,3], " & ",
           support[,4], " & ",
           support[,5], " & ",
           support[,6], " & ",
           support[,7], " \\")
print(data.frame(tex), row.names = FALSE)
