# Packages
library(sf)
sf::sf_use_s2(FALSE)
library(rnaturalearth)
library(scales)

# Create a directory where to save generated figures and tables
if(!dir.exists("paper/figures")) dir.create("paper/figures")

# Load images
tab <- read.csv("data/images.csv")

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

# Load sites boundaries
bound <- list()
grid <- list()
for(k in 1:n){

  # Boundaries
  bound[[k]] <- st_read(paste0("data/gis/", sites[k], ".gpkg"), 
                        layer = "boundaries", quiet = TRUE)
  
  # Grid
  grid[[k]] <- st_read(paste0("data/gis/", sites[k], ".gpkg"), 
                       layer = "grid2000", quiet = TRUE)
  
}

# Map Europe [Figure 1 - Fig_main_map]
pdf("paper/figures/Fig_main_map.pdf", width = 4.5, height = 5.9, useDingbats = FALSE)

  colo <- c("#5F9DFB","#5F9DFB", "#5F9DFB", "#5F9DFB",
            "#F8766D", "#F8766D", "#F8766D")
  poso <- c(3,1,3,1,1,3,3)

  world <- ne_countries(scale = "medium")
  world <- st_crop(world, st_bbox(c(xmin = -15, 
                                    xmax = 50, 
                                    ymin = 30, 
                                    ymax = 75), 
                                  crs = st_crs(4326)))
  
  par(mar = c(0, 0, 0, 0))
  plot(st_geometry(world), xlim = c(-5, 28), ylim = c(37, 70),
       col = "lightgrey", border = "grey")
  for(k in 1:n){
    
    cent <- st_centroid(bound[[k]])
    plot(st_geometry(cent), col = colo[k], pch = 16, cex = 2, 
         add = TRUE)
    
    cent <- st_coordinates(cent)
    text(cent, labels = sites_name[k],
         pos = poso[k],
         cex = 1, font = 2)
  }
  
  legend("topleft", col=colo[c(1,5)], pch = 16, pt.cex = 2, 
         legend=c("Train / Val / Test", "External"), cex=1.2, bty="n") 
  

dev.off()

# Maps sites [Figure S1 - Fig_supp_sites]
pdf("paper/figures/Fig_supp_sites.pdf", width = 13.2, height = 6.6, useDingbats = FALSE)

  colo <- c("#5F9DFB","#5F9DFB", "#5F9DFB", "#5F9DFB",
            "#F8766D", "#F8766D", "#F8766D")
  panel <- c("a", "b", "c", "d", "e", "f", "g")
  surf <- c("326", "255", "265", "491", "5,782", "2,271", "3,657")
  
  mat <- matrix(c(1,1,2,2,3,3,4,4,
                  0,5,5,6,6,7,7,0), 
                nrow = 2, byrow = TRUE)
  layout(mat)
  
  for(k in 1:n){
    
    tabk <- tab[tab$site == sites[k],]
    
    par(mar = c(2, 0, 3, 0))
    plot(st_geometry(st_transform(grid[[k]], 3035)), 
         col = NA, border = "darkgrey", lwd = 2)
    plot(st_geometry(st_transform(bound[[k]], 3035)), 
         col = NA, border = colo[k], lwd = 2, add = TRUE)
    title(main = paste0("(", panel[k], ") ", sites_name[k], " [", 
                        surf[k], " km²]"), 
                        cex.main = 1.7)
    
  }

dev.off()

