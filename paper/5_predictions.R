# Packages
library(dplyr)
library(ggplot2)
library(data.table)
library(sf)
library(rnaturalearth)
library(tidyr)
library(cowplot)
sf::sf_use_s2(FALSE)

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

# Load metadata on images
data_images <- read.csv("data/images.csv")

# Initialize the list to store plots
plotlist.out = vector(mode = "list", length = length(sites))
maplist.out = vector(mode = "list", length = length(sites))

# Attribute name to each plot
names(plotlist.out) = sites
names(maplist.out) = sites

# Vector of grids
grid_vec = paste0("grid", c(250, 500, 1000, 2000, 5000))

# Initialize table with statistics per grid
table.stat.grid = data.frame(site = character(0), grid = character(0), 
                             ncells = numeric(0), rsquare = numeric(0), 
                             rmse = numeric(0), mae = numeric(0)) 

# Loop on all sites
for(i in 1:length(sites)){
  
  # Identify site
  site.in = sites[i]
  
  # Load predictions
  data.predict.in_file = paste0("outputs/sites/", site.in, "/prediction_revue_full.csv")
  data.predict.in = fread(data.predict.in_file) %>% 
    mutate(image = as.numeric(image)) 
  colnames(data.predict.in) = gsub("ŷ", "u", colnames(data.predict.in))
  
  # Metadata for this site
  data_images.in = subset(data_images, site == site.in)
  
  # Loop on all grids
  for(j in 1:length(grid_vec)){
    
    # Read grid for this site
    grid.in = st_read(paste0("data/gis/", site.in, ".gpkg"), 
                      layer = grid_vec[j], quiet = TRUE)
    
    # Final formatting of the image data
    pred.per.cell = data_images.in %>%
      # Add an ID for each interaction
      mutate(id_interaction = paste(owner_date, .data[[grid_vec[j]]], sep = "_")) %>%
      # Add information on prediction
      left_join(data.predict.in %>% mutate(site = tolower(site)), 
                by = c("image", "site")) %>%
      # Group predictions by interaction
      group_by(.data[[grid_vec[j]]], id_interaction) %>%
      summarize(u_human = ifelse(any(u_human), 1, 0), 
                y_human = ifelse(any(y_human), 1, 0), 
                u_anthropic = ifelse(any(u_anthropic), 1, 0), 
                y_anthropic = ifelse(any(y_anthropic), 1, 0), 
                u_vegetation = ifelse(any(u_vegetation), 1, 0), 
                y_vegetation = ifelse(any(y_vegetation), 1, 0), 
                u_rock = ifelse(any(u_rock), 1, 0), 
                y_rock = ifelse(any(y_rock), 1, 0), 
                u_snow = ifelse(any(u_snow), 1, 0), 
                y_snow = ifelse(any(y_snow), 1, 0)) %>%
      # Minor formatting
      ungroup() %>% 
      pivot_longer(cols = colnames(.)[3:dim(.)[2]], 
                   names_to = "variable", values_to = "value") %>%
      separate(col = "variable", into = c("type", "category"), sep = "_") %>%
      mutate(type = ifelse(type == "y", "observed", "predicted")) %>%
      pivot_wider(names_from = "type", values_from = "value") %>%
      # Sum by cell
      group_by(.data[[grid_vec[j]]], category) %>%
      summarise(predicted = sum(predicted, na.rm = TRUE), 
                observed = sum(observed, na.rm = TRUE))
    
    # Fit model of observed vs predicted
    mod = lm(predicted ~ observed*category, data = pred.per.cell)
    
    # Calculate goodness of fit
    rsquare = 1 -
      sum((pred.per.cell$observed - pred.per.cell$predicted)^2) /
      sum((pred.per.cell$observed - mean(pred.per.cell$observed))^2)
    pgp = 2*sum(pmin(pred.per.cell$observed, pred.per.cell$predicted)) /
      (sum(pred.per.cell$observed) + sum(pred.per.cell$predicted))
 
    # Fill the stat table
    table.stat.grid = rbind(table.stat.grid, data.frame(
      site = site.in, grid = grid_vec[j], ncells = dim(grid.in)[1], 
      rsquare = rsquare, pgp = pgp))
    
    # Only make the plot if it is the chosen grid
    if(grid_vec[j] == "grid2000"){
      
      # Identify the maximum value in the data (useful to position text in the plot)
      max.val.ij = max(c(pred.per.cell$predicted, pred.per.cell$observed), na.rm = TRUE)
      
      # - Make plot
      plot.in = pred.per.cell %>%
        ungroup() %>%
        mutate(fit = predict(mod, newdata = .), 
               predicted = predicted + rnorm(dim(.)[1], 0, 0.1)) %>%
        ggplot(aes(x = observed, y = predicted, color = category)) + 
        geom_point(alpha = 0.4) + 
        geom_line(aes(y = fit)) + 
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
        xlab("Observed interactions") + 
        ylab("Predicted interactions") +
        scale_color_manual(values = c(`anthropic` = "#495057", `human` = "#E5989B", 
                                      `rock` = "#582F0E", `snow` = "#0077B6",
                                      `vegetation` = "#386641")) +
        theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
              panel.background = element_rect(color = 'black', fill = 'white'), 
              plot.title = element_text(hjust = 0.5, size = 18), 
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 14), 
              legend.text = element_text(size = 18),
              legend.title = element_blank(), 
              legend.key = element_blank()) + 
        geom_text(data = data.frame(observed = 0.08*max.val.ij,
                                    predicted = 0.95*max.val.ij, category = NA),
                  label = paste0("R\u00b2 = ", round(rsquare, digits = 2),
                                 "\nPGP = ", round(pgp, digits = 2)),
                  size = 5, color = "black", hjust = 0, vjust = 0.9) +
        ggtitle(stringr::str_to_title(gsub("\\_", "\\ ", site.in)))
      
      # - Save to list
      plotlist.out[[i]] = plot.in + theme(legend.position = "none")
      
      # Make a map of observed vs predicted per feature
      map.in = grid.in %>%
        left_join((pred.per.cell %>% 
                     pivot_longer(names_to = "prediction", values_to = "value", 
                                  cols = all_of(c("observed", "predicted"))) %>%
                     mutate(cat_predict = paste0(category, "_", prediction)) %>%
                     select(-category, -prediction) %>%
                     pivot_wider(names_from = "cat_predict", values_from = "value")), 
                  by = setNames(grid_vec[j], "idcell")) %>% 
        pivot_longer(names_to = "cat_predict", values_to = "value", cols = all_of(
          colnames(.)[2:(dim(.)[2]-1)])) %>%
        mutate(Interactions = ifelse(is.na(value), 0, value)) %>%
        separate(col = "cat_predict", into = c("category", "prediction"), sep = "_") %>%
        mutate(prediction = stringr::str_to_title(prediction)) %>%
        ggplot(aes(geometry = geom, fill = Interactions)) + 
        geom_sf(color = "lightgrey") + 
        facet_grid(prediction ~ category) + 
        scale_fill_gradient(low = "white", high = "#D1495D") +
        theme(panel.grid = element_blank(), 
              panel.background = element_rect(color = 'black', fill = 'white'), 
              plot.title = element_text(hjust = 0.5, size = 18), 
              strip.background = element_blank(),
              strip.text = element_text(size = 15), 
              legend.text = element_text(size = 13),
              legend.title = element_text(size = 15),
              axis.text = element_blank(), 
              axis.ticks = element_blank()) + 
        ggtitle(stringr::str_to_title(gsub("\\_", "\\ ", site.in)))
      # - Store in list
      maplist.out[[i]] = map.in
      
    }
    
  }
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Figure on predictions per site [Figure 4 - Fig_main_mapfeatures]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# - Extract legend for the main plot
plot.in_legend = cowplot::get_legend(plot.in)
# - Divide into val / train / test and external
plotlist_train = plotlist.out[c("carpathians", "french_alps", "stubai_valley", "vinschgau")]
plotlist_external = plotlist.out[c("danube", "dovre", "sierra_nevada")]
# - Add legend
plotlist_external$legend = plot.in_legend
# - Assemble plots
plot.out = plot_grid(
  plot_grid(plotlist = plotlist_train, nrow = 1, align = "hv", scale = 0.9), 
  plot_grid(plotlist = plotlist_external, nrow = 1, align = "hv", scale = 0.9), 
  maplist.out[["stubai_valley"]], rel_heights = c(1, 1, 1.5),
  ncol = 1, scale = 0.95, labels = c("(a)", "(b)", "(c)"), label_size = 20
)
# - Save plots
ggsave("paper/figures/Fig_main_mapfeatures.pdf", plot.out, width = 38, 
       height = 33, units = "cm", bg = "white")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Figure on stats per grid [Figure S7 - Fig_supp_statmapfeatures]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Build plot of statistics per grid
plot.stat.grid = table.stat.grid %>%
  mutate(grid = as.numeric(gsub("grid", "", grid))) %>%
  pivot_longer(names_to = "metric", values_to = "value", 
               cols = all_of(c("rsquare", "pgp"))) %>%
  mutate(metric = ifelse(metric == "pgp", "PGP", "R\u00b2"), 
         metric = factor(metric, levels = c("R\u00b2", "PGP")), 
         Site = stringr::str_to_title(gsub("\\_", "\\ ", site))) %>%
  ggplot(aes(x = grid, y = value, color = Site)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ metric, scale = "free") + 
  theme_bw() + 
  ylab("") + xlab("Grid pixel size (m)")
# - Save plot
ggsave("paper/figures/Fig_supp_statmapfeatures.pdf", plot.stat.grid, width = 20, 
       height = 7, units = "cm", bg = "white")

