# Packages
library(sfsmisc)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot) 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Read data 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fetch results data
results_full = fread("outputs/results_swin_full.csv")
results_balanced = fread("outputs/results_swin_balanced.csv")
results_control = fread("outputs/results_swin_control.csv")
results = rbind(mutate(results_full, dataset = "full"),
                mutate(results_balanced, dataset = "balanced"), 
                mutate(results_control, dataset = "control"))

# Check
resultsorder <- data.frame(results[results$labels == "weighted avg",])
resultsorder <- resultsorder[order(resultsorder$f1, decreasing = TRUE),]
resultsorder[1:10,]

mean(resultsorder[,6])

aggregate(resultsorder[,6], list(resultsorder[,2]), mean)

aggregate(resultsorder[,6], list(resultsorder[,8]), mean)
aggregate(resultsorder[,6], list(resultsorder[,8]), max)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Figure on the random seed effect [Figure S3 - Fig_supp_seed]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Make plot
plot.seed = results %>%
  filter(labels == "weighted avg") %>%
  mutate(seed = as.character(rep)) %>%
  select(-support, -rep, -labels) %>%
  pivot_longer(names_to = "metric", values_to = "value", 
               cols = c("recall", "precision", "f1-score")) %>%
  mutate(metric = stringr::str_to_title(metric), 
         metric = factor(metric, levels = c("Precision", "Recall", "F1-Score")), 
         dataset = factor(stringr::str_to_title(dataset), levels = c(
           "Full", "Balanced", "Control"))) %>%
  ggplot(aes(x = seed, y = value, fill = dataset)) + 
  geom_boxplot(outliers = FALSE) + 
  facet_wrap( ~ metric, scales = "free", nrow = 1) +
  scale_fill_manual(values = c(`Full` = "#99D98C", `Balanced` = "#52B69A", 
                               `Control` = "#1E6091")) +
  ylab("") + xlab("Random seed") +
  theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
        panel.background = element_rect(color = 'black', fill = 'white'), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        strip.text = element_text(size = 17), 
        legend.key = element_blank(), 
        legend.text = element_text(size = 14), 
        legend.title = element_blank())  

# Save plot
ggsave("paper/figures/Fig_supp_seed.pdf", plot.seed, width = 25, 
       height = 8, units = "cm", dpi = 600, bg = "white")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Figure on val/test/external results [Figure 3 - Fig_main_results]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Classification report
classif_testexternal <- bind_rows(
  test_full = fread("outputs/test/classification_report_full.csv"),
  test_balanced = fread("outputs/test/classification_report_balanced.csv"),
  test_control = fread("outputs/test/classification_report_control.csv"),
  external_full = fread("outputs/external/classification_report_full.csv"),
  external_balanced = fread("outputs/external/classification_report_balanced.csv"),
  external_control = fread("outputs/external/classification_report_control.csv"),
  .id = "source") %>%
  separate(source, into = c("metric", "dataset"), sep = "_")

# Check
classiforder <- data.frame(classif_testexternal[classif_testexternal$labels == "weighted avg",])
classiforder <- classiforder[order(classiforder$f1, decreasing = TRUE),]
classiforder

classif_testexternal[classif_testexternal$metric == "test" & 
                     classif_testexternal$dataset == "full", ]

# 1. Identify the best run for each dataset modality based on weighted avg f1-score
best_runs <- results %>%
  filter(labels == "weighted avg") %>%
  group_by(dataset) %>%
  slice_max(`f1-score`, n = 1, with_ties = FALSE) %>%
  select(dataset, best_run_id = run_id, best_f1 = `f1-score`)

# 2. Get all performance values for these best runs across all labels and metrics
best_model_values <- results %>%
  semi_join(best_runs, by = c("dataset", "run_id" = "best_run_id")) %>%
  pivot_longer(names_to = "metric", values_to = "best_value",
               cols = c("precision", "recall", "f1-score")) %>%
  mutate(metric = stringr::str_to_title(metric), 
         metric = factor(metric, levels = c("Precision", "Recall", "F1-Score")), 
         dataset = factor(stringr::str_to_title(dataset), levels = c(
           "Full", "Balanced", "Control")), 
         labels = factor(labels, levels = c(
           "vegetation", "snow", "rock", "human", "anthropic", "samples avg", 
           "weighted avg", "macro avg", "micro avg")))

# 3. Calculate significance between dataset modalities for each label and metric
significance <- results %>%
  pivot_longer(names_to = "metric", values_to = "value",
               cols = c("precision", "recall", "f1-score")) %>%
  group_by(labels, metric) %>%
  summarise(full_balanced = t.test(value[dataset == "full"], 
                                   value[dataset == "balanced"])$p.value,
            full_control = t.test(value[dataset == "full"], 
                                   value[dataset == "control"])$p.value,
            balanced_control = t.test(value[dataset == "full"], 
                                  value[dataset == "balanced"])$p.value,
            max_value = max(value, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(names_to = "comp", values_to = "p_value",
               cols = c("full_balanced", "full_control", "balanced_control")) %>%
  mutate(comp = gsub("\\_", "\\ vs\\ \\.", comp), 
         p_adjusted = p.adjust(p_value, method = "bonferroni"), 
         metric = stringr::str_to_title(metric), 
         metric = factor(metric, levels = c("Precision", "Recall", "F1-Score")), 
         labels = factor(labels, levels = c(
           "vegetation", "snow", "rock", "human", "anthropic", "samples avg", 
           "weighted avg", "macro avg", "micro avg"))) 

# Extract pvalues in a table
# - Make table
table_signif = significance %>%
  arrange(metric, desc(labels)) %>%
  mutate(metric = as.character(metric), labels = as.character(labels)) %>%
  mutate(pvalue = scales::pvalue(p_adjusted, accuracy = 0.001, add_p = FALSE)) %>%
  select(metric, labels, comp, pvalue) %>%
  pivot_wider(names_from = comp, values_from = pvalue)
# - Print to put in latex table
for(i in 1:dim(table_signif)[1]){
  print(paste0(paste(table_signif[i, ], collapse = " & "), "\\"))
}



# Finish formatting significance  
significance = significance %>%
  group_by(labels, metric, max_value) %>%
  summarize(pmin = min(p_adjusted)) %>% 
  ungroup() %>%
  mutate(significance = ifelse(pmin < 0.05, "*", " "),
         y_position = max_value * 1.08)



# Prepare main plot data (excluding summary rows if desired)
plot_data <- results %>%
  mutate(labels = factor(labels, levels = c(
    "vegetation", "snow", "rock", "human", "anthropic", "samples avg", 
    "weighted avg", "macro avg", "micro avg"))) %>%
  pivot_longer(names_to = "metric", values_to = "value",
               cols = c("precision", "recall", "f1-score")) %>%
  mutate(metric = stringr::str_to_title(metric), 
         metric = factor(metric, levels = c("Precision", "Recall", "F1-Score")), 
         dataset = factor(stringr::str_to_title(dataset), levels = c(
           "Full", "Balanced", "Control")))

# Plot validation
plot.validation = ggplot(plot_data, aes(x = labels, y = value, fill = dataset)) + 
  geom_boxplot(outliers = FALSE, alpha = 0.7) + 
  geom_point(data = best_model_values, aes(x = labels, y = best_value),
             size = 2, shape = 23,  color = "#780000", inherit.aes = TRUE,
             position = position_dodge(width = 0.75)) +
  geom_text(data = significance,
            aes(x = labels, y = y_position, label = significance),
            size = 6, vjust = 0.9, inherit.aes = FALSE) +
  facet_wrap(~ metric, nrow = 1) +
  scale_fill_manual(values = c(`Full` = "#99D98C", `Balanced` = "#52B69A", 
                                `Control` = "#1E6091")) +
  coord_flip() + 
  ylab("") + xlab("") +
  ggtitle("(a) Validation across 50 runs") + 
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
        panel.background = element_rect(color = 'black', fill = 'white'), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        strip.text = element_text(size = 13), 
        legend.key = element_blank(), 
        legend.text = element_text(size = 14), 
        legend.title = element_blank(), 
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold"))


# Plot test with best model
plot.test = classif_testexternal %>%
  filter(metric == "test") %>% select(-metric) %>%
  pivot_longer(names_to = "metric", values_to = "value",
               cols = c("precision", "recall", "f1-score")) %>%
  mutate(metric = stringr::str_to_title(metric), 
         metric = factor(metric, levels = c("Precision", "Recall", "F1-Score")), 
         labels = factor(labels, levels = c(
           "vegetation", "snow", "rock", "human", "anthropic", "samples avg", 
           "weighted avg", "macro avg", "micro avg")), 
         dataset = factor(stringr::str_to_title(dataset), levels = c(
           "Full", "Balanced", "Control"))) %>%
  ggplot(aes(x = labels, y = value, color = dataset)) + 
  geom_point(stat="identity", position=position_dodge(0.2)) + 
  facet_wrap(~ metric, nrow = 1) +
  scale_color_manual(values = c(`Full` = "#99D98C", `Balanced` = "#52B69A", 
                                `Control` = "#1E6091")) +
  coord_flip() + 
  ylab("") + xlab("") + 
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  ggtitle("(b) Test with best model") +
  theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
        panel.background = element_rect(color = 'black', fill = 'white'), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        strip.text = element_text(size = 13), 
        legend.position = "none", 
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold"))

# Plot external
plot.external = classif_testexternal %>%
  filter(metric == "external") %>% select(-metric) %>%
  pivot_longer(names_to = "metric", values_to = "value",
               cols = c("precision", "recall", "f1-score")) %>%
  mutate(metric = stringr::str_to_title(metric), 
         metric = factor(metric, levels = c("Precision", "Recall", "F1-Score")), 
         labels = factor(labels, levels = c(
           "vegetation", "snow", "rock", "human", "anthropic", "samples avg", 
           "weighted avg", "macro avg", "micro avg")), 
         dataset = factor(stringr::str_to_title(dataset), levels = c(
           "Full", "Balanced", "Control"))) %>%
  ggplot(aes(x = labels, y = value, color = dataset)) + 
  geom_point(stat="identity", position=position_dodge(0.2)) + 
  facet_wrap(~ metric, nrow = 1) +
  scale_color_manual(values = c(`Full` = "#99D98C", `Balanced` = "#52B69A", 
                                `Control` = "#1E6091")) +
  coord_flip() + 
  ylab("") + xlab("") + 
  ggtitle("(c) External validation with best model") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
        panel.background = element_rect(color = 'black', fill = 'white'), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        strip.text = element_text(size = 13), 
        legend.position = "none", 
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold"))
  
# Compile into a single plot
plot.results = plot_grid(plot.validation, plot.test, plot.external, ncol = 1, 
                         scale = 0.95, align = "hv", rel_heights = c(1, 0.9, 0.9))

# Save plot
ggsave("paper/figures/Fig_main_results.pdf", plot.results, width = 22, 
       height = 20, units = "cm", dpi = 600, bg = "white")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Figure on site by site results [Figure S5 - Fig_supp_performance_sites]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

# Initialize output list
list_sites = vector(mode = "list", length = n)
names(list_sites) = sites

# Loop on all sites to read data
for(k in 1:n) list_sites[[k]] = fread(paste0(
  "outputs/sites/", sites[k], "/classification_report_balanced.csv"))

# Assemble in one dataset
data_sites = bind_rows(list_sites, .id = "site")

# Make plot
plot_sites = data_sites %>%
  select(-support) %>%
  pivot_longer(names_to = "metric", values_to = "value",
               cols = c("precision", "recall", "f1-score")) %>%
  mutate(metric = stringr::str_to_title(metric), 
         metric = factor(metric, levels = c("Precision", "Recall", "F1-Score")), 
         labels = factor(labels, levels = c(
           "vegetation", "snow", "rock", "human", "anthropic", "samples avg", 
           "weighted avg", "macro avg", "micro avg"))) %>%
  mutate(site_type = ifelse(site %in% sites[5:7], "External sites", 
                            "Training sites"), 
         site = stringr::str_to_title(gsub("\\_", "\\ ", site)),
         site = factor(site, levels = stringr::str_to_title(gsub("\\_", "\\ ", sites)))) %>%
  filter(value > 0) %>%
  ggplot(aes(x = labels, y = value, shape = site, color = site_type)) + 
  geom_point() + 
  scale_shape_manual(values = c(c(1:6), 8)) + 
  scale_color_manual(values = c("#C1121F", "#669BBC")) + 
  coord_flip() + 
  facet_wrap(~ metric) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
        panel.background = element_rect(color = 'black', fill = 'white'), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        strip.text = element_text(size = 15), 
        axis.text = element_text(size = 12), 
        axis.title = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 13), 
        legend.key = element_rect(fill = NA, color = NA))


# Save plot
ggsave("paper/figures/Fig_supp_performance_sites.pdf", plot_sites, 
       width = 28, height = 8, units = "cm", dpi = 600, bg = "white")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Table of support per study site [Table S2]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

support = data_sites %>% 
  filter(labels %in% c("human", "rock", "vegetation", "anthropic", "snow")) %>% 
  select(site, labels, support) %>% 
  pivot_wider(names_from = "site", values_from = "support") %>%
  as.data.frame()

tex=paste0(support[,1], " & ",
           support[,2], " & ",
           support[,3], " & ",
           support[,4], " & ",
           support[,5], " & ",
           support[,6], " & ",
           support[,7], " & ",
           support[,8], " \\")
print(data.frame(tex), row.names = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$
# Relation between support and precision [Figure S6 - Fig_supp_support]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$

# Make plot
plot_support = data_sites %>%
  filter(labels %in% c("human", "rock", "vegetation", "anthropic", "snow")) %>% 
  ggplot(aes(x = support, y = precision)) + 
  geom_point(fill = "lightblue", color = "black", shape = 21) + 
  xlab("Support") + ylab("Precision") +
  theme_bw()

# Save plot
ggsave("paper/figures/Fig_supp_support.pdf", plot_support, 
       width = 14, height = 8, units = "cm", dpi = 600, bg = "white")
