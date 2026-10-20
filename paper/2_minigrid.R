library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# Fetch minigrid experiment
minigrid_full = fread("outputs/results_minigrid_full.csv")
minigrid_balanced = fread("outputs/results_minigrid_balanced.csv")
minigrid_control = fread("outputs/results_minigrid_control.csv")
minigrid = rbind(mutate(minigrid_full, dataset = "full"),
                 mutate(minigrid_balanced, dataset = "balanced"),
                 mutate(minigrid_control, dataset = "control"))

# Check order
minigridorder <- data.frame(minigrid[minigrid$labels == "weighted avg",])
minigridorder <- minigridorder[order(minigridorder$f1, decreasing = TRUE),]
minigridorder[1:10,]

aggregate(minigridorder[,8], list(minigridorder[,2]), mean)

# Format minigrid raw data
minigrid_formatted = minigrid %>%
  filter(labels == "weighted avg") %>%
  select(-support, -labels, -run_id) %>%
  mutate(batch_size = paste0("BS = ", batch_size), 
         learning_rate = paste0("LR = ", learning_rate), 
         lr_bs = paste0(batch_size, "\n", learning_rate), 
         backbone = case_when(
           backbone == "hf_swt_t" ~ "Swin Transformer", 
           backbone == "hf_resnet" ~ "ResNet", 
           backbone == "hf_cnx2_t" ~ "ConvNeXt", 
           backbone == "hf_vit_g16" ~ "Vision Transformer"
         )) %>%
  pivot_longer(names_to = "metric", values_to = "value", 
               cols = c("precision", "recall", "f1-score")) %>%
  group_by(backbone, batch_size, learning_rate, lr_bs, dataset, metric) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE), 
            n = n()) %>%
  mutate(lwr = mean - sd, upr = mean + sd)

# Make the plot [Figure 2 - Fig_main_minigrid]
plot_minigrid = minigrid_formatted %>%
  # Fix for configuration with no convergence
  rbind(minigrid_formatted %>% filter(backbone == "Vision Transformer" & 
                                        lr_bs == "BS = 16\nLR = 1e-04" &
                                        dataset == "full") %>% 
          mutate(lr_bs = "BS = 16\nLR = 1e-05",
                 mean = NA, sd = NA, lwr = NA, upr = NA)) %>%
  mutate(metric = factor(metric, levels = c("recall", "precision", "f1-score"))) %>% 
  ggplot(aes(x = backbone, y = mean, fill = dataset))+
  geom_bar(stat="identity", color= "lightgray", position=position_dodge())+ 
  geom_errorbar(aes(ymin = lwr, ymax = upr), 
                position = position_dodge(.9), width = 0.2) + 
  facet_grid(lr_bs ~ metric) + 
  geom_hline(data = merge(minigrid_formatted %>%
                            group_by(metric, dataset) %>%
                            summarize(maxi = max(mean)) %>%
                            mutate(metric = factor(metric, levels = c(
                              "recall", "precision", "f1-score"))), 
                          data.frame(lr_bs = unique(minigrid_formatted$lr_bs))), 
             aes(yintercept = maxi, color = dataset), 
             linetype = "dashed") + 
  coord_flip() + 
  scale_fill_brewer(palette="Paired") + 
  scale_color_brewer(palette="Paired") + 
  xlab("") + ylab("") +
  theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
        panel.background = element_rect(color = 'black', fill = 'white'), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        strip.text.x = element_text(size = 17), 
        strip.text.y = element_text(size = 10), 
        legend.key = element_blank(), 
        legend.text = element_text(size = 14), 
        legend.title = element_blank()) 

# Save plot
ggsave("paper/figures/Fig_main_minigrid.pdf", plot_minigrid, width = 20, 
       height = 12, units = "cm", dpi = 600, bg = "white")


