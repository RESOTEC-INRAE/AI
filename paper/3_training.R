library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

# Fetch training data
training_full = fread("outputs/best_model_curves_full.csv")
training_balanced = fread("outputs/best_model_curves_balanced.csv")
training_control = fread("outputs/best_model_curves_control.csv")
training = rbind(mutate(training_full, dataset = "full"),
                 mutate(training_balanced, dataset = "balanced"), 
                 mutate(training_control, dataset = "control"))

# Format training data for plotting
training_formatted = training %>%
  pivot_longer(names_to = "metrics", values_to = "value", 
               cols = all_of(c("train_loss", "val_loss", 
                               "train_f1", "val_f1"))) %>%
  separate(col = "metrics", into = c("trainval", "metric"), sep = "_")

# Make the plot [Figure S4 - Fig_supp_training]
plot_training = training_formatted %>%
  mutate(trainval = ifelse(trainval == "train", "Training", "Validation"), 
         metric = ifelse(metric == "f1", "F1-score", "Loss"),
         dataset = stringr::str_to_title(dataset), 
         dataset = factor(dataset, levels = c(
           "Full", "Balanced", "Control"))) %>%
  ggplot(aes(x = epoch, y = value, color = trainval, shape = trainval)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(metric ~ dataset, scales = "free") + 
  scale_color_manual(values = c("#2F74B1", "#FD842F")) +
  ylab("") + xlab("Epoch") +
  theme(panel.grid = element_line(colour = "grey", linetype = "dotted"), 
        panel.background = element_rect(color = 'black', fill = 'white'), 
        strip.background = element_rect(color = 'black', fill = 'white'),
        strip.text = element_text(size = 17), 
        legend.key = element_blank(), 
        legend.text = element_text(size = 14), 
        legend.title = element_blank()) 

# Save plot
ggsave("paper/figures/Fig_supp_training.pdf", plot_training, width = 20, 
       height = 12, units = "cm", dpi = 600, bg = "white")
