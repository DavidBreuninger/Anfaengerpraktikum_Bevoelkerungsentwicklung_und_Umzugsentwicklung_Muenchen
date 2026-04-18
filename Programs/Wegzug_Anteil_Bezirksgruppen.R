## this file contains the code for the stacked percentage barplot on slide 24

library(tidyverse)
library(ggplot2)
library(data.table)
library(scales)

# read data
wegzug_Bezirksgruppen <-  read.csv("Clean_Data/umzug_Bezirksgruppen.csv")

# pivot longer for plots
wegzug_Bezirksgruppen_long <- wegzug_Bezirksgruppen %>%
  select(!insgesamt) %>%
  rename("selber Bezirk" = selber_Bezirk) %>%
  rename("nicht benachbarte Bezirke" = Restbezirke) %>%
  pivot_longer(cols = c("selber Bezirk", "Nachbarbezirke", "nicht benachbarte Bezirke", "außerstaedtisch"),
               names_to = "Wegzug",
               values_to = "Anzahl")

# percent stacked barplot for "Wegzüge in Stadtbezirken" 
# plot for slide
plot_wegzug_Bezirke <- ggplot(wegzug_Bezirksgruppen_long, 
                             aes(x = Jahr, y = Anzahl, 
                                 fill = factor(Wegzug, levels = c("außerstaedtisch", "nicht benachbarte Bezirke", "Nachbarbezirke", "selber Bezirk")))) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw() + 
  labs(fill = "Wegzug", y = "Anteil von Wegzügen") +
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "nicht benachbarte Bezirke" = "#009292", "Nachbarbezirke" = "#DB6D00", "selber Bezirk" = "#B66DFF")) +
  theme(axis.text.x = element_text(size = 7), legend.text = element_text(size = 12), title = element_text(size = 12))

# save plots
ggsave("Results/Wegzug_Anteil_ Bezirke_gruppiert.jpg", plot = plot_wegzug_Bezirke, width = 12, height = 8)