# read data
wegzug_stadt <- read.csv("Clean_Data/umzug_innen_außen.csv")

# pivot longer for plots
wegzug_stadt_long <- wegzug_stadt %>%
  select(!insgesamt) %>%
  pivot_longer(cols = c("innerstaedtisch", "außerstaedtisch"),
               names_to = "Wegzug",
               values_to = "Anzahl")

# percent stacked barplot for "Wegzüge in Stadtbezirken" 
# plot for slide 
plot_wegzug_Stadt <- ggplot(wegzug_stadt_long, aes(x = Jahr, y = Anzahl, fill = Wegzug)) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw() + 
  labs(fill = "Wegzug", y = "Anteil von Wegzügen")  +
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "innerstaedtisch" = "#8491B4")) + 
  theme(axis.text.x = element_text(size = 7), legend.text = element_text(size = 12), title = element_text(size = 12))

# save plots
ggsave("Results/wegzug_Anteil_innen_außen.jpg", plot = plot_wegzug_Stadt, width = 12, height = 8)