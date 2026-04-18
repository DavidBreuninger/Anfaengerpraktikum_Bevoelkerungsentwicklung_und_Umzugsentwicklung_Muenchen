## this file contains the code for the line plot on slide 19

#read data 
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

# adds column "Wegzugsrate", percentage of people moving away, split into "deutsch"/"nichtdeutsch"
Wegzugsrate_split <- Mobilitaet_thin %>% 
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = mittlere_Hauptwohnsitzbevölkerung[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung %in% c("deutsch", "nichtdeutsch")) %>%
  mutate(Wegzugsrate = (Gesamtwegzug / gesamt_bev) * 100) %>%
  filter(Raumbezug != "Stadt München")

#lineplot Wegzugsrate by citizenship
g15 <- ggplot(Wegzugsrate_split, aes(x = Jahr, y = Wegzugsrate, color = Ausprägung)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Raumbezug) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10),
        panel.spacing.x = unit(1.2, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        strip.background = element_rect(color = "grey", fill = "grey90"),
        legend.text = element_text(size = 11)) +
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2")) +
  labs(y = "Wegzugsrate",
       color = "Ausprägung")

g15
# save plot
ggsave("Results/Wegzugsrate_staatbürgerschaft.jpg", plot = g15, width = 12, height = 6)
