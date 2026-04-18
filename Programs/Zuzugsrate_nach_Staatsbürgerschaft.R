## this file contains the code for the line plot on slide 17

#read data 
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

# adds column "Zuzugsrate", percentage of people moving to districg, split into "deutsch"/"nichtdeutsch"
Zuzugsrate_split <- Mobilitaet_thin %>% 
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = mittlere_Hauptwohnsitzbevölkerung[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung %in% c("deutsch", "nichtdeutsch")) %>%
  mutate(Zuzugsrate = (Gesamtzuzug / gesamt_bev) * 100) %>%
  filter(Raumbezug != "Stadt München")

#lineplot Zuzugsrate by citizenship
g14 <- ggplot(Zuzugsrate_split, aes(x = Jahr, y = Zuzugsrate, color = Ausprägung)) +
  geom_line() +
  geom_point(size = 0.9) +
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
  labs(y = "Zuzugsrate",
       color = "Ausprägung")

g14
#save plot
ggsave("Results/Zuzugsrate_Staatsbürgerschaft.jpg", plot = g14, width = 10, height = 8)
