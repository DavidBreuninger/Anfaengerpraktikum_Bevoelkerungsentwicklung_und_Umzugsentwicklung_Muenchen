## this file contains the code for the line plot on slide 16

#read data 
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

# add column "Wegzugsrate",  percentage of people moving to disrict
Zuzugsrate_gesamt <- Mobilitaet_thin %>%
  filter(Ausprägung == "insgesamt" & Raumbezug != "Stadt München") %>%
  mutate(Zuzugsrate = (Gesamtzuzug / mittlere_Hauptwohnsitzbevölkerung) * 100)

#lineplot Zuzugsrate total
g13 <- ggplot(Zuzugsrate_gesamt, aes(x = Jahr, y = Zuzugsrate)) +
  geom_line(color = "black") +
  geom_point() +
  facet_wrap(~ Raumbezug) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10),
        panel.spacing.x = unit(1.2, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        strip.background = element_rect(color = "grey", fill = "grey90")) +
  labs(y = "Zuzugsrate in Prozent")

g13

ggsave("Results/Zuzugsrate_gesamt.jpg", plot = g13,width = 10, height = 8)