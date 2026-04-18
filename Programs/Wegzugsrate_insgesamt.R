## this file contains the code for the line plot on slide 18

#read data 
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

# add column "Wegzugsrate", percentage of people moving away from district
Wegzugsrate_gesamt <- Mobilitaet_thin %>%
  filter(Ausprägung == "insgesamt" & Raumbezug != "Stadt München") %>%
  mutate(Wegzugsrate = (Gesamtwegzug / mittlere_Hauptwohnsitzbevölkerung) * 100)

#lineplot Wegzugsrate total
g12 <- ggplot(Wegzugsrate_gesamt, aes(x = Jahr, y = Wegzugsrate)) +
  geom_line(color = "black") +
  geom_point() +
  facet_wrap(~ Raumbezug) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10),
        panel.spacing.x = unit(1.2, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        strip.background = element_rect(color = "grey", fill = "grey90")) +
  labs(y = "Wegzugsrate in Prozent")

g12

ggsave("Results/Wegzugsrate_insgesamt.jpg", plot = g12, width = 12, height = 6)