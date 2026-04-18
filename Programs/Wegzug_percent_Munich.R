# read data
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

# transform data set to contain the number of people moving away in Munich for each year
Wegzug_gesamt <- Mobilitaet_thin %>%
  filter(Ausprägung == "insgesamt" & Raumbezug == "Stadt München") %>%
  mutate(Gesamtwegzug = innerstädtisch.Weggezogene.. + außerstädtisch.Weggezogene.) %>%
  rename("mittlere_Bevölkerung" = mittlere.Hauptwohnsitzbevölkerung.)  %>%
  select(all_of(c("Jahr", "Gesamtwegzug", "mittlere_Bevölkerung"))) %>%
  mutate(Prozent = Gesamtwegzug / mittlere_Bevölkerung * 100)

# line plot with percentage of people moving away from a district in Munich
plot_muenchen_prozent <- ggplot(Wegzug_gesamt, aes(x = Jahr,  y = Prozent)) +
  geom_point(color = "black") + geom_line(color = "black") +
  labs(y = "Anteil in Prozent (Anzahl Wegzüge / mittl. Bevölkerung * 100)",) +
  theme_bw() + 
  theme(title = element_text(size = 12))

# save plots
ggsave("Results/plot_muenchen_prozent.jpg", plot = plot_muenchen_prozent, width = 12, height = 8)