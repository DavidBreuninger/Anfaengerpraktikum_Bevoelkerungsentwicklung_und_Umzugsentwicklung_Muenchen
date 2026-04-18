## this file contains the code for the line plot on slide 21

# read data
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")
str(Mobilitaet_thin)
# transform data set to contain the number of people moving away in Munich for each year
Wegzug_gesamt <- Mobilitaet_thin %>%
  filter(Ausprägung == "insgesamt" & Raumbezug == "Stadt München") %>%
  select(all_of(c("Jahr", "Gesamtwegzug", "mittlere_Hauptwohnsitzbevölkerung"))) %>%
  mutate(Prozent = Gesamtwegzug / mittlere_Hauptwohnsitzbevölkerung * 100)

# line plot with percentage of people moving away from a district in Munich
plot_muenchen_prozent <- ggplot(Wegzug_gesamt, aes(x = Jahr,  y = Prozent)) +
  geom_point(color = "black") + geom_line(color = "black") +
  labs(y = "Anteil in Prozent (Anzahl Wegzüge / mittl. Bevölkerung * 100)",) +
  theme_bw() + 
  theme(title = element_text(size = 12))

# save plots
ggsave("Results/wegzug_prozent_gesamt.jpg", plot = plot_muenchen_prozent, width = 12, height = 8)