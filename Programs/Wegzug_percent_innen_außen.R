# read data
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

# pivot to long for plotting
Mobilitaet_long <- Mobilitaet_thin %>%
  pivot_longer(cols = c("innerstädtisch.Weggezogene..", "außerstädtisch.Weggezogene."), 
               names_to = "Wegzug",
               values_to = "Anzahl_Wegzug") %>%
  rename("mittlere_Bevölkerung" = mittlere.Hauptwohnsitzbevölkerung.)

# mutate column with percentage of people moving away from a district in Munich, 
# with column "Wegzug" differentiating between "inner-/außerstaedtisch"
Mobilitaet_muenchen_weg <- Mobilitaet_long %>%
  filter(Ausprägung == "insgesamt" & Raumbezug == "Stadt München") %>%
  select(all_of(c("Jahr", "Wegzug", "Anzahl_Wegzug", "mittlere_Bevölkerung"))) %>%
  mutate(Wegzug = case_when(
    Wegzug == "innerstädtisch.Weggezogene.." ~ "innerstaedtisch",
    Wegzug == "außerstädtisch.Weggezogene." ~ "außerstaedtisch")) %>% 
  mutate(Prozent = Anzahl_Wegzug / mittlere_Bevölkerung * 100)

# line plot for moving away from district in Munich, split in "inner-/außerstaedtisch"
plot_stadt_prozent <- ggplot(Mobilitaet_muenchen_weg,
                             aes(x = Jahr, y = Prozent, color = Wegzug)) +
  geom_point() + geom_line() +
  labs(y = "Anteil in Prozent (Anzahl Wegzüge / mittl. Bevölkerung * 100)", color = "Wegzug") + 
  theme_bw() +
  scale_color_manual(values = c("#F0D852", "#8491B4")) +
  theme(legend.text = element_text(size = 12), title = element_text(size = 12))

#save plot
ggsave("Results/plot_stadt_prozent.jpg", plot = plot_stadt_prozent, width = 12, height = 8)