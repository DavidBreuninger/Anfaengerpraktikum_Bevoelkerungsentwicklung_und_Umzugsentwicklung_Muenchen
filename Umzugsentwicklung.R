library(tidyverse)
library(ggplot2)
library(data.table)
library(scales)
source("functions.R")

# read data
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")
wegzug_stadt <- read.csv("Clean_Data/umzug_innen_außen.csv")
wegzug_Bezirksgruppen <-  read.csv("Clean_Data/umzug_Bezirksgruppen.csv")

# pivot longer for plots
wegzug_stadt_long <- wegzug_stadt %>%
  select(!insgesamt) %>%
  pivot_longer(cols = c("innerstaedtisch", "außerstaedtisch"),
               names_to = "Wegzug",
               values_to = "Anzahl")

wegzug_Bezirksgruppen_long <- wegzug_Bezirksgruppen %>%
  select(!insgesamt) %>%
  rename("selber Bezirk" = selber_Bezirk) %>%
  rename("nicht benachbarte Bezirke" = Restbezirke) %>%
  pivot_longer(cols = c("selber Bezirk", "Nachbarbezirke", "nicht benachbarte Bezirke", "außerstaedtisch"),
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
  theme(axis.text.x = element_text(size = 7), legend.text = element_text(size = 11), legend.title = element_text(size = 12))

# plot for slide
plot_wegzug_Bezirke <- ggplot(wegzug_Bezirksgruppen_long, 
                             aes(x = Jahr, y = Anzahl, 
                                 fill = factor(Wegzug, levels = c("außerstaedtisch", "nicht benachbarte Bezirke", "Nachbarbezirke", "selber Bezirk")))) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw() + 
  labs(fill = "Wegzug", y = "Anteil von Wegzügen") +
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "nicht benachbarte Bezirke" = "#009292", "Nachbarbezirke" = "#DB6D00", "selber Bezirk" = "#B66DFF")) +
  theme(axis.text.x = element_text(size = 7), legend.text = element_text(size = 11), legend.title = element_text(size = 12))

# save plots
ggsave("Results/plot_wegzug_Stadt.jpg", plot = plot_wegzug_Stadt, width = 12, height = 8)
ggsave("Results/plot_wegzug_Bezirke.jpg", plot = plot_wegzug_Bezirke, width = 12, height = 8)

# Entwicklung Wegzüge Münchens inner-/außerstaedtisch

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
  theme(legend.text = element_text(size = 11), legend.title = element_text(size = 12))
plot_stadt_prozent

# mutate column with percentage of people moving away from a district in Munich, 
# "inner-/außerstaedtisch" summed up for each year
Mobilitaet_allg <- Mobilitaet_muenchen_weg %>%
  group_by(Jahr, mittlere_Bevölkerung) %>%
  summarise(Gesamtwegzug = sum(Anzahl_Wegzug)) %>%
  ungroup() %>%
  mutate(Prozent = Gesamtwegzug / mittlere_Bevölkerung * 100)

# line plot with percentage of people moving away from a district in Munich
plot_muenchen_prozent <- ggplot(Mobilitaet_allg, aes(x = Jahr,  y = Prozent)) +
  geom_point(color = "black") + geom_line(color = "black") +
  labs(y = "Anteil in Prozent (Anzahl Wegzüge / mittl. Bevölkerung * 100)",) +
  theme_bw()

# save plots
ggsave("Results/plot_muenchen_prozent.jpg", plot = plot_muenchen_prozent, width = 12, height = 8)
ggsave("Results/plot_stadt_prozent.jpg", plot = plot_stadt_prozent, width = 12, height = 8)