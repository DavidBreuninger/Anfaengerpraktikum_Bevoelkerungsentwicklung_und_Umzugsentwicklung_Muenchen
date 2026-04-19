##This file contains the code for the line plot on slide 7.
#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

#This plot shows us the changes in München average resident population from 2000 to 2024.
#choose the data only from München
Mobil_muenchen <- Mobilitaet_thin %>%
  filter(Raumbezug == "Stadt München")

p1 <- ggplot(Mobil_muenchen, aes(x = Jahr, y = mittlere_Hauptwohnsitzbevölkerung, color = Ausprägung)) + geom_point() + 
  geom_line() +labs(y = "mittlere Wohnbevölkerung in Millionen", 
                    color="Staatsbürgerschaft")+
  scale_colour_manual(values = c(deutsch = "#E69F00",
                                 nichtdeutsch = "#0072B2",
                                 insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-6))+ #Population in millions
  theme_bw() 
p1

#save plot
ggsave("Results/p_mittlerer_Wohnbevölkerung.jpg", plot = p1,width = 10, height = 6)
