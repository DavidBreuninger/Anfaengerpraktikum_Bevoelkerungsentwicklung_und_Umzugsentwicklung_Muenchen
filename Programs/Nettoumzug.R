##This file contains the code for the line plot on slide 13.
#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")

Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

#This plot shows us the trend of net population inflow into München from 2000 to 2024.
#Calculate net population inflow(ar) ,
#Netto = (external migration in + internal migration in) - (external migration out + internal migration out) 
mnew <- Mobilitaet_thin %>%
  mutate(Netto = Gesamtzuzug - Gesamtwegzug)

p4 <- mnew %>%
  filter(Raumbezug == "Stadt München") %>%
  ggplot( aes(x = Jahr, y = Netto, color = Ausprägung)) + geom_point()+ 
  geom_line() + labs(y = "in Tausend",
                     title = "Nettoumzug (Zuzug – Wegzug) in München",
                     color = "Staatsbürgerschaft")+
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2",
                                insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-3))+ #Population in thousands
  theme_bw()+theme(plot.title = element_text(hjust = 0.5)) #Center the title

p4

#save plot
ggsave("Results/p_Nettoumzug.jpg", plot = p4,width = 6, height = 4)
