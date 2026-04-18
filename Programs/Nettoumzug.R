##This file contains the code for the lineplot on slide 13.
#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
mnew <- read.csv("Clean_Data/mnew.csv")

#This plot shows us the trend of net population inflow into München from 2000 to 2024.
#Calculate net population inflow(ar) ,
#ar = external migration in - external migration out + internal migration in - internal migration out
mnew <- mnew%>%
  mutate(ar = Basiswert.1 - Basiswert.3 + Basiswert.2 -Basiswert.4)

p4 <- mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ 
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
