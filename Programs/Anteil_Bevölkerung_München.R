##This file contains the code for the lineplot on slide 8.
#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
mnew <- read.csv("Clean_Data/mnew.csv")

#This plot shows us the population percentages of German and non-German citizens in München  from 2000 to 2024.
#Calculate the proportion of German and non-German citizens in the total population for each district and each year.
#German + non-German = 1 in each year
mnew <- mnew %>%
  group_by(Jahr, Raumbezug) %>% 
  mutate(rb5 = Basiswert.5 / Basiswert.5[Ausprägung == "insgesamt"])

p3b<-mnew%>%
  filter(Ausprägung != "insgesamt" , sn == 26)%>%
  ggplot(aes(x = Jahr, y = rb5, color = Ausprägung)) +
  geom_point(size=0.6)+ geom_line() +
  labs(y= "Anteil an der Bevölkerung",
       color = "Staatsbürgerschaft")+
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2"))+
  scale_y_continuous(limits = c(0,1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), #Rotate x-axis by 45°
        strip.text = element_text(size = 7,face="bold"), #bolded facet title
        panel.spacing.x = unit(1.2, "lines")) #Controlling the horizontal spacing of facet Grafik

p3b

#save plot
ggsave("Results/p_Anteil_Bevölkerung_München.jpg", plot = p3b,width = 10, height = 6)
