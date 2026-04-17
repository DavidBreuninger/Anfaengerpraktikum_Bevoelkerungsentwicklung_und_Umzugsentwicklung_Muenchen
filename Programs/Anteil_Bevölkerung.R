#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
mnew <- read.csv("Clean_Data/mnew.csv")
bnew <- read.csv("Clean_Data/bnew.csv")

#This plot shows us the population percentages of German and non-German citizens in different district from 2000 to 2024.
#Calculate the proportion of German and non-German citizens in the total population for each district and each year.
#German + non-German = 1 in each year
mnew <- mnew %>%
  group_by(Jahr, Raumbezug) %>% 
  mutate(rb5 = Basiswert.5 / Basiswert.5[Ausprägung == "insgesamt"])

p3<-mnew%>%
  filter(Ausprägung != "insgesamt" , Raumbezug != "Stadt München")%>%
  ggplot(aes(x = Jahr, y = rb5, color = Ausprägung)) +
  geom_point(size=0.6)+ geom_line() +
  facet_wrap(~Raumbezug) +
  labs(y= "Anteil an der Bevölkerung",
       title = "Entwicklung der Staatsbürgerschaft in den Bezirken",
       color = "Staatsbürgerschaft")+
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2"))+
  scale_y_continuous(limits = c(0,1))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.text.x = element_text(angle = 45, hjust = 1), #Rotate x-axis by 45°
        strip.text = element_text(size = 7,face="bold"), #bolded facet title
        panel.spacing.x = unit(1.2, "lines")) #Controlling the horizontal spacing of facet Grafik

p3

#save plot
ggsave("Results/p_Anteil_Bevölkerung.jpg", plot = p3,width = 8, height = 6)