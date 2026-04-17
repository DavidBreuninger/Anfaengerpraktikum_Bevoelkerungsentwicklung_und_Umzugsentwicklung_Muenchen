#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
mnew <- read.csv("Clean_Data/mnew.csv")
bnew <- read.csv("Clean_Data/bnew.csv")

#This plot shows us the changes in München average resident population from 2000 to 2024.
#choose the data only from München
plot1 <- mnew%>%
  filter(Raumbezug == "Stadt München")

p1<-ggplot(plot1, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + 
  geom_line() +labs(y = "mittlere Wohnbevölkerung in Millionen", 
                    color="Staatsbürgerschaft")+
  scale_colour_manual(values = c(deutsch = "#E69F00",
                                 nichtdeutsch = "#0072B2",
                                 insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-6))+ #Population in millions
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) #Center the title
p1

#save plot
ggsave("Results/p_Einwohnerzahl.jpg", plot = p1,width = 10, height = 6)
