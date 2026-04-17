#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
mnew <- read.csv("Clean_Data/mnew.csv")
bnew <- read.csv("Clean_Data/bnew.csv")

#This plot shows us the trend of  population outflow into München from 2000 to 2024.
#outflow = internal migration out + external migration out
p6<-mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "in Tausend", 
       title = "Wegzug" ,
       color = "Staatsbürgerschaft") +
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2",
                                insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-3)) + #Population in thousands
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5), #Center the title
                    legend.position = "none",     #Remove Legend
                    strip.text = element_blank()) #Remove facet title
p6

#save plot
ggsave("Results/p_Wegzug.jpg", plot = p6,width = 6, height = 3)