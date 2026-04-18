#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
mnew <- read.csv("Clean_Data/mnew.csv")
bnew <- read.csv("Clean_Data/bnew.csv")

#This plot shows us the trend of  population inflow into München from 2000 to 2024.
#inflow = internal migration in + external migration in
p5<- mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "in Tausend", 
       title = "Zuzug",
       color = "Staatsbürgerschaft") +
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2",
                                insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-3)) + #Population in thousands
  scale_x_continuous(expand = expansion(mult = c(0.05,0.06)))+ #right of x axis widens by 6% , left widens by 5%
  theme_bw()+theme(plot.title = element_text(hjust = 0.5), #Center the title
                   legend.position = "none",     #Remove Legend
                   strip.text = element_blank()) #Remove facet title

p5

#save plot
ggsave("Results/p_Zuzug.jpg", plot = p5,width = 6, height = 3)
