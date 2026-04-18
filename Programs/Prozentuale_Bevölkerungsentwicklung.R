##This file contains the code for the lineplot on slide 11.
#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
mnew <- read.csv("Clean_Data/mnew.csv")

#This plot shows us the population growth rate of each district compared to 2000.
#Make the population into index (2000=100) and then sort the districts by the latest year.
#Calculate the index : Current value / 2000 value × 100
mnew_index <- mnew %>%
  group_by(Raumbezug,Ausprägung) %>%
  mutate(indexb1 = 100 * Basiswert.5 / Basiswert.5[Jahr == 2000])

#Sort by latest year
order1<- mnew_index%>% 
  filter(Jahr == max(Jahr),Raumbezug != "Stadt München",Ausprägung == "insgesamt") %>% 
  arrange(desc(indexb1))

#Redefining the order of zones,those with faster growth are listed first.
mnew_index$Raumbezug <- factor(mnew_index$Raumbezug , levels = order1$Raumbezug )

p2 <- mnew_index %>% filter(Raumbezug != "Stadt München",Ausprägung == "insgesamt") %>% 
  ggplot(aes(x = Jahr, y = indexb1)) +
  geom_point(color = "black",size=1) +
  geom_line(color = "black")+ facet_wrap(~ Raumbezug) + 
  labs(y = "Bevölkerungsindex (2000=100)",
       title = "Prozentuale Bevölkerungsentwicklung nach Stadtbezirken")+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.text.x = element_text(angle = 45, hjust = 1), #Rotate x-axis by 45°
        strip.text = element_text(size = 7,face="bold"), #bolded facet title
        panel.spacing.x = unit(1.2, "lines")) #Controlling the horizontal spacing of facet Grafik
p2

#save plot
ggsave("Results/p_Prozentuale_Bevölkerungsentwicklung.jpg", plot = p2,width = 10, height = 6)
