##This file contains the code for the line plot on slide 10.

#read data
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

#This plot shows us the population growth rate of München compared to 2000.
#Calculate the index : Current value / 2000 value × 100
mnew_index2 <- Mobilitaet_thin %>%
  group_by(Raumbezug, Ausprägung) %>%
  mutate(indexb1 = 100 * mittlere_Hauptwohnsitzbevölkerung / mittlere_Hauptwohnsitzbevölkerung[Jahr == 2000]) %>%
  ungroup()

p2b <- mnew_index2 %>% filter(Raumbezug == "Stadt München", Ausprägung == "insgesamt") %>% 
  ggplot(aes(x = Jahr, y = indexb1)) +
  geom_point(color = "black",size=1) +
  geom_line(color = "black")+  
  labs(y = "Bevölkerungsindex (2000=100)")+theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Rotate x-axis by 45°

p2b

#save plot
ggsave("Results/p_Prozentuale_Bevölkerungsentwicklung_München.jpg", plot = p2b,width = 10, height = 6)
