#Prepare for Data
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")

Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")
mnew <- Mobilitaet 
bnew <- Bevoelkerungsdichte

 #Add a new column called sn to store the district number
 #Give Stadt München number 26
bnew <- bnew%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))

mnew <- mnew%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))

 #remove the number in Raumbezug
bnew <-bnew%>% 
  mutate(Raumbezug = case_when(
    sn == 1 ~ "Altstadt",
    sn == 2 ~ "Ludwigsvorstadt",
    sn == 3 ~ "Maxvorstadt",
    sn == 4 ~ "Schwabing-West",
    sn == 5 ~ "Haidhausen",
    sn == 6 ~ "Sendling",
    sn == 7 ~ "Sendling-Westpark",
    sn == 8 ~ "Schwanthalerhöhe",
    sn == 9 ~ "Neuhausen",
    sn == 10 ~ "Moosach",
    sn == 11 ~ "Milbertshofen",
    sn == 12 ~ "Schwabing",
    sn == 13 ~ "Bogenhausen",
    sn == 14 ~ "Berg am Laim",
    sn == 15 ~ "Trudering",
    sn == 16 ~ "Ramersdorf",
    sn == 17 ~ "Obergiesing",
    sn == 18 ~ "Untergiesing",
    sn == 19 ~ "Thalkirchen",
    sn == 20 ~ "Hadern",
    sn == 21 ~ "Pasing",
    sn == 22 ~ "Aubing",
    sn == 23 ~ "Allach",
    sn == 24 ~ "Feldmoching",
    sn == 25 ~ "Laim",
    TRUE ~ Raumbezug))

mnew <- mnew%>%
  mutate(Raumbezug = case_when(
    sn == 1 ~ "Altstadt",
    sn == 2 ~ "Ludwigsvorstadt",
    sn == 3 ~ "Maxvorstadt",
    sn == 4 ~ "Schwabing-West",
    sn == 5 ~ "Haidhausen",
    sn == 6 ~ "Sendling",
    sn == 7 ~ "Sendling-Westpark",
    sn == 8 ~ "Schwanthalerhöhe",
    sn == 9 ~ "Neuhausen",
    sn == 10 ~ "Moosach",
    sn == 11 ~ "Milbertshofen",
    sn == 12 ~ "Schwabing",
    sn == 13 ~ "Bogenhausen",
    sn == 14 ~ "Berg am Laim",
    sn == 15 ~ "Trudering",
    sn == 16 ~ "Ramersdorf",
    sn == 17 ~ "Obergiesing",
    sn == 18 ~ "Untergiesing",                          
    sn == 19 ~ "Thalkirchen",
    sn == 20 ~ "Hadern",
    sn == 21 ~ "Pasing",
    sn == 22 ~ "Aubing",
    sn == 23 ~ "Allach",
    sn == 24 ~ "Feldmoching",
    sn == 25 ~ "Laim",
    TRUE ~ Raumbezug))

#creat plots

#This plot shows us the changes in München average resident population from 2000 to 2024.
 #choose the data only from München
plot1 <- mnew%>%
  filter(Raumbezug == "Stadt München")

p1<-ggplot(plot1, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + 
  geom_line() +labs(y = "mittlere Wohnbevölkerung in Millionen", 
                    title = "Einwohnerzahl",
                    color="Staatsbürgerschaft")+
  scale_colour_manual(values = c(deutsch = "#E69F00",
                                 nichtdeutsch = "#0072B2",
                                 insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-6))+ #Population in millions
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) #Center the title
p1

 #save plot
ggsave("Results/p_Einwohnerzahl.jpg", plot = p1,width = 10, height = 6)


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


#This plot shows us the population growth rate of München compared to 2000.
 #Calculate the index : Current value / 2000 value × 100
mnew_index2 <- mnew %>%
  group_by(Raumbezug,Ausprägung) %>%
  mutate(indexb1 = 100 * Basiswert.5 / Basiswert.5[Jahr == 2000])

p2b <-mnew_index2 %>% filter(Raumbezug == "Stadt München", Ausprägung == "insgesamt") %>% 
  ggplot(aes(x = Jahr, y = indexb1)) +
  geom_point(color = "black",size=1) +
  geom_line(color = "black")+  
  labs(y = "Bevölkerungsindex (2000=100)",
       title = "Prozentuale Bevölkerungsentwicklung in der Stadt München")+theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.text.x = element_text(angle = 45, hjust = 1)) #Rotate x-axis by 45°

p2b

 #save plot
ggsave("Results/p_Prozentuale_Bevölkerungsentwicklung_München.jpg", plot = p2b,width = 10, height = 6)


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


#This plot shows us the population percentages of German and non-German citizens in München  from 2000 to 2024.
p3b<-mnew%>%
  filter(Ausprägung != "insgesamt" , sn == 26)%>%
  ggplot(aes(x = Jahr, y = rb5, color = Ausprägung)) +
  geom_point(size=0.6)+ geom_line() +
  labs(y= "Anteil an der Bevölkerung",
       title = "Entwicklung der Staatsbürgerschaft in der Stadt München",
       color = "Staatsbürgerschaft")+
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2"))+
  scale_y_continuous(limits = c(0,1))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.text.x = element_text(angle = 45, hjust = 1), #Rotate x-axis by 45°
        strip.text = element_text(size = 7,face="bold"), #bolded facet title
        panel.spacing.x = unit(1.2, "lines")) #Controlling the horizontal spacing of facet Grafik

p3b

 #save plot
ggsave("Results/p_Anteil_Bevölkerung_München.jpg", plot = p3b,width = 8, height = 6)


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
ggsave("Results/p_Nettoumzug.jpg", plot = p5,width = 6, height = 4)


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
  theme_bw()+theme(plot.title = element_text(hjust = 0.5), #Center the title
                   legend.position = "none",     #Remove Legend
                   strip.text = element_blank()) #Remove facet title

p5

 #save plot
ggsave("Results/p_Zuzug.jpg", plot = p5,width = 6, height = 3)


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


