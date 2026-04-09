


library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")

library("scales")

Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")
m <-Mobilitaet #nache einlesung
b <-Bevoelkerungsdichte #nache einlesung

bnew<-b
mnew<-m
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


bnew <-bnew%>% #damit auf plot nicht abgeschnitten wird
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






#Einwohnerzahl nach Staatsbürgerschaft
mplot1 <- mnew%>%
  filter(sn == 26)
p2<-ggplot(mplot1, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + 
  geom_line() +labs(y = "mittlere Wohnbevölkerung in Millionen",
                    title = "Einwohnerzahl",
                    color="Staatsbürgerschaft")+
  scale_colour_manual(values = c(deutsch = "#E69F00",
                                 nichtdeutsch = "#0072B2",
                                 insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-6)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis_d()
p2


ggsave("Results/p2.jpg", plot = p2,width = 10, height = 6)+
  scale_color_viridis_d()

ggsave("Results/p_mittlere_Wohnbevölkerung.jpg", plot = p2,width = 10, height = 6)



#Prozentuale Bevölkerungsentwicklung nach Stadtbezirke

mnew <- mnew %>%
  group_by(Raumbezug,Ausprägung) %>%
  mutate(indexb1 = 100 * Basiswert.5 / Basiswert.5[Jahr == 2000]
  )

orderb <- mnew %>% 
  filter(Jahr == max(Jahr),Raumbezug != "Stadt München", Ausprägung == "insgesamt") %>% 
  arrange(desc(indexb1))
  
mnew$Raumbezug <- factor(mnew$Raumbezug, 
                         levels = unique(orderb$Raumbezug))

p3 <-mnew %>% filter(Raumbezug != "Stadt München" & Ausprägung == "insgesamt") %>% 
  ggplot(aes(x = Jahr, y = indexb1)) +

  geom_point(color = "blue",size=1) +
  geom_line(color = "blue")+ facet_wrap(~ Raumbezug) + 
  labs(y = "Index (2000 = 100)",
       title = "Potezentuale Bevölkerungsentwicklung nach Stadbezirken")+

  geom_point(color = "#0072B2",size=1) +
  geom_line(color = "#0072B2")+ facet_wrap(~ Raumbezug) + 
  labs(y = "Bevölkerungsindex (2002=100)")+

  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))
p3

ggsave("Results/p_Prozentuale_Bevölkerungsentwicklung.jpg", plot = p3,width = 10, height = 6)


#Prozentuale Bevölkerungsentwicklung in der Stadt München

p3b <-mnew %>% 
  filter(sn == 26, Ausprägung == "insgesamt") %>% 
  ggplot(aes(x = Jahr, y = indexb1)) +

  geom_point(color = "blue") +
  geom_line(color = "blue")+  
  labs(y = "Index (2000 = 100)",
       title = "Potezentuale Bevölkerungsentwicklung in der Stadt München")+

  geom_point(color = "#0072B2",size=1) +
  geom_line(color = "#0072B2")+  
  labs(y = "Bevölkerungsindex (2002=100)",
       title = "Prozentuale Bevölkerungsentwicklung in der Stadt München")+

  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))

p3b

ggsave("Results/p_Prozentuale_Bevölkerungsentwicklung.jpg", plot = p3b,width = 10, height = 6)





#Entwicklung in der Stadt München
  
  #ar
mnew <- mnew%>%
  mutate(ar = Basiswert.1 - Basiswert.3 + Basiswert.2 -Basiswert.4)

p5 <- mnew%>%
  filter(sn == 26)%>%
 ggplot( aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ 

  geom_line() + labs(y = "in Tausend",
                     title = "Nettoumzug (Zuzug – Wegzug) in München",
                     color = "Staatsbürgerschaft") + 

  geom_line() + labs(y = "Nettoumzug (Zuzug – Wegzug) in Tausend",
                     title = "Nettoumzug",
                     color = "Staatsbürgerschaft")+
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2",
                                insgesamt = "black"))+

  scale_y_continuous(labels = label_number(scale = 1e-3)) +theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis_d()

p5
ggsave("Results/p_Nettoumzug.jpg", plot = p5,width = 6, height = 4)



#Entwicklung in der Stadt München
p7<- mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +

  labs(y = "in Tausend",
       title = "Zuzug",
       color = "Staatsbürgerschaft") +
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2",
                                insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-3)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_blank()) +
  scale_color_viridis_d() + theme(legend.position = "none")
  
p7

ggsave("Results/p_Zuzug.jpg", plot = p7,width = 5, height = 3)


#Entwicklung in der Stadt München
p8<-mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "in Tausend", 

       title = "Wegzug" ,
       color = "Staatsbürgerschaft") +
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2",
                                insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5),
                    strip.text = element_blank())+
  scale_color_viridis_d() + theme(legend.position = "none")
p8

ggsave("Results/p_Wegzug.jpg", plot = p8,width = 5, height = 3)




mnew <- mnew %>%
  group_by(Jahr, Raumbezug) %>% 
  mutate(rb5 = Basiswert.5 / Basiswert.5[Ausprägung == "insgesamt"])



ggsave("Results/p12.jpg", plot = p12,width = 10, height = 6)

#Staatsbürgerschaft
p12<-mnew%>%
  filter(Ausprägung != "insgesamt" , Raumbezug != "Stadt München")%>%
ggplot(aes(x = Jahr, y = rb5, color = Ausprägung)) +
  geom_point(size=0.6)+ geom_line() +
  facet_wrap(~Raumbezug) +
  labs(y= "Anteil an der Bevölkerung", 
       color = "Staatsbürgerschaft")+
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2"))+
  scale_y_continuous(limits = c(0,1))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))
  
p12

ggsave("Results/p_Anteil_Bevölkerung.jpg", plot = p12,width = 8, height = 6)

pnew<-mnew%>%
  filter(Ausprägung != "insgesamt" , sn == 26)%>%
  ggplot(aes(x = Jahr, y = rb5, color = Ausprägung)) +
  geom_point(size=0.6)+ geom_line() +
  labs(y= "Anteil an der Bevölkerung", 
       color = "Staatsbürgerschaft")+
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2"))+
  scale_y_continuous(limits = c(0,1))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines")) +
  scale_color_viridis_d()

pnew

ggsave("Results/pnew.jpg", plot = pnew,width = 10, height = 6)

ggsave("Results/p_Anteil.jpg", plot = pnew,width = 8, height = 6)

mnew <- mnew%>% 
  group_by(Raumbezug, Ausprägung, Jahr)%>%
  mutate(au = 100 *(Basiswert.3 + Basiswert.1)/ Basiswert.5,
         inn = 100 *(Basiswert.2 ) / Basiswert.5)


p13 <- mnew%>%
  filter(sn == 26)%>%
  ggplot(aes(x = Jahr, y = inn + au, color = Ausprägung)) +
  geom_point() + geom_line() + 
  labs(title="Umzüge in der Stadt München",
       y = "Anteil in Protzent",
       color = "Staatsbürgerschaft") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

p13

ggsave("Results/p13.jpg", plot = p13,width = 10, height = 6)  


p14 <- mnew%>%
  filter(sn == 26)%>%
  ggplot(aes(x = Jahr, y = au, color = Ausprägung)) + 
  geom_point() + geom_line() +
  labs(title="Außerstädtische Umzüge in der Stadt München",
       y = "Anteil in Protzent",
       color = "Staatsbürgerschaft") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

p14 

ggsave("Results/p14.jpg", plot = p14,width = 10, height = 6)


p15 <- mnew%>%
  filter(sn == 26)%>%
  ggplot(aes(x = Jahr, y = inn, color = Ausprägung)) + 
  geom_point() + geom_line() +
  labs(title="Innerstädtische Umzüge in der Stadt München",
       y = "Anteil in Protzent",
       color = "Staatsbürgerschaft") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

p15

ggsave("Results/p15.jpg", plot = p15,width = 10, height = 6)



mnew%>%
  filter(sn == 26)%>%
ggplot(aes(x = Jahr, y = Indikatorwert, color = Ausprägung)) +geom_point() + geom_line()




