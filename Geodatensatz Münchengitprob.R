#Tipp aus Word
#Pakete laden
#install.packages("sf")
library("sf")
library("dplyr")
library("ggplot2")
library("readr")
library("stringr")

#Datensätze einlesen
#https://opendata.muenchen.de/dataset/vablock_stadtbezirke_opendata
vablock_stadtbezirk <- read.csv("Data/vablock_stadtbezirk.csv")
Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")
read_csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
bezirke <- vablock_stadtbezirk

#Geodatensatz plotten
bezirke <- bezirke %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()
st_crs(bezirke) <- 25832
bezirke <- st_transform(bezirke, 4326)


#Mobilitätsdatensatz für join vorbereiten
Mobilität_clean <- Mobilitaet %>%
  mutate(bezirk_num = as.numeric(sub(" .*", "", Raumbezug))) %>%
  mutate(Zuzügegesamt = Basiswert.1 + Basiswert.2) %>%
  mutate(Wegzügegesamt = Basiswert.3 + Basiswert.4)
Mobilität_cleaner <- Mobilität_clean %>%
  filter(grepl("^[0-9]{2} ", Raumbezug))

#Join durchführen
plotdata <- bezirke %>%
  left_join(Mobilität_cleaner, by = c("sb_nummer" = "bezirk_num"))


#plotdata für Zuzugs- und Wegzugsrate
plotdatapi <- plotdata %>%
  mutate(Zuzugsrate = (Zuzügegesamt / Basiswert.5) * 100,
         Wegzugsrate = (Wegzügegesamt / Basiswert.5) * 100) %>%
  filter(Ausprägung == "insgesamt")


plotdatapd <- plotdata %>%
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = Basiswert.5[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung == "deutsch") %>%
  mutate(Zuzugsrate = Zuzügegesamt / gesamt_bev * 100,
         Wegzugsrate = Wegzügegesamt / gesamt_bev * 100)

plotdatapnd <- plotdata %>%
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = Basiswert.5[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung == "nichtdeutsch") %>%
  mutate(Zuzugsrate = Zuzügegesamt / gesamt_bev * 100,
         Wegzugsrate = Wegzügegesamt / gesamt_bev * 100)


plotdatapd <- plotdata %>%
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = Basiswert.5[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung == "deutsch") %>%
  mutate(Zuzugsrate = Zuzügegesamt / gesamt_bev * 100,
         Wegzugsrate = Wegzügegesamt / gesamt_bev * 100)

plotdata <- plotdata%>%
  group_by(Raumbezug, Jahr) %>%
  mutate(wer = 100 *((Basiswert.1 + Basiswert.2) / (Basiswert.1[Ausprägung == "insgesamt"][1] + Basiswert.2[Ausprägung == "insgesamt"][1])),
                                       zur = 100* ((Basiswert.3 + Basiswert.4) / (Basiswert.3[Ausprägung == "insgesamt"][1] + Basiswert.4[Ausprägung == "insgesamt"][1])))
#Zuzugsrate Nicht-Deutsche Staatsbürger
 plotdata%>%
  filter(Ausprägung == "nichtdeutsch")%>%
ggplot() +
  geom_sf(aes(fill = zur)) +
  scale_fill_viridis_c(
    limits = c(0, 100),
    values = scales::rescale(c(0, 25, 30, 35, 40,  50, 55, 60, 65, 70, 75, 100)),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzugsrate nichtdeutscher Staatsbürgern",
       fill = "Protzent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
 
 
 plotdata%>%
   filter(Ausprägung == "nichtdeutsch")%>%
   ggplot() +
   geom_sf(aes(fill = zur)) +
   scale_fill_viridis_c(
     limits = c(0, 100),
     values = scales::rescale(c(0, 25,  40, 45, 50, 55, 60,  75, 100)),
     breaks = c(0, 25, 50, 75, 100)
   ) +
   facet_wrap(~ Jahr) +
   theme_minimal() +
   labs(
        fill = "Protzent") +
   theme(axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank())
 
 
 
g1 <- plotdata%>%
   filter(Ausprägung == "nichtdeutsch")%>%
   ggplot() +
   geom_sf(aes(fill = zur)) +
   facet_wrap(~ Jahr) +
   theme_minimal() +
   labs(
        fill = "Protzent") +
   theme(axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank()) + scale_fill_gradient(
           low = "lightyellow",
           high = "saddlebrown",
           limits = c(30, 90),
           breaks = c(30, 50, 70, 90)
         )

 


g2 <-plotdata%>%
  filter(Ausprägung == "deutsch")%>%
  ggplot() +
  geom_sf(aes(fill = zur)) +
  scale_fill_gradient(
    low = "thistle",
    high = "purple4",
    limits = c(10, 70),
    breaks = c(10, 30, 50, 70)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(
       fill = "Protzent") +

plotdatapnd <- plotdata %>%
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = Basiswert.5[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung == "nichtdeutsch") %>%
  mutate(Zuzugsrate = Zuzügegesamt / gesamt_bev * 100,
         Wegzugsrate = Wegzügegesamt / gesamt_bev * 100)

#Zuzugsrate Nicht-Deutsche Staatsbürger
ggplot(plotdatapnd) +
  geom_sf(aes(fill = Zuzugsrate)) +
  scale_fill_viridis_c(
    limits = c(0, 30),
    values = scales::rescale(c(0, 5, 10, 20, 30)),
    breaks = c(0, 5, 10, 20, 30)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzugsrate von Nicht-Deutschen",
       fill = "Zuzugsrate") +

  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


g3<-plotdata%>%
  filter(Ausprägung == "deutsch")%>%
  ggplot() +
  geom_sf(aes(fill = wer)) +
  scale_fill_gradient(
    low = "thistle",
    high = "purple4",
    limits = c(10, 70),
    breaks = c(10, 30, 50, 70)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(
       fill = "Protzent") +

#Wegzugsrate von Nicht-Deutschen Staatsbürgern
ggplot(plotdatapnd) +
  geom_sf(aes(fill = Wegzugsrate)) +
  scale_fill_viridis_c(
    limits = c(0, 35),
    values = scales::rescale(c(0, 5, 10, 20, 30, 35)),
    breaks = c(0, 10, 20, 30, 35)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzugsrate von Nicht-Deutschen",
       fill = "Wegzugsrate") +

  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


g4 <-plotdata%>%
  filter(Ausprägung == "nichtdeutsch")%>%
  ggplot() +
  geom_sf(aes(fill = wer)) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "saddlebrown",
    limits = c(30, 90),
    breaks = c(30, 50, 70, 90)
  )+
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(
       fill = "Protzent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


#Wegzugsrate von Nicht-Deutschen Staatsbürgern

#Zuzugsrate insgesamt

#Zuzugsrate insgesamt

ggplot(plotdatapi) +
  geom_sf(aes(fill = Zuzugsrate)) +
  scale_fill_viridis_c(
    limits = c(10, 40),

    values = scales::rescale(c(10,  12.5, 15, 17.5, 20, 30, 40)),
    breaks = c( 10,  20,  30,  40)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzugsrate",
       fill = "Protzent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) 


g5 <- ggplot(plotdatapi) +
  geom_sf(aes(fill = Zuzugsrate)) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzugsrate",
       fill = "Protzent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) + scale_fill_gradient(
          low = "lightyellow",
          high = "saddlebrown",
          limits = c(10, 40),
          breaks = c( 10,  20,  30,  40)
        )



#Wegzugsrate insgesamt
g6 <-ggplot(plotdatapi) +
  geom_sf(aes(fill = Wegzugsrate)) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzugsrate",
       fill = "Protzent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) + scale_fill_gradient(
          low = "lightyellow",
          high = "saddlebrown",
          limits = c(10, 40),
          breaks = c(10,  20,  30,  40)
        )

#Zuzugsrate von Deutschen Staatsbürgern


#Wegzugsrate von Deutschen Staatsbürgern


#Plots mit Bevölkerungsdatensatz
#Bevölkerungsdatensatz für join vorbereiten
Bevölkerung_clean <- Bevoelkerungsdichte %>%

    values = scales::rescale(c(10, 20, 30, 40)),
    breaks = c(10, 15, 20, 25, 30, 35, 40)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(fill = "Zuzugsrate") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Wegzugsrate insgesamt
ggplot(plotdatapi) +
  geom_sf(aes(fill = Wegzugsrate)) +
  scale_fill_viridis_c(
    limits = c(10, 40),
    values = scales::rescale(c(0, 5, 10, 20, 30, 40)),
    breaks = c(10, 15, 20, 25, 30, 35, 40)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzugsrate insgesamt",
       fill = "Wegzugsrate") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Zuzugsrate von Deutschen Staatsbürgern
ggplot(plotdatapd) +
  geom_sf(aes(fill = Zuzugsrate)) +
  scale_fill_viridis_c(
    limits = c(0, 15),
    breaks = c(0, 5, 10, 15)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzugsrate von Deutschen",
       fill = "Zuzugsrate") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Wegzugsrate von Deutschen Staatsbürgern
ggplot(plotdatapd) +
  geom_sf(aes(fill = Wegzugsrate)) +
  scale_fill_viridis_c(
    limits = c(0, 15),
    breaks = c(0, 5, 10, 15)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzugsrate von Deutschen",
       fill = "Wegzugsrate") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Plots mit Bevölkerungsdatensatz
#Bevölkerungsdatensatz für join vorbereiten
Bevölkerung_clean <- indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25 %>%

  mutate(bezirk_num = as.numeric(sub(" .*", "", Raumbezug))) %>%
  filter(grepl("^[0-9]{2} ", Raumbezug))

#Join durchführen
plotdatabevölkerung <- bezirke %>%
  left_join(Bevölkerung_clean, by = c("sb_nummer" = "bezirk_num"))

#Bevölkerung 2024
plotdataBevölkerungsentwicklung <- plotdatabevölkerung %>%
  filter(Jahr == 2024)


g7 <-ggplot(plotdataBevölkerungsentwicklung) +
  geom_sf(aes(fill = Basiswert.1)) +
  theme_minimal() +
  labs(title = "Einwohneranzahl 2024",
       fill = "Anzahl") +

ggplot(plotdataBevölkerungsentwicklung) +
  geom_sf(aes(fill = Basiswert.1)) +
  scale_fill_viridis_c(
    limits = c(20000, 125000),
    breaks = c(20000, 40000, 60000, 80000, 100000, 125000)
  ) +
  theme_minimal() +
  labs(title = "Bevölkerung",
       fill = "Anzahl Einwohner") +

  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) + scale_fill_gradient(
          low = "lightyellow",
          high = "saddlebrown",
          limits = c(20000,  125000),
          breaks = c(0, 20000, 40000,  60000, 80000, 100000, 125000)
        )

#Bevölkerungsdichte 2024
plotdataBevölkerungsdichte <- plotdatabevölkerung %>%
  mutate(Bevölkerungsdichte = Basiswert.1 / Basiswert.2) %>%
  filter(Jahr == 2024, Ausprägung == "insgesamt")


g8 <-ggplot(plotdataBevölkerungsdichte) +
  geom_sf(aes(fill = Bevölkerungsdichte)) +
  theme_minimal() +
  labs(title = "Bevölkerungsdichte 2024",
       fill = "Dichte pro km²") + 

ggplot(plotdataBevölkerungsdichte) +
  geom_sf(aes(fill = Bevölkerungsdichte)) +
  scale_fill_viridis_c(
    limits = c(0, 16000),
    breaks = c(0, 4000, 8000, 12000, 16000)
  ) +
  theme_minimal() +
  labs(title = "Bevölkerungsdichte",
       fill = "Dichte pro km²") +

  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+ scale_fill_gradient(
          low = "lightyellow",
          high = "saddlebrown",
          limits = c(0, 16000),
          breaks = c(0, 4000, 8000, 12000, 16000)
        )











#Netto-Migrationsraten
plotdataNetto <- plotdata %>%
  filter(Ausprägung == "insgesamt") %>%
  mutate(netto = (Basiswert.1 + Basiswert.2) - (Basiswert.3 + Basiswert.4))

#Netto-Migrationsrate insgesamt
plotdataNettorate <- plotdataNetto %>%
  mutate(netto_rate = (Zuzügegesamt - Wegzügegesamt) / Basiswert.5 * 100)


g9<-ggplot(plotdataNettorate) +

ggplot(plotdataNettorate) +

  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "purple",
    mid = "white",
    high = "yellow",
    midpoint = 0,
    limits = c(-7, 7),
    breaks = c(-7, -4, -2, 0, 2, 4, 7)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +

  labs(title = "Nettoumzugsrate",
       fill = "Protzent") +

  labs(fill = "Netto-Migrationsrate") +

  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Netto Migrationsrate für nicht-deutsche
plotdataNettoratend <- plotdata %>%
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = Basiswert.5[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung == "nichtdeutsch") %>%
  mutate(netto_rate = (Zuzügegesamt - Wegzügegesamt) / gesamt_bev * 100)

g10<- ggplot(plotdataNettoratend) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "purple",
    mid = "white",
    high = "yellow",
    midpoint = 0,
    limits = c(-6, 6),
    breaks = c(-6, -3, 0, 3, 6)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +

  labs(
       fill = "Protzent") +

  labs(title = "Nicht-Deutsche Staatsbürger",
       fill = "Rate") +

  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Netto-Migrationsrate von deutschen
plotdataNettorated <- plotdata %>%
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = Basiswert.5[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung == "deutsch") %>%
  mutate(netto_rate = (Zuzügegesamt - Wegzügegesamt) / gesamt_bev * 100)

g11<-ggplot(plotdataNettorated) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "purple",
    mid = "white",
    high = "yellow",
    midpoint = 0,
    limits = c(-6, 6),
    breaks = c(-6, -3, 0, 3, 6)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +

  labs(
       fill = "Protzent") +

  labs(title = "Deutsche Staatsbürger",
       fill = "Rate") +

  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

ggsave("Results/g1.jpg", plot = g1,width = 3, height = 3)
ggsave("Results/g2.jpg", plot = g2,width = 3, height = 3)
ggsave("Results/g3.jpg", plot = g3,width = 3, height = 3)
ggsave("Results/g4.jpg", plot = g4,width = 3, height = 3)
ggsave("Results/g10.jpg", plot = g10,width = 3, height = 3)
ggsave("Results/g11.jpg", plot = g11,width = 3, height = 3)

ggsave("Results/g5.jpg", plot = g5,width = 3, height = 3)
ggsave("Results/g6.jpg", plot = g6,width = 3, height = 3)
ggsave("Results/g7.jpg", plot = g7,width = 3, height = 3)
ggsave("Results/g8.jpg", plot = g8,width = 3, height = 3)
ggsave("Results/g9.jpg", plot = g9,width = 3, height = 3)
