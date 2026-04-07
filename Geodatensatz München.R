#Tipp aus Word
#Pakete laden
install.packages("sf")
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
ggplot(bezirke) + 
  geom_sf() + 
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


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

#Zuzugsrate insgesamt
ggplot(plotdatapi) +
  geom_sf(aes(fill = Zuzugsrate)) +
  scale_fill_viridis_c(
    limits = c(10, 40),
    values = scales::rescale(c(10, 20, 30, 40)),
    breaks = c(10, 15, 20, 25, 30, 35, 40)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzugsrate insgesamt",
       fill = "Zuzugsrate") +
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

maxb <- max(abs(plotdataBevölkerungsentwicklung$Basiswert.1), na.rm = TRUE)
minb <- min(abs(plotdataBevölkerungsentwicklung$Basiswert.1), na.rm = TRUE)
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
        axis.title = element_blank())

#Bevölkerungsdichte 2024
plotdataBevölkerungsdichte <- plotdatabevölkerung %>%
  mutate(Bevölkerungsdichte = Basiswert.1 / Basiswert.2) %>%
  filter(Jahr == 2024, Ausprägung == "insgesamt")

maxd <- max(abs(plotdataBevölkerungsdichte$Bevölkerungsdichte), na.rm = TRUE) %>% round(digits = 2)
mind <- min(abs(plotdataBevölkerungsdichte$Bevölkerungsdichte), na.rm = TRUE) %>%
  round(digits = 2)

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
        axis.title = element_blank())

#Netto-Migrationsraten
#Netto-Migrationsrate insgesamt
plotdataNettorate <- plotdataNetto %>%
  mutate(netto_rate = (Zuzügegesamt - Wegzügegesamt) / Basiswert.5 * 100)

ggplot(plotdataNettorate) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-7, 7),
    breaks = c(-7, -4, -2, 0, 2, 4, 7)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Netto-Migrationsrate nach Stadtbezirk",
       fill = "Netto-Migrationsrate") +
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

ggplot(plotdataNettoratend) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-6, 6),
    breaks = c(-6, -3, 0, 3, 6)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Netto-Migrationsrate von Nicht-Deutschen",
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

ggplot(plotdataNettorated) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-4, 4),
    breaks = c(-4, -2, 0, 2, 4)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Netto-Migrationsrate von Deutschen",
       fill = "Rate") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())