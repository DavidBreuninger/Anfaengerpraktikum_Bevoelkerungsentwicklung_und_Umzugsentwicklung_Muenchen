library("sf")
bezirke <- read.csv("Data/vablock_stadtbezirk.csv")
Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")

#munich geo data adjusted
bezirke <- bezirke %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()
st_crs(bezirke) <- 25832
bezirke <- st_transform(bezirke, 4326)

#mobility data adjusted
Mobilität_clean <- Mobilitaet %>%
  mutate(bezirk_num = as.numeric(sub(" .*", "", Raumbezug))) %>%
  mutate(Zuzügegesamt = Basiswert.1 + Basiswert.2) %>%
  mutate(Wegzügegesamt = Basiswert.3 + Basiswert.4)%>%
  filter(grepl("^[0-9]{2} ", Raumbezug))

#perform join
plotdata <- bezirke %>%
  left_join(Mobilität_clean, by = c("sb_nummer" = "bezirk_num"))

#net migration rate total population slide 14
plotdataNettorate <- plotdata %>%
  filter(Ausprägung == "insgesamt") %>%
  mutate(netto_rate = (Zuzügegesamt - Wegzügegesamt) / Basiswert.5 * 100)

g9 <- ggplot(plotdataNettorate) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-8, 8),
    breaks = c(-8, -4,  0, 4, 8)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(fill = "Prozent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

g9 

ggsave("Results/nettoumzugsrate_gesamt.jpg", plot = g9, width = 6, height = 6)
