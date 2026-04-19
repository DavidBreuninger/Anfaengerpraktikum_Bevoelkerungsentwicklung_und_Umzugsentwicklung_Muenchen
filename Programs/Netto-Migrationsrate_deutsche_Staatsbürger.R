## code for plot with title "deutsche Staatsbürger" on slide 15 

# read data
bezirke <- read.csv("Data/vablock_stadtbezirk.csv")
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")

#munich geo data adjusted
bezirke <- bezirke %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()
st_crs(bezirke) <- 25832
bezirke <- st_transform(bezirke, 4326)

#perform join
plotdata <- bezirke %>%
  left_join(Mobilitaet_thin, by = c("sb_nummer" = "BezirksID"), relationship = "many-to-many")

#net migration rate for Germans
plotdataNettorate_d <- plotdata %>%
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = mittlere_Hauptwohnsitzbevölkerung[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung == "deutsch") %>%
  mutate(netto_rate = (Gesamtzuzug - Gesamtwegzug) / gesamt_bev * 100)

g11 <- ggplot(plotdataNettorate_d) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-8, 8),
    breaks = c(-8, -4, 0, 4, 8)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "deutsche Staatsbürger",
       fill = "Prozent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

g11

ggsave("Results/nettoumzugsrate_deutsch.jpg", plot = g11, width = 6, height = 6)
