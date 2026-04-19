## code for plot on slide 14

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

#net migration rate for total population 
plotdataNettorate <- plotdata %>%
  filter(Ausprägung == "insgesamt") %>%
  mutate(netto_rate = (Gesamtzuzug - Gesamtwegzug) / mittlere_Hauptwohnsitzbevölkerung * 100)

#plot net migration rate for total population over the years
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
  facet_wrap(~ Jahr, nrow = 4) +
  theme_minimal() +
  labs(fill = "Prozent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

g9 

ggsave("Results/nettoumzugsrate_gesamt.jpg", plot = g9, width = 4, height = 3)
