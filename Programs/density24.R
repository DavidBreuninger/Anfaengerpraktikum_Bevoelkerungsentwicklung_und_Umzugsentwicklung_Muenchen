library("sf")
#read data
vablock_stadtbezirk <- read.csv("Data/vablock_stadtbezirk.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")

#geo data transformed
bezirke <- vablock_stadtbezirk %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()
st_crs(bezirke) <- 25832
bezirke <- st_transform(bezirke, 4326)

##plots with population data
#population data for join prepared
Bevölkerung_clean <- Bevoelkerungsdichte %>%
  mutate(bezirk_num = as.numeric(sub(" .*", "", Raumbezug))) %>%
  filter(grepl("^[0-9]{2} ", Raumbezug))

#perform join
plotdatabevölkerung <- bezirke %>%
  left_join(Bevölkerung_clean, by = c("sb_nummer" = "bezirk_num"))

#population density 2024 slide 6
plotdataBevölkerungsdichte <- plotdatabevölkerung %>%
  mutate(Bevölkerungsdichte = Basiswert.1 / Basiswert.2) %>%
  filter(Jahr == 2024, Ausprägung == "insgesamt")

density2024 <- ggplot(plotdataBevölkerungsdichte) +
  geom_sf(aes(fill = Bevölkerungsdichte)) +
  theme_minimal() +
  labs(title = "Bevölkerungsdichte 2024",
       fill = "Dichte pro km²") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)) + 
  scale_fill_gradient(
    low = "#EFEDF5",
    high = "#54278F",
    limits = c(0, 16000),
    breaks = c(0, 4000, 8000, 12000, 16000)
  )

density2024

ggsave("Results/density2024.jpg", plot = density2024, width = 8, height = 5)