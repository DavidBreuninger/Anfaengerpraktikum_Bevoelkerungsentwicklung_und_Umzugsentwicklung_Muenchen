## code for the plot of population density 2024 on slide 6
#read data
vablock_stadtbezirk <- read.csv("Data/vablock_stadtbezirk.csv")
Bevoelkerungsdichte_thin <- read.csv("Clean_Data/Bevoelkerungsdichte_thin.csv")

#geo data transformed
bezirke <- vablock_stadtbezirk %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()
st_crs(bezirke) <- 25832
bezirke <- st_transform(bezirke, 4326)

#perform join
plotdatabevölkerung <- bezirke %>%
  left_join(Bevoelkerungsdichte_thin, by = c("sb_nummer" = "BezirksID"))

#population density 2024 
plotdataBevölkerung2024 <- plotdatabevölkerung %>%
  filter(Jahr == 2024, Ausprägung == "insgesamt")

# plot
density2024 <- ggplot(plotdataBevölkerung2024) +
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
