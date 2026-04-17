library("sf")

#geo data transformed
bezirke <- bezirke %>%
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

g8 <- ggplot(plotdataBevölkerungsdichte) +
  geom_sf(aes(fill = Bevölkerungsdichte)) +
  theme_minimal() +
  labs(title = "Bevölkerungsdichte",
       fill = "Dichte pro km²") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5))+ 
  scale_fill_gradient(
    low = "lightyellow",
    high = "saddlebrown",
    limits = c(0, 16000),
    breaks = c(0, 4000, 8000, 12000, 16000)
  )

g8

ggsave("Results/g8.jpg", plot = g8,width = 3, height = 3)
