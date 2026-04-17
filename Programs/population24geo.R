#geo data transformed
bezirke <- bezirke %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()
st_crs(bezirke) <- 25832
bezirke <- st_transform(bezirke, 4326)

#population data for join prepared
Bevölkerung_clean <- Bevoelkerungsdichte %>%
  mutate(bezirk_num = as.numeric(sub(" .*", "", Raumbezug))) %>%
  filter(grepl("^[0-9]{2} ", Raumbezug))

#perform join
plotdatabevölkerung <- bezirke %>%
  left_join(Bevölkerung_clean, by = c("sb_nummer" = "bezirk_num"))

#population 2024 slide 6
plotdataBevölkerungsentwicklung <- plotdatabevölkerung %>%
  filter(Jahr == 2024)

g7 <- ggplot(plotdataBevölkerungsentwicklung) +
  geom_sf(aes(fill = Basiswert.1)) +
  theme_minimal() +
  labs(title = "Einwohneranzahl 2024",
       fill = "Anzahl") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 15))+ 
  scale_fill_gradient(
    low = "lightyellow",
    high = "saddlebrown",
    limits = c(20000,  125000),
    breaks = c(0, 20000, 40000,  60000, 80000, 100000, 125000)
  )

g7

ggsave("Results/g7.jpg", plot = g7,width = 3, height = 3)