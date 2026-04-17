#mobility data adjusted
Mobilität_clean <- Mobilitaet %>%
  mutate(bezirk_num = as.numeric(sub(" .*", "", Raumbezug))) %>%
  mutate(Zuzügegesamt = Basiswert.1 + Basiswert.2) %>%
  mutate(Wegzügegesamt = Basiswert.3 + Basiswert.4)%>%
  filter(grepl("^[0-9]{2} ", Raumbezug))

#data adjusting
Mobilität_sn <- Mobilität_clean %>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))
plotdatal <- Mobilität_sn %>%
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

plotdatals <- plotdatal %>%
  group_by(Raumbezug, Jahr) %>%
  mutate(gesamt_bev = Basiswert.5[Ausprägung == "insgesamt"][1]) %>%
  ungroup() %>%
  filter(Ausprägung %in% c("deutsch", "nichtdeutsch")) %>%
  mutate(Zuzugsrate = (Zuzügegesamt / gesamt_bev) * 100,
         Wegzugsrate = (Wegzügegesamt / gesamt_bev) * 100)

#lineplot Zuzugsrate by citizenship
g14 <- ggplot(plotdatals, aes(x = Jahr, y = Zuzugsrate, color = Ausprägung)) +
  geom_line() +
  geom_point(size = 0.9) +
  facet_wrap(~ Raumbezug) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10),
        panel.spacing.x = unit(1.2, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.background = element_rect(color = "grey", fill = "grey90")) +
  scale_color_manual(values = c(deutsch = "#E69F00",
                                nichtdeutsch = "#0072B2")) +
  labs(y = "Zuzugsrate",
       color = "Ausprägung")

g14

ggsave("Results/g14.jpg", plot = g14,width = 3, height = 3)
