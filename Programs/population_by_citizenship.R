#adjust mobility data
mnew <- Mobilitaet %>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))

#Population by citizenship slide 7
mplot1 <- mnew%>%
  filter(Raumbezug == "Stadt München")
p2<-ggplot(mplot1, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + 
  geom_line() +labs(y = "mittlere Wohnbevölkerung in Millionen",
                    color = "Staatsbürgerschaft")+
  scale_colour_manual(values = c(deutsch = "#E69F00",
                                 nichtdeutsch = "#0072B2",
                                 insgesamt = "black"))+
  scale_y_continuous(labels = label_number(scale = 1e-6)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 13))
p2

ggsave("Results/p_mittlere_Wohnbevölkerung.jpg", plot = p2,width = 10, height = 6)
