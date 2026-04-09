
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")


#viele plots davon nur sehr wenige in Presentation




Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")

m <-Mobilitaet #nache einlesung
b <-Bevoelkerungsdichte #nache einlesung



b1<- b%>%
  filter(Raumbezug == "Stadt München")
ggplot(b1, aes(x = Jahr, y = Basiswert.1)) + geom_point() + geom_line() + geom_smooth(method = "lm")
model <- lm(Jahr ~ Basiswert.1, data = b1)
summary(model)

ggplot(b1, aes(x = Jahr, y = Basiswert.1)) + geom_point() + geom_line() +
  labs(y = "Einwohnerzahl", title = "Stadt München")
#Geborene 2025: 15 399 https://de.statista.com/statistik/daten/studie/1228950/umfrage/geburten-todesfaelle-muenchen/
#Gestorbene 2025: 12 539
#12, 19 ausreißer nicht deutscher
#https://stadt.muenchen.de/dam/jcr:fcc81666-f6ef-4c83-84b3-8714c6c38f6f/mb220302_r.pdf



b2 <- b%>%
  filter(Raumbezug %in% c("Stadt München", "01 Altstadt - Lehel", "03 Maxvorstadt",
                          "02 Ludwigsvorstadt - Isarvorstadt", "08 Schwanthalerhöhe",
                          "21 Pasing - Obermenzing", "23 Allach - Untermenzing",
                          "19 Thalkirchen - Obersendling - Forstenried - Fürstenried - Solln"
                          , "13 Bogenhausen"))%>%
  group_by(Raumbezug)

ggplot(b2, aes(x = Jahr, y = Indikatorwert, color = Raumbezug)) +geom_point() +geom_line()
b3 <- b2 %>%
  filter(Raumbezug != "Stadt München")
ggplot(b3, aes(x = Jahr, y = Basiswert.1, color = Raumbezug)) +geom_point() +geom_line()
ggplot(b3, aes(x = Jahr, y = Indikatorwert, color = Raumbezug)) +geom_point() +geom_line()

bu <- b%>%
  filter(Jahr <= 2014)
bo<- b%>%
  filter(Jahr > 2014)

bu %>%
  group_by(Raumbezug) %>%
  summarise(
    korrelation = cor(Indikatorwert, Jahr ) )%>%
  print(n = Inf) # alle sehr hoch und ähnlich

bo %>%
  group_by(Raumbezug) %>%
  summarise(
    korrelation = cor(Indikatorwert, Jahr ) )%>%
  print(n = Inf) # sb 17 02 01 08  auffälig

bo1<- bo%>%
  filter(Raumbezug %in% c("01 Altstadt - Lehel", "17 Obergiesing - Fasangarten",
                          "02 Ludwigsvorstadt - Isarvorstadt", "08 Schwanthalerhöhe"))

ggplot(bo1, aes(x = Jahr , y = Indikatorwert, color = Raumbezug)) + geom_point() +geom_line()



bnew <- b%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))




mnew <- m%>% #protzentuale Wert für Ausagekraft ähnlcih wie Indikatorwert nur um 10 Faktor anders
  mutate(bpn1 = 100 *Basiswert.1 / Basiswert.5,
         bpn2 = 100 *Basiswert.2 / Basiswert.5,
         bpn3 = 100 *Basiswert.3 / Basiswert.5,
         bpn4 = 100 *Basiswert.4 / Basiswert.5)



mnew <- mnew%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))



mnew <- mnew%>%
  mutate(gsr = Basiswert.1 - Basiswert.3)
mnew <- mnew%>%
  mutate(rgsr = gsr / Basiswert.5)

mnew <- mnew%>%
  mutate(ar = Basiswert.1 - Basiswert.3 + Basiswert.2 -Basiswert.4)
mnew <- mnew%>%
  mutate(rar = ar / Basiswert.5)

m1 <- mnew%>%
  filter(Raumbezug == "Stadt München")%>%
  filter(Ausprägung == "insgesamt")
ggplot(m1, aes(x = Jahr, y = Indikatorwert)) + geom_point() + geom_line() + geom_smooth(method = "lm")

m2 <- mnew%>%
  filter(Raumbezug == "Stadt München")
ggplot(m2, aes(x = Jahr, y = Indikatorwert, color = Ausprägung)) + geom_point() +geom_line()


mnew%>%
  filter(Raumbezug == "Stadt München")%>%
  ggplot(aes(x = Jahr, y = Basiswert.1, color = Ausprägung)) +geom_point() +geom_line()


mnew%>%
  filter(Raumbezug == "Stadt München")%>%
  ggplot(aes(x = Jahr, y = Basiswert.2, color = Ausprägung)) +geom_point() +geom_line()

mnew%>%
  filter(Raumbezug == "Stadt München")%>%
  ggplot(aes(x = Jahr, y = Basiswert.3, color = Ausprägung)) +geom_point() +geom_line()




mp301 <- mnew %>%
  filter(sn == 26)
ggplot(mp301, aes(x = Jahr, y = bpn3, color = Ausprägung)) +geom_point() + geom_line() + geom_smooth(method = "lm")
ggplot(mp301, aes(x = Jahr, y = Basiswert.3, color = Ausprägung)) +geom_point() + geom_line() + geom_smooth(method = "lm")
ggplot(mp301, aes(x = Jahr, y = bpn4, color = Ausprägung)) +geom_point() + geom_line()
ggplot(mp301, aes(x = Jahr, y = Basiswert.4, color = Ausprägung)) +geom_point() + geom_line()

mp302 <- mnew %>%
  filter(sn == 17 |sn == 22 |sn == 12 |sn == 07 |sn == 10 |sn == 02 )

ggplot(mp302, aes(x = Jahr, y = bpn3, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = bpn4, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)



ggplot(mp302, aes(x = Jahr, y = bpn3 + bpn4, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)


ggplot(mp301, aes(x = Jahr, y = bpn3 , color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp301, aes(x = Jahr, y = Basiswert.3 , color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)

ggplot(mp302, aes(x = Jahr, y = bpn2, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = bpn4 +bpn3, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = Basiswert.2, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = Basiswert.4 + Basiswert.3, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)


ggplot(mp301, aes(x = Jahr, y = bpn1, color = Ausprägung)) +geom_point() + geom_line()

ggplot(mp301, aes(x = Jahr, y = Basiswert.1, color = Ausprägung)) +geom_point() + geom_line()


mplot1 <- m3%>%
  filter(Raumbezug == "Stadt München")
ggplot(mplot1, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + geom_line() #+geom_smooth(method = "lm")


ggplot(m3, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + geom_line() +facet_wrap(~ Raumbezug, scales = "free") 


ggplot(mplot1, aes(x = Jahr, y = bpn1, color = Ausprägung)) + geom_point() + geom_line()

ggplot(mplot1, aes(x = Jahr, y = bpn2, color = Ausprägung)) + geom_point() + geom_line()

ggplot(mplot1, aes(x = Jahr, y = bpn3, color = Ausprägung)) + geom_point() + geom_line()

ggplot(mplot1, aes(x = Jahr, y = bpn4, color = Ausprägung)) + geom_point() + geom_line()

mnew <- m3%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))


mnew <- mnew%>%
  mutate(gsr = Basiswert.1 - Basiswert.3)
mnew <- mnew%>%
  mutate(rgsr = gsr / Basiswert.5)

mnew <- mnew%>%
  mutate(ar = Basiswert.1 - Basiswert.3 + Basiswert.2 -Basiswert.4)
mnew <- mnew%>%
  mutate(rar = ar / Basiswert.5)

mplot009 <- mnew%>%
  filter(sn == 26)

ggplot(mplot009, aes(x = Jahr, y = gsr, color = Ausprägung)) + geom_point()+ geom_line() + geom_smooth(method = "lm")
ggplot(mplot009, aes(x = Jahr, y = rgsr, color = Ausprägung)) + geom_point()+ geom_line() + geom_smooth(method = "lm")

mplot09 <- mnew%>%
  filter(Ausprägung == "insgesamt")%>%
  filter(Jahr == 2000 | Jahr == 2005 | Jahr == 2010 | Jahr == 2015 | Jahr == 2020 | Jahr == 2024)%>%
  filter(sn != 26)

ggplot(mplot09, aes(x = Jahr, y = ar, color = Raumbezug)) + geom_point()+ geom_line() #ma könnte zunächst meinen corona Auswirkung


mplot0901 <- mnew%>%
  filter(sn == 26)

ggplot(mplot0901, aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ geom_line() # staatsbürgerschaft

ggplot(mplot0901, aes(x = Jahr, y = rar, color = Ausprägung)) + geom_point()+ geom_line()

mplot0902<- mnew%>%
  filter(sn == 17 | sn == 22)

ggplot(mplot0902, aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug)

ggplot(mplot0902, aes(x = Jahr, y = rar, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug)

ggplot(mplot0902, aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug)

ggplot(mplot0902, aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug)



mnew <- mnew %>% #indexbildung von Jahr 2000
  group_by(Raumbezug) %>%
  group_by(Ausprägung)%>%
  mutate(indexb1 = 100 * Basiswert.1 / Basiswert.1[Jahr == 2000],
         indexb2 = 100 * Basiswert.2 / Basiswert.2[Jahr == 2000],
         indexb3 = 100 * Basiswert.3 / Basiswert.3[Jahr == 2000],
         indexb4 = 100 * Basiswert.4 / Basiswert.4[Jahr == 2000],
         indexb12 = 100 * (Basiswert.1 + Basiswert.2)/ (Basiswert.1[Jahr == 2000] +Basiswert.2[Jahr == 2000]),
         indexb34 = 100 * (Basiswert.4 + Basiswert.3)/ (Basiswert.3[Jahr == 2000] +Basiswert.4[Jahr == 2000]))



mpi1 <- mnew%>%
  filter(sn == 26)%>%
  group_by(Ausprägung)
ggplot(mpi1, aes(x = Jahr , y = indexb1, color = Ausprägung)) +geom_point()+ geom_line()
ggplot(mnew, aes(x = Jahr , y = indexb1, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)

ggplot(mnew, aes(x = Jahr , y = indexb2, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)

ggplot(mnew, aes(x = Jahr , y = indexb3, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)


ggplot(mnew, aes(x = Jahr , y = indexb4, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)

ggplot(mnew, aes(x = Jahr , y = indexb12, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)


ggplot(mnew, aes(x = Jahr , y = indexb34, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)


bj <- b%>%
  rename(bi = Indikatorwert)%>%
  rename(b1 = Basiswert.1)%>%
  rename(b2 = Basiswert.2)


zusammen <- full_join(m, bj, by = c("Jahr", "Raumbezug"))

pz1 <- zusammen%>%
  filter(Ausprägung.x == "insgesamt")%>%
  filter(Jahr != 2000 & Jahr != 2001)


pz2 <- zusammen%>%
  filter(Ausprägung.x == "insgesamt")%>%
  filter(Jahr == 2024)
ggplot(pz2, aes(x = bi, y = Indikatorwert, color = Raumbezug)) + geom_point() 

zusammen <- zusammen %>% # umzüge durch quadratkilometer
  mutate(bqm1 = Basiswert.1 / b2,
         bqm2 = Basiswert.2 / b2,
         bqm3 = Basiswert.3.x / b2,
         bqm4 = Basiswert.4.x / b2)
pz3<- zusammen%>%
  filter(Ausprägung.x == "insgesamt")
ggplot(pz3, aes(x = Jahr, y = bqm1, color = Raumbezug)) +geom_point() +geom_line()
ggplot(pz3, aes(x = Jahr, y = bqm1)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(pz3, aes(x = Jahr, y = bqm2, color = Raumbezug)) +geom_point() +geom_line()


ggplot(zusammen, aes(x = Jahr, y = bqm1, color = Ausprägung.x)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(zusammen, aes(x = Jahr, y = bqm2, color = Ausprägung.x)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(zusammen, aes(x = Jahr, y = bqm3, color = Ausprägung.x)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(zusammen, aes(x = Jahr, y = bqm4, color = Ausprägung.x)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)



bnew <- bnew %>%
  group_by(Raumbezug) %>%
  group_by(Ausprägung)%>%
  mutate(indexb1 = 100 * Basiswert.1 / Basiswert.1[Jahr == 2002]
  )
ggplot(bnew, aes(x = Jahr, y = indexb1)) +geom_point() +
  geom_line()+ facet_wrap(~ Raumbezug)

mnew <- mnew %>%
  group_by(Raumbezug) %>%
  group_by(Ausprägung)%>%
  mutate(indexb5 = 100 * Basiswert.5 / Basiswert.5[Jahr == 2000]
  )
ggplot(mnew, aes(x = Jahr, y = indexb5, color = Ausprägung)) +geom_point() +
  geom_line()+ facet_wrap(~ Raumbezug)

ggplot(mnew, aes(x = Jahr, y = Indikatorwert, color = Ausprägung)) +geom_point() +
  geom_line()+ facet_wrap(~ Raumbezug)


mnew%>%
  filter(sn == 26)%>%
  ggplot(aes(x = Jahr, y = Indikatorwert, color = Ausprägung)) +geom_point() +
  geom_line()+ facet_wrap(~ Raumbezug)


mnew%>%
  filter(sn == 26)%>%
  ggplot(aes(x = Jahr, y = Basiswert.2, color = Ausprägung)) +geom_point() +
  geom_line()+ facet_wrap(~ Raumbezug)



mnew%>%
  filter(Ausprägung == "insgesamt")%>%
ggplot( aes(x =Jahr, y = bpn3 + bpn4)) + geom_point() + geom_line() + facet_wrap(~ Raumbezug)

mnew%>%
  filter(Ausprägung == "insgesamt")%>%
  ggplot( aes(x =Jahr, y =  bpn4)) + geom_point() + geom_line() + facet_wrap(~ Raumbezug)
mnew%>%
  filter(Ausprägung == "insgesamt")%>%
  ggplot( aes(x =Jahr, y = bpn3 )) + geom_point() + geom_line() + facet_wrap(~ Raumbezug)

bnew%>%
  filter(Ausprägung == "insgesamt")%>%
  filter(sn != 26)%>%
  filter(Jahr == 2024)%>%
  ggplot( aes(x =Jahr, y = Basiswert.1 )) + geom_point() + geom_line() + facet_wrap(~ Raumbezug)

mnew%>%
  filter(sn == 26)%>%
  ggplot(aes(x = Jahr, y = Basiswert.2, color = Ausprägung)) + geom_point() + geom_line()

mnew%>%
  filter(sn != 26)%>%
  ggplot(aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point() + geom_line() + facet_wrap(~ Raumbezug)


mnew%>%
  filter(sn == 12 | sn == 19)%>%
  ggplot(aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point() + geom_line() + facet_wrap(~ Raumbezug)


mnew%>%
  filter(sn == 12 | sn == 19)%>%
  ggplot(aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point() + geom_line() + facet_wrap(~ Raumbezug)

mnew <- mnew%>% 
  group_by(Raumbezug, Ausprägung, Jahr)%>%
  mutate(au = 100 *(Basiswert.3 + Basiswert.1)/ Basiswert.5,
         inn = 100 *(Basiswert.2 ) / Basiswert.5)

mnew%>%
  filter(sn == 26)%>%
  ggplot(aes(x = Jahr, y = au, color = Ausprägung)) + 
  geom_point() + geom_line() +
  labs(title="Außerstädtische Umzüge in der Stadt München",
       y = "Anteil in Protzent",
       color = "Staatsbürgerschaft") 

mnew%>%
  filter(sn == 26)%>%
  ggplot(aes(x = Jahr, y = inn, color = Ausprägung)) + 
  geom_point() + geom_line() +
  labs(title="Innerstädtische Umzüge in der Stadt München",
       y = "Anteil in Protzent",
       color = "Staatsbürgerschaft") 
