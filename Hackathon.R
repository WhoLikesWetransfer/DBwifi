library(tidyverse)
path <- "C:\\Users\\Jonathan\\Hackathon\\DBData.csv"
path2 <- "C:\\Users\\Jonathan\\Hackathon\\BahnhofCords.csv"
#Pfad impport

raw <- read_delim(file = path, delim = ";")
Bahnhof <- read_delim(file = path2, delim = ";")
#Dateityp getrennt durch ";" laden und benennen

cleaned <- raw %>% 
  filter(link_gw_conn) %>%
  #arrange(sid, created) %>%
  mutate(uploadpp = tprx / pax_auth,
         downloadpp = tptx / pax_auth,
         time = created %>% as.character() %>% substr(1, 16))
#Nur aktive Gateway Verbindungen mit "TRUE", beim Zeitformat Sekunden abgeschnitten, Internetgeschwindigkeit pro Person
  
aggregated <- cleaned %>%
  group_by(time, sid) %>%
  summarize(
    pingavg = mean(link_ping, na.rm = TRUE),
    uploadppavg = mean(uploadpp, na.rm = TRUE),
    downloadppavg = mean(downloadpp, na.rm = TRUE),
    gps_breiteavg = mean(gps_breite, na.rm = TRUE),
    gps_laengeavg = mean(gps_laenge, na.rm = TRUE),
    #Durchschnittswerte von den Koordinaten, Ping und von der Internetgeschwindigkeit, NA's bereinigt
  )

finde_bahnhof <- function(breite, laenge) {
  result <- Bahnhof %>%
    mutate(Lat_Breite = abs(Lat_Breite - breite),
           Lat_Long = abs(Lat_Long - laenge),
           ) %>%
    filter(Lat_Breite <= 0.05 & Lat_Long <= 0.05)
  if(nrow(result) == 0) {
    return(NA)
  } else {
    return(result$Bahnhof)
  }
  #NUR Funktion: Bahnhof Koordinaten mit Zugkoordinaten vergleichen, alles mit Abstand L&B unter 0.05 = Bahnhofname, ansonten NA
}    

full <- aggregated %>%
  rowwise() %>%
  mutate(Bahnhof = finde_bahnhof(breite = gps_breiteavg,
                                 laenge = gps_laengeavg)
         #Funktion anwenden und Zeile f�r Zeile anwenden
  )

NONA <- full %>% 
  ungroup() %>%
  group_by(sid) %>%
  arrange(sid, time) %>%
  #mutate(fahrt = stringi::stri_rand_strings(1, length = 10)) %>%
  mutate(fahrt = ifelse(difftime(time, lag(time), units = "mins") > 3, stringi::stri_rand_strings(1, length = 10), NA)) %>%
  fill(fahrt, .direction = "down") %>%
  mutate(fahrt = ifelse(is.na(fahrt), as.character(sid), fahrt)) %>%
  ungroup() %>%
  group_by(fahrt) %>%
  fill(Bahnhof, .direction = c("down")) %>%
  inner_join(Bahnhof, by = "Bahnhof") %>%
  mutate(cordsdiff = (abs(Lat_Breite - gps_breiteavg) + abs(Lat_Long - gps_laengeavg))/2) %>%
  ungroup() %>%
  arrange(fahrt, time)
#Ungroup weil wegen Rowwise und vorheriger Gruppierung(?), Gruppierung nach Zug ID
#Alles mit mehr als 3 Minuten Abstand bekommt Random String
#Leere von oben nach unten mit Bahnhof auff�llen
#Innerjoin = Schnittmenge - F�ge dem Bahnhof die Koordinate aus "Bahnhof" hinzu
#Neue Spalte Cordsdiff --> Finde n�hesten Bahnhof, Position des Zuges von der Position des Bahnhofs abziehen
  
station <- 1:nrow(NONA) 
x <- 1
for(i in 1:nrow(NONA)) {
  if(i > 1) {
    if(NONA$fahrt[i-1] != NONA$fahrt[i]) {
      x <- 1
    } else {
      if(NONA$Bahnhof[i-1] != NONA$Bahnhof[i]){
        x <- x + 1
      }
    }
    station[i] <- x
  }
}
#Neue Spalte "Station" (Anzahl), z�hlt die Stationen durch
#nrow z�hlt die Zeilen in NONA
#Ist die vorherige fahrt ID eine andere, ist dies die erste Station und somit "1"
#Ansonsten: F�r jede neue Station i + 1 aufz�hlen

NONA$station <- station 

realbahnhof <- NONA %>% 
  group_by(station, fahrt) %>%
  mutate(realstation = ifelse(min(cordsdiff) == cordsdiff, station, NA)) %>%
  mutate(realbahnhof = ifelse(min(cordsdiff) == cordsdiff, Bahnhof, NA)) %>%
  ungroup() %>%
  group_by(fahrt) %>%
  arrange(fahrt, time) %>%
  fill(realbahnhof, .direction = c("down")) %>%
  fill(realstation, .direction = c("down")) %>%
  drop_na(realbahnhof)%>%
  mutate(streckenname = ifelse(station == realstation, NA, paste0(realbahnhof, " nach ",Bahnhof))) %>%
  fill(streckenname, .direction = c("up")) %>%
  drop_na(streckenname) 
#Erstelle neue Spalten mit tats�chlichem Bahnhof und Station, nur bei Minimum von cordsdif ausf�llen, ansonsten NA
#Realer Bahnhof und reale Station auff�llen und die vorherige Rechteck Variante unn�tig machen
#NA's l�schen
#Neue Spalte Streckenname: Wenn Realstation ungleich Station --> Realbahnhof + Wort("nach") + Bahnhof
# F�lle mit diesem Streckennamen nach oben auf und l�sche alle verbliebenden NA

qualitaet <- realbahnhof %>%
  group_by(streckenname) %>%
  summarize(
    upload = mean(uploadppavg, na.rm = TRUE),
    download = mean(downloadppavg, na.rm = TRUE),
    ping = mean(pingavg, na.rm = TRUE),
    n = n_distinct(fahrt)
  ) %>%
  filter(n >= 5)
# Aggregation: W�hle nur Streckennamen, mit mehr >5 unterschiedlichen Fahrten
# Durschnitt der Internetverbinung + ping pro Strecke

five <- realbahnhof %>%
  filter( streckenname == "Koeln Hbf nach Hannover Hbf") %>%
  group_by(fahrt) %>%
  summarize(datenpunkte = n()) %>%
  head(5)
# Test um uns exemplarisch die Fahrten und die Menge an Zeitdatenpunkten f�r eine Strecke (hier K�ln-Hannover) anzuschauen 

library(ggplot2)

realbahnhof %>%
  filter(fahrt %in% five$fahrt) %>%
  filter( streckenname == "Koeln Hbf nach Hannover Hbf") %>%
  filter(downloadppavg < 10000) %>%
  ungroup() %>%
  group_by(fahrt) %>%
  mutate(minute = 1:n()) %>% 
  ggplot() +
    aes(x = minute, y = downloadppavg, colour = fahrt) +
    geom_smooth(se = FALSE)
install
# Test um uns die Visualisierung der Internetgeschwindigkeit f�r verschiedene Fahrten f�r das zu erstellende Dashboard anzuschauen


