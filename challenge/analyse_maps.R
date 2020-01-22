# Pakete installieren und laden
## install.packages(c("tidyverse", "rgdal", "maptools"))
library(tidyverse)
library(rgdal)
library(maptools)


# Tabelle der rechtsextremen Aufmärsche laden und richtige Spaltenformate setzen
demos <- read_csv(
  "data/rechtsextreme_aufmaersche.csv",
  col_types = cols(
    .default = col_character(),
    Lat = col_double(),
    Long = col_double(),
    Datum = col_date(format = "%d.%m.%Y"),
    Teilnehmer = col_integer()
  )
)

## Bundesländer mit NAs rauswerfen
demos <- demos %>%
  filter(Bundesland != "#N/A")

# Flächen der Bundesländer laden
bundeslaender_geo_raw <- readOGR("data/bundeslaender.geo.json")
bundeslaender_geo <- fortify(bundeslaender_geo_raw, region = "NAME_1")


# Einfache Choroplethen-Karte mit Symbolen
bundeslaender_daten <- demos %>%
  group_by(Bundesland) %>%
  summarise(Anzahl = n())

bundeslaender_merge <- left_join(
  bundeslaender_geo,
  bundeslaender_daten,
  by = c("id" = "Bundesland")
)

ggplot(data = bundeslaender_merge, aes(map_id = id, x = long, y = lat, group = group, fill = Anzahl)) +
  geom_map(
    map = bundeslaender_geo,
    color = "white",
    size = 0.05
  ) +
  # scale_fill_continuous(low = "#C3DDEB", high = "#006CAB") +
  scale_fill_continuous(low = "#E0CBC3", high = "#DF6233") +
  coord_fixed() +
  theme_void()


# Einfache Symbol-Karte
bundeslaender_daten <- demos %>%
  group_by(Bundesland) %>%
  summarise(Anzahl = n(), Teilnehmer = sum(Teilnehmer, rm.na = TRUE))

bundeslaender_merge <- left_join(
  bundeslaender_geo,
  bundeslaender_daten,
  by = c("id" = "Bundesland")
)

staedte_daten <- demos %>%
  group_by(Ort, Lat, Long) %>%
  summarise(Anzahl = n(), Teilnehmer = sum(Teilnehmer, rm.na = TRUE))

ggplot(data = bundeslaender_merge, aes(map_id = id, x = long, y = lat, group = group, fill = Anzahl)) +
  geom_map(
    map = bundeslaender_geo,
    color = "white",
    size = 0.05
  ) +
  geom_point(
    data = staedte_daten,
    aes(x = Long, y = Lat, size = Teilnehmer),
    color = "#DF6233",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_continuous(low = "#EAEAEA", high = "#666666") +
  coord_fixed() +
  theme_void()
