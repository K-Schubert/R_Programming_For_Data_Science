library(mapproj)
devtools::install_github("dkahle/ggmap")
library(ggmap)
library(magrittr)

birth_place <- c("Glasgow, Scotland", "Manacor, Spain", "Lausanne, Switzerland", "Belgrade, Serbia", "Basel, Switzerland")
register_google(key = "YOUR_API_KEY")
has_goog_key()
birth_coord <- geocode(birth_place, source = "google")

map <- get_map(location = 'Switzerland', zoom = 4)

map %>%
  ggmap()
  + geom_point(data = birth_coord, 
                        aes(lon, lat, col = Players, size = GS)) + 
    scale_size(name="Grand Slam Wins") + 
    xlab("Longitude") + ylab("Latitude")

