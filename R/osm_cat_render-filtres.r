waterarea <- list(
  waterway = "riverbank",
  landuse = c("reservoir", "water", "basin", "salt_pond"),
  natural = c("lake", "water"),
  amenity = "swimming_pool",
  leisure = "swimming_pool"
) # minzoom: 12

waterway_low <- list(waterway = c("river", "canal")) # minzoom: 8, maxzoom: 12

waterway_med <- list(waterway = c("stream", "river", "canal")) # minzoom: 13; maxzoom: 14

waterway_high <- list(waterway = c("weir", "river", "canal", "derelict_canal", "stream", "drain", "ditch", "wadi")) # minzoom: 15

placenames_medium <- list(
  place = c("town", "city"),
  admin_level = 0:9
) # minzoom: 4; maxzoom: 15

placenames_small <- list(place = c("suburb", "village", "locality", "hamlet", "quarter", "neighbourhood", "isolated_dwelling", "farm")) # minzoom: 10; maxzoom: 17
# AND NOT tags @> 'capital=>yes


motorway_label <- list(highway = c("motorway", "trunk")) # minzoom: 9

mainroad_label <- list(highway = c("primary", "secondary", "tertiary")) # minzoom: 12

minorroad_label <- list(highway = c("motorway", "motorway_link", "trunk", "trunk_link", "primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", "road", "path", "track", "service", "footway", "bridleway", "cycleway", "steps", "pedestrian", "living_street", "unclassified", "residential", "raceway")) # minzoom: 14

building_label <- list(building = "!no") # minzoom: 14
# AND building NOT=('no')

leisure_label <- list(leisure = c("park", "sports_centre", "stadium", "pitch")) # minzoom: 14

objectes_osm_cat <- c(waterarea, waterway_low, waterway_med, waterway_high, placenames_medium, placenames_small, motorway_label, mainroad_label, minorroad_label, building_label, leisure_label)
# save(objectes_osm_cat, file="data/objectes_osm_cat.RData", compress="xz")

#' Filtres dels objectes renderitzats a openstreetmap.cat
#'
#' Filtres extrets de <https://github.com/osm-bzh/osmbr-mapstyle/blob/master/osm_ca.yml>
#' @return
#' @export
#'
#' @examples
filtres_osm_cat <- function() {
  lapply(objectes_osm_cat, function(y) {
    # if (length(y) > 1){
    paste0("^(", paste(y, collapse = "|"), ")$")
    # } else {
    #   y
    # }
  })
}
