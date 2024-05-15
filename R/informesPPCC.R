#' Genera informes pels  PPCC
#'
#' Generar informes per territoris, comarques o municipis dels Països Catalans amb tots els objectes d'OSM segons els filtres especificats. Només inclou els municipis i zones catalanoparlants de les comarques bilingües.
#'
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació dels informes serà la subcarpeta \code{informes}.
#' @param etiquetes filtre d'etiquetes d'objectes d'OSM per la consulta d'Overpass seguint el format del paràmetre `features` d'[osmdata::add_osm_features()]. Combinació `O`.
#' @param claus filtre de claus d'objectes d'OSM per la consulta d'Overpass seguint el format del paràmetre `key` d'[osmdata::add_osm_feature()]. Combinació `I`.
#' @param actualitzaInformes si és `TRUE` i ja existeix el fitxer d'informe, el mou a la carpeta «ANTIC».
#' @param sufixFitxers text afegir com a sufix al nom dels fitxers dels informes («arrelProjecte/informe-Regio-comarca$sufixFitxer$.tsv»).
#' @param divisions `data.frame` amb informació dels [PPCC], \code{\link{territoris}}, \code{\link{comarques}} o \code{\link{municipis}}. Per defecte, comarques.
#' @param consulta_wikidata Si és `TRUE`, afegeix les columnes `noms_wd`, `ca.viquipedia`, `wikidata_tipus` amb informació extreta de Wikidata pels objectes que tenen etiqueta wikidata.
#' @param coordenades Si és `TRUE`, els informes inclouen les columnes `latitude` i `longitude`. Per defecte, `FALSE`.
#' @param data Punt en el temps en format [ISO_8601](https://ca.wikipedia.org/wiki/ISO_8601). Per exemple, «2014-09-11T00:00:00Z» per l'11 de setembre de 2014. Si no s'especifica, consulta el moment actual.
#' @param data2 Data en el mateix format que `data`. Si s'especifica, retorna la diferència entre `data` i `data2` (veure [wiki](https://wiki.openstreetmap.org/wiki/Overpass_API/Overpass_QL#Difference_between_two_dates_(diff))).
#' @param adiff Si és `TRUE` i hi ha paràmetre `data(2)*`, afegeix columnes per les dades de [diferències augmentades](https://wiki.openstreetmap.org/wiki/Overpass_API/Overpass_QL#Augmented-difference_between_two_dates_(adiff)).
#' @param format Com es desaran els resultats. Per defecte, "RData".
#'
#' @return Retorna una llista amb els informes.
#' @export
#'
#' @examples
#' ordres <- obte_informesPPCC(
#'   arrelProjecte = "PPCC/osm_cat",
#'   sufixFitxers = "_name-osm_cat",
#'   divisions = comarques[!comarques$regio %in% c("CatNord", "Sardenya"), ]
#' )
obte_informesPPCC <- function(arrelProjecte, etiquetes = filtres_osm_cat(), claus = c("name", "!name:ca"),
                              actualitzaInformes = FALSE, sufixFitxers = "", divisions = toponimsCat::comarques,
                              consulta_wikidata = FALSE,
                              coordenades = FALSE, data = NULL, data2 = NULL, adiff = FALSE, format = c("RData", "tsv")) {
  format <- match.arg(format)

  if (!"informe" %in% names(divisions)) {
    divisions$informe <- cami_informePPCC(
      divisions = divisions, arrelProjecte = arrelProjecte, sufixFitxers = sufixFitxers, format = format
    )
  }

  nom_informe <- gsub(
    paste0("^", file.path(arrelProjecte, "informes"), "/|\\.(RData|tsv)$"),
    "",
    divisions$informe,
    ignore.case = TRUE
  )

  informes <- list()
  pb <- pbapply::timerProgressBar(max = nrow(divisions))
  on.exit(close(pb))
  for (i in 1:nrow(divisions)) {
    cop <- osmdata::opq(
      bbox = paste0("relation(id:", divisions$id[i], ")"),
      osm_types = "nwr",
      out = if (coordenades) "tags center" else "tags",
      datetime = data, datetime2 = data2, adiff = adiff, timeout = 1000
    ) |>
      osmdata::add_osm_features(features = etiquetes, value_exact = FALSE) |>
      osmdata::add_osm_feature(key = claus)
    consulta <- osmdata::opq_string(cop)

    if ("parcial" %in% names(divisions) && divisions$parcial[i]) { # comarques parcials
      areaDivisio <- paste0("(relation(id:", divisions$id[i], "); map_to_area->.divisio; ")
      consulta <- gsub(
        paste0("(relation(id:", divisions$id[i], "); map_to_area->.searchArea; );"),
        paste0("(relation(id:", divisions$id[i], "); map_to_area->.divisio; area[name='Països Catalans']->.llengua; );"),
        consulta,
        fixed = TRUE
      )
      consulta <- gsub(" (area.searchArea);", " (area.divisio)(area.llengua);", consulta)
    }

    informe <- obte_informe(
      arrelProjecte = arrelProjecte, fitxerInforme = divisions$informe[i],
      consulta = consulta, actualitzaInforme = actualitzaInformes, consulta_wikidata = consulta_wikidata
    )
    informes[[nom_informe[i]]] <- informe

    pbapply::setTimerProgressBar(pb, i)
  }

  return(informes)
}


cami_informePPCC <- function(divisions, arrelProjecte, sufixFitxers, format = c("RData", "tsv")) {
  if ("admin_level" %in% names(divisions)) {
    fitxerInforme <- paste0("informe-", divisions$regio, "-", divisions$`name:ca`, sufixFitxers)
  } else if ("regio" %in% names(divisions)) { ## territoris dels Països Catalans
    fitxerInforme <- paste0("informe-", divisions$regio, sufixFitxers)
  } else if (nrow(divisions) == 1) { # PPCC
    fitxerInforme <- paste0("informe-PPCC", sufixFitxers)
  } else {
    warning("Divisió desconeguda. Corregiu «R/informesPPCC.R».")
    fitxerInforme <- paste0("informe-", divisions$`name:ca`, sufixFitxers)
  }

  if (missing(arrelProjecte)) {
    camins <- file.path("informes", paste0(fitxerInforme, ".", format))
  } else {
    camins <- file.path(arrelProjecte, "informes", paste0(fitxerInforme, ".", format))
  }

  return(camins)
}
