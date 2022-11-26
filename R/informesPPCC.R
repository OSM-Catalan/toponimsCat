#' Genera informes pels  PPCC
#'
#' Generar informes per territoris, comarques o municipis dels Països Catalans amb tots els objectes d'OSM segons els filtres especificats. Només inclou els municipis i zones catalanoparlants de les comarques bilingües.
#'
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació dels informes serà la subcarpeta \code{informes}.
#' @param filtre filtre d'etiquetes d'objectes d'OSM per la consulta d'Overpass.
#' @param actualitzaInformes si és \code{TRUE} i ja existeix el fitxer d'informe, el mou a la carpeta «ANTIC».
#' @param sufixFitxers text afegir com a sufix al nom dels fitxers dels informes («arrelProjecte/informe-Regio-comarca$sufixFitxer$.tsv»).
#' @param divisions \code{data.frame} amb informació dels \code{\link{PPCC}}, \code{\link{territoris}}, \code{\link{comarques}} o \code{\link{municipis}}. Per defecte, comarques.
#' @param coordenades si és \code{TRUE}, els informes inclouen les columnes \code{latitude} i \code{longitude}. Per defecte, \code{FALSE}.
#'
#' @return Retorna la taula de les divisions amb els nous camps \code{cmd}, que conté l'ordre per generar els informes amb
#'  \code{write_osm_objects_report} de \href{https://github.com/OSM-Catalan/LangToolsOSM}{LangToolsOSM},
#'  i el camp \code{informe}, que conté el camí del fitxer de l'informe que es crearà.
#' @export
#'
#' @examples
#' ordres<- generaInformesPPCC(arrelProjecte="PPCC/calle-carrer",
#'                             filtre="nwr[name~'^[Cc]alle'][!'name:ca']",
#'                             sufixFitxers="_name-calle",
#'                             divisions=comarques[!comarques$regio %in% c("CatNord", "Sardenya"), ])
#' \dontrun{
#' # Crida les ordres de LantToolsOSM (cal que estigui instal·lat a l'entorn Python configurat a R)
#'   for (i in 1:length(cmd)){
#'     message(i, " / ", length(cmd), "\t", cmd[i])
#'     system(ordres)
#'   }
#' }
generaInformesPPCC<- function(arrelProjecte, filtre, actualitzaInformes=FALSE,
                              sufixFitxers="", divisions=toponimsCat::comarques,
                              coordenades=FALSE){
  for (i in 1:nrow(divisions)){
    if ("admin_level" %in% names(divisions)){
      fitxerInforme<- paste0("informe-", divisions$regio[i], "-", divisions$`name:ca`[i], sufixFitxers, ".tsv")
    } else if ("regio" %in% names(divisions)){  ## territoris dels Països Catalans
      fitxerInforme<- paste0("informe-", divisions$regio[i], sufixFitxers, ".tsv")
    } else if (nrow(divisions) == 1){ # PPCC
      fitxerInforme<- paste0("informe-PPCC", sufixFitxers, ".tsv")
    } else{
      warning("Divisió desconeguda. Corregiu «R/informesPPCC.R».")
      fitxerInforme<- paste0("informe-", divisions$`name:ca`[i], sufixFitxers, ".tsv")
    }
    divisions$informe[i]<- file.path(arrelProjecte, "informes", fitxerInforme)

    areaDivisio<- paste0("rel(", divisions$id[i], ");map_to_area->.divisio; ")
    if ("parcial" %in% names(divisions) && divisions$parcial[i]){  # comarques parcials
      consulta<- paste0("\"[out:json][timeout:1000]; ",
                        areaDivisio,
                        "area[name='Països Catalans']->.llengua; ",
                        filtre, "(area.divisio)(area.llengua); ")
    } else {
      consulta<- paste0("\"[out:json][timeout:1000]; ",
                        areaDivisio,
                        filtre, "(area.divisio);")
    }
    if (coordenades){
      consulta<- paste(consulta, "out center tags qt;\"")
    } else {
      consulta<- paste(consulta, "out tags qt;\"")
    }
    cmd<- generaInforme(arrelProjecte=arrelProjecte, fitxerInforme=fitxerInforme,
                        consulta=consulta, actualitzaFitxer=actualitzaInformes, coordenades=coordenades)

    if (length(cmd) == 0){
      cmd<- ""
    }

    divisions$cmd[i]<- cmd
  }

  return(divisions)
}
