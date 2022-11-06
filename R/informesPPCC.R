#' Generar informes per comarques o municipis dels PPCC amb tots els objectes d'OSM segons els filtres especificats. Només inclou els municipis i zones catalanoparlants de les comarques bilingües.
#'
#'
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació dels informes serà la subcarpeta \code{informes}.
#' @param filtre filtre d'etiquetes d'objectes d'OSM per la consulta d'Overpass.
#' @param actualitzaInformes si és \code{TRUE} i ja existeix el fitxer d'informe, el mou a la carpeta «ANTIC».
#' @param sufixFitxers text afegir com a sufix al nom dels fitxers dels informes («arrelProjecte/informe-Regio-comarca$sufixFitxer$.tsv»).
#' @param divisions \code{data.frame} amb informació de les \code{\link{comarques}} o \code{\link{municipis}}. Per defecte, comarques.
#'
#' @return Retorna la taula de les divisions amb els nous camps \code{cmd}, que conté l'ordre per generar els informes amb
#'  \code{write_osm_objects_report} de \href{https://github.com/OSM-Catalan/LangToolsOSM}{LangToolsOSM},
#'  i el camp \code{informe}, que conté el camí del fitxer de l'informe que es crearà.
#' @export
#'
#' @examples
#' ordres<- generaInformesPPCC(arrelProjecte="PPCC/calle-carrer",
#'                             filtre="nwr[name~'^[Cc]alle'][!'name:ca']",
#'                             sufixFitxers="_name-calle")
#' \dontrun{
#' # Crida les ordres de LantToolsOSM (cal que estigui instal·lat a l'entorn Python configurat a R)
#'   for (i in 1:length(cmd)){
#'     message(i, " / ", length(cmd), "\t", cmd[i])
#'     system(ordres)
#'   }
#' }
generaInformesPPCC<- function(arrelProjecte, filtre, actualitzaInformes=FALSE,
                            sufixFitxers="", divisions=toponimsCat::comarques){
  for (i in 1:nrow(divisions)){
    fitxerInforme<- paste0("informe-", divisions$regio[i], "-", divisions$`name:ca`[i], sufixFitxers, ".tsv")
    divisions$informe[i]<- file.path(arrelProjecte, "informes", fitxerInforme)

    areaDivisio<- paste0("rel(", divisions$id[i], ");map_to_area->.divisió; ")
    if ("parcial" %in% names(divisions) && divisions$parcial[i]){  # comarques parcials
      consulta<- paste0("\"[out:json][timeout:1000]; ",
                        areaDivisio,
                        "area[name='Països Catalans']->.llengua; ",
                        filtre, "(area.divisio)(area.llengua); ",
                        "out tags qt;\"")
    } else {
      consulta<- paste0("\"[out:json][timeout:1000]; ",
                        areaDivisio,
                        filtre, "(area.divisio);",
                        "out tags qt;\"")
    }
    cmd<- generaInforme(arrelProjecte=arrelProjecte, fitxerInforme=fitxerInforme,
                        consulta=consulta, actualitzaFitxer=actualitzaInformes)

    if (length(cmd) == 0){
      cmd<- ""
    }

    divisions$cmd[i]<- cmd
  }

  return(divisions)
}
