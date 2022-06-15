#' Generar informes per les zones catalanoparlants de les comarques dels PPCC.
#'
#'
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació dels informes serà la subcarpeta \code{informes}.
#' @param filtre filtre d'etiquetes d'objectes d'OSM per la consulta d'Overpass.
#' @param actualitzaInformes si és \code{TRUE} i ja existeix el fitxer d'informe, el mou a la carpeta «ANTIC».
#' @param sufixFitxers text afegir com a sufix al nom dels fitxers dels informes («arrelProjecte/informe-Regio-comarca$sufixFitxer$.tsv»)
#' @param comarques \code{data.frame} amb informació de les comarques. Per defecte, \code{\link{comarques}}
#'
#' @return Retorna la taula de les comarques amb els nous camps \code{cmd}, que conté l'ordre per generar els informes amb LangTools,
#' i el camp \code{informe}, que conté el camí del fitxer de l'informe que es crearà.
#' @export
#'
#' @examples
#' ordres<- generaInformesPPCC(arrelProjecte="ppcc/calle-carrer",
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
                            sufixFitxers="", comarques=toponimsCat::comarques){
  for (i in 1:nrow(comarques)){
    if (!is.na(comarques$`historic:admin_level`[i])){
      tipus<- "['historic:admin_level']"
    } else if (!is.na(comarques$admin_level[i])){
      tipus<- paste0("[admin_level=", comarques$admin_level[i], "]")
    }else{
      tipus<- ""
    }
    areaRegio<- paste0("['name:ca'='", gsub("\\'", "\\\\'", comarques$`name:ca`[i]) ,"']", tipus)
    fitxerInforme<- paste0("informe-", comarques$regio[i], "-", comarques$`name:ca`[i], sufixFitxers, ".tsv")
    comarques$informe[i]<- file.path(arrelProjecte, "informes", fitxerInforme)

    if (!comarques$parcial[i]){
      cmd<- generaInforme(arrelProjecte=arrelProjecte, fitxerInforme=fitxerInforme,
                          filtreArea=areaRegio, filtreObjectes=filtre,
                          actualitzaFitxer=actualitzaInformes)
    } else {
      consulta<- paste0("\"[out:json][timeout:1000]; ",
                        "area", areaRegio, "->.regio; ",
                        "area[name='Països Catalans']->.llengua; ",
                        "( ", filtre, "(area.regio)(area.llengua); ); ",
                        "out tags qt;\"")
      cmd<- generaInforme(arrelProjecte=arrelProjecte, fitxerInforme=fitxerInforme,
                          consulta=consulta, actualitzaFitxer=actualitzaInformes)
    }

    if (length(cmd) == 0){
      cmd<- ""
    }

    comarques$cmd[i]<- cmd
  }

  return(comarques)
}
