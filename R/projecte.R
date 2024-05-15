# TODO: mirar ?shQuote ?Quotes i r"(text amb " i ' a l'hora)" per generar ordres correctes independentment de si contenen «"'`»

#' obte_camins
#'
#' @param nomFitxer Vector de noms de fitxers. Si no hi ha `arrelProjecte`, conté els camins complets des del projecte.
#' @param arrelProjecte Camí de l'arrel d'un projecte.
#' @param tipus Tipus de fitxers `["informes", "revisions", "edicions"]`.
#'
#' @return Una llista amb `arrelProjecte`, `nomFitxer` i `camins`.
#' @noRd
obte_camins <- function(nomFitxer, arrelProjecte,
                        tipus = c("informes", "revisions", "edicions", "revisions/FET", "edicions/FET")) {
  tipus <- match.arg(tipus)

  if (missing(nomFitxer) && missing(arrelProjecte)) {
    return(list(arrelProjecte = NULL, nomFitxer = NULL, camins = NULL, tipus = tipus))
  }

  if (!missing(nomFitxer)) {
    if (is.character(nomFitxer)) {
      if (missing(arrelProjecte)) {
        arrelProjecte <- gsub(paste0("/+", tipus, "$"), "", dirname(nomFitxer))
        nomFitxer <- basename(nomFitxer)
      } else if (!missing(arrelProjecte)) {
        nomFitxer <- basename(nomFitxer)
      }
    } else { # nomFitxer pot ser una taula o llista de taules
      if (is.data.frame(nomFitxer)) {
        atribut_fitxer <- attr(nomFitxer, "fitxer")
        noms_llista <- NULL
      } else if (is.list(nomFitxer)) {
        atribut_fitxer <- vapply(nomFitxer, attr, which = "fitxer", FUN.VALUE = character(1))
        noms_llista <- names(nomFitxer)
      }
      if (!is.null(atribut_fitxer)) {
        return(obte_camins(nomFitxer = atribut_fitxer, arrelProjecte = arrelProjecte, tipus = tipus))
      } else if (!is.null(noms_llista)) {
        return(obte_camins(nomFitxer = noms_llista, arrelProjecte = arrelProjecte, tipus = tipus))
      } else {
        return(obte_camins(arrelProjecte = arrelProjecte, tipus = tipus))
      }
    }
  } else if (!missing(arrelProjecte)) {
    nomFitxer <- dir(file.path(arrelProjecte, tipus), pattern = "\\.(RData|tsv)$", ignore.case = TRUE)
  }

  ## Normalitza camins per evitar problemes en modificar-los
  arrelProjecte <- gsub("/+$", "", arrelProjecte)
  nomFitxer <- gsub("/+", "/", nomFitxer)

  res <- list(
    arrelProjecte = arrelProjecte,
    nomFitxer = nomFitxer,
    camins = file.path(arrelProjecte, tipus, nomFitxer),
    tipus = tipus
  )

  return(res)
}


#' Subdivisions d'àrees amb Overpass # TODO: osmdata
#'
#' Genera consultes per preparar taules amb les subdivisions d'àrees per \href{https://overpass-turbo.eu}{Overpass}.
#' Utilitzat per dividir països en àrees més manejables i per evitar conjunts de canvis massa grans a l'hora d'afegir exotopònims.
#'
#' @param area filtre de l'àrea per la que es vol generar les subdivisions.
#' @param filtreSubdivisions filtre per identificar les relacions que defineixen les subdivisions.
#' @param etiquetes etiquetes a afegir com a columnes a la sortida d'Overpass en el format \code{csv}.
#' @param format si és \code{csv}, la consulta retorna dades en aquest format. Si és \code{json}, mostra les divisions al mapa.
#'
#' @return Consulta per executar a \href{https://overpass-turbo.eu}{Overpass}.
#' @export
#'
#' @examples
#' consulta<- subdivisionsConsultaOverpass(area="['name:ca'='Rússia'][admin_level=2]",
#'    filtreSubdivisions="[admin_level=4]",
#'    etiquetes=c("name", "'name:ca'", "'wikidata'", "admin_level"))
#' cat(consulta)
#' consulta<- subdivisionsConsultaOverpass(area="['name:ca'='Rússia'][admin_level=2]",
#'    filtreSubdivisions="[admin_level=4]",
#'    format="json")
#' cat(consulta)
subdivisionsConsultaOverpass<- function(area, filtreSubdivisions, etiquetes=c("name", "'name:ca'", "wikidata"), format=c("csv", "json")){
  format<- match.arg(format)
  filtres<- paste0("area", area, "; relation(area)", filtreSubdivisions)
  if (format == "csv"){
    # https://wiki.openstreetmap.org/wiki/Overpass_API/Overpass_QL#CSV_output_mode
    q<- paste0("[out:csv(::type, ::id, ", paste(etiquetes, collapse=", "), "; true; ';')][timeout:1000]; ",
               filtres, "; out;")
  } else if (format == "json"){
    q<- paste0("[out:json][timeout:1000]; ", filtres, "; out body; >; out skel qt;")
  }

  return(q)
}





#' Recompte de casos dels informes, edicions o ambdós
#'
#' Compta el nombre de casos i casos únics, si tenen informació de wikidata o no
#' i altres detalls. Per les edicions, la informació s'agrega a nivell d'informe,
#' és a dir, si hi ha diversos fitxers d'edicions per un mateix informe, els suma.
#'
#' @param arrelProjecte camí a l'arrel del projecte. Es llegiran tots els fitxers *.tsv de la subcarpeta \code{informes} i/o \code{edicions/FET} si no s'especifiquen altres paràmetres.
#' @param informes vector de caràcters amb els camins a fitxers d'informes.
#' @param edicions vector de caràcters amb els camins a fitxers d'edicions.
#' @param dades un \code{data.frame} amb una columna anomenada \code{informe} (o \code{edicio} per la funció \code{recompteCasosEdicions}) amb els camins als fitxers «.tsv». Si s'especifica, s'ignoren la resta de paràmetres.
#'
#' @return Una taula amb files per cada informe i amb les columnes següents:
#'   \describe{
#'     \item{informe}{camí del fitxer d'informe.}
#'     \item{nObjectes}{nombre d'objectes d'OSM.}
#'     \item{nCasos}{nombre de casos únics de les etiquetes \code{«name»}, \code{«name:ca»}, \code{«alt_name:ca»},
#'  \code{«alt_name»}, \code{«noms_wd»}, \code{«ca.viquipedia»} i \code{«wikidata»}.}
#'     \item{nObjectesNomWikidata}{nombre d'objectes d'OSM amb nom en català a wikidata (columna noms_wd dels informes).}
#'     \item{nCasosNomWikidata}{nombre de casos únics amb nom en català a wikidata (columna noms_wd dels fitxers de revisions).}
#'     \item{revisat}{si existeix o no unfitxer de revisió a la subcarpeta del projecte \code{revisions/FET}).}
#'
#'   \strong{Només per la funció \code{recompteCasos}:}
#'     \item{nObjectesEditat}{nombre d'objectes editats d'OSM.}
#'     \item{nCasosEditat}{nombre de casos únics editats de les etiquetes \code{«name»}, \code{«name:ca»}, \code{«alt_name:ca»},
#'  \code{«alt_name»}, \code{«noms_wd»}, \code{«ca.viquipedia»} i \code{«wikidata»}.}
#'     \item{nObjectesNomWikidataEditat}{nombre d'objectes editats d'OSM amb nom en català a wikidata (columna noms_wd dels informes).}
#'     \item{nCasosNomWikidataEditat}{nombre de casos únics editats amb nom en català a wikidata (columna noms_wd dels fitxers de revisions).}
#' }
#' @name recompteCasos
#
# @examples

NULL


#' @rdname recompteCasos
#' @export
recompteCasosInformes<- function(arrelProjecte, informes, dades){
  if (!missing(dades)){
    if ("informe" %in% names(dades)){
      informes<- dades$informe
    } else {
      stop("«dades» ha de contenir una columna anomenada «informe» amb els camins als fitxers.")
    }
  } else {
    if (missing(informes) & !missing(arrelProjecte)){
      informes<- dir(file.path(arrelProjecte, "informes"), pattern="\\.tsv$", full.names=TRUE)
    } else if (missing(informes) & missing(arrelProjecte)){
      stop("Cal especificar algun paràmetre (dades, informes o arrelProjecte)")
    }
    dades<- data.frame(informe=informes)
  }

  if (nrow(dades) == 0){
    message("No hi ha cap informe.")
    return(data.frame(informe=character(), nObjectes=numeric(), nCasos=numeric(), nObjectesNomWikidata=numeric(), nCasosNomWikidata=numeric(), revisat=logical()))
  }

  dades$informe<- gsub("//", "/", dades$informe)  # Normalitza camins
  dades$nObjectes<- dades$nCasos<- NA_integer_
  pb<- pbapply::timerProgressBar(max=nrow(dades))
  on.exit(close(pb))
  for (i in 1:nrow(dades)){
    objectesOSM<- try(utils::read.table(dades$informe[i], header=TRUE, sep="\t", quote="\"", skip=1, check.names=FALSE, comment.char=""))
    if (inherits(objectesOSM, "data.frame")){
      casosRevisar<- unique(objectesOSM[, c("name", "name:ca", "alt_name:ca", "alt_name", "noms_wd", "ca.viquipedia", "wikidata")])
      dades$nObjectes[i]<- nrow(objectesOSM)
      dades$nCasos[i]<- nrow(casosRevisar)

      if ("noms_wd" %in% names(objectesOSM)){
        if (!"nObjectesNomWikidata" %in% names(dades)) dades$nObjectesNomWikidata<- NA_integer_
        if (!"nCasosNomWikidata" %in% names(dades)) dades$nCasosNomWikidata<- NA_integer_
        dades$nObjectesNomWikidata[i]<- sum(!objectesOSM$noms_wd %in% c(NA, ""))
        dades$nCasosNomWikidata[i]<- sum(!casosRevisar$noms_wd %in% c(NA, ""))
      }

      fitxerRevisio<- gsub("^informe-", "revisio-", basename(gsub("(_v[0-9]+)*\\.tsv$", "", dades$informe[i])))
      camiRevisions<- gsub("informes", "revisions/FET", dirname(dades$informe[i]), "FET")
      if (!"revisat" %in% names(dades)) dades$revisat<- NA
      dades$revisat[i]<- any(grepl(fitxerRevisio, dir(camiRevisions)) | nrow(objectesOSM) == 0)
    } else{
      warning("Error a l'informe", i, ":", dades$informe[i], "\n")
      print(objectesOSM)
    }
    pbapply::setTimerProgressBar(pb, i)
  }

  return(dades)
}


#' @rdname recompteCasos
#' @export
recompteCasosEdicions<- function(arrelProjecte, edicions, dades){
  if (!missing(dades)){
    if ("edicio" %in% names(dades)){
      edicions<- dades$edicio
    } else {
      stop("«dades» ha de contenir una columna anomenada «edicio» amb els camins als fitxers.")
    }
  } else {
    if (missing(edicions) & !missing(arrelProjecte)){
      edicions<- dir(file.path(arrelProjecte, "edicions", "FET"), pattern="\\.tsv$", full.names=TRUE)
    } else if (missing(edicions) & missing(arrelProjecte)){
      stop("Cal especificar algun paràmetre (dades, edicions o arrelProjecte)")
    }
    dades<- data.frame(edicio=edicions)
  }

  if (nrow(dades) == 0){
    message("No hi ha cap edicio.")
    dades<- data.frame(informe=character(), nObjectes=numeric(), nCasos=numeric(),
                       nObjectesNomWikidata=numeric(), nCasosNomWikidata=numeric())

    return(dades)
  }

  dades$edicio<- gsub("//", "/", dades$edicio)  # Normalitza camins

  dades$nObjectes<- dades$nCasos<- NA_integer_
  pb<- pbapply::timerProgressBar(max=nrow(dades))
  on.exit(close(pb))
  for (i in 1:nrow(dades)){
    objectesOSM<- try(utils::read.table(dades$edicio[i], header=TRUE, sep="\t", quote="\"", skip=1, check.names=FALSE, comment.char=""))
    if (inherits(objectesOSM, "data.frame")){
      casosEditats<- unique(objectesOSM[, c("name", "name:ca", "alt_name:ca", "alt_name", "noms_wd", "ca.viquipedia", "wikidata")])
      dades$nObjectes[i]<- nrow(objectesOSM)
      dades$nCasos[i]<- nrow(casosEditats)

      if ("noms_wd" %in% names(objectesOSM)){
        if (!"nObjectesNomWikidata" %in% names(dades)) dades$nObjectesNomWikidata<- NA_integer_
        if (!"nCasosNomWikidata" %in% names(dades)) dades$nCasosNomWikidata<- NA_integer_
        dades$nObjectesNomWikidata[i]<- sum(!objectesOSM$noms_wd %in% c(NA, ""))
        dades$nCasosNomWikidata[i]<- sum(!casosEditats$noms_wd %in% c(NA, ""))
      } else {
        # dades$nObjectesNomWikidata[i]<- 0
        # dades$nCasosNomWikidata[i]<- 0
      }

      fitxerInforme<- paste0(basename(gsub("(_v[0-9]+)*\\.tsv$", "", dades$edicio[i])), ".tsv")
      camiInformes<- gsub("edicions/FET", "informes", dirname(dades$edicio[i]), "FET")
      dades$informe[i]<- file.path(camiInformes, fitxerInforme)
    } else{
      warning("Error a l'edicio", i, ":", dades$edicio[i], "\n")
      print(objectesOSM)
    }
    pbapply::setTimerProgressBar(pb, i)
  }

  dadesResum<- by(dades, dades$informe, function(x){
    out<- unique(x[, "informe", drop=FALSE])
    out<- cbind(out, t(colSums(x[, sapply(x, is.numeric)])))
  })
  dadesResum<- do.call(rbind, dadesResum)

  return(dadesResum)
}


#' @rdname recompteCasos
#' @export
recompteCasos<- function(arrelProjecte, informes, dades){
  message("Recompte els casos dels informes...")
  casosInformes<- recompteCasosInformes(arrelProjecte=arrelProjecte, informes=informes, dades=dades)

  if (missing(arrelProjecte)){
    arrelProjecte<- gsub("/informes", "", unique(dirname(casosInformes$informe)))
  }
  message("Recompte els casos de les edicions...")
  casosEdicions<- recompteCasosEdicions(arrelProjecte=arrelProjecte)
  names(casosEdicions)[-which(names(casosEdicions) == "informe")]<- paste0(setdiff(names(casosEdicions), "informe"), "Editat")

  # Unifica els noms dels fitxers d'edicions amb els dels informes.
  # ULL VIU: Omet els fitxers d'edicions que no contenen la part del nom de l'informe un cop extreta l'extensió
  casosEdicionsInforme<- lapply(casosInformes$informe, function(x){
                           sel<- grep(gsub("\\.tsv", "", x), casosEdicions$informe)
                           data.frame(informe=x, t(colSums(casosEdicions[sel, setdiff(names(casosEdicions), "informe")])))
                         })
  casosEdicionsInforme<- do.call(rbind, casosEdicionsInforme)
  casos<- merge(casosInformes, casosEdicionsInforme, all=TRUE)
  casos[]<- lapply(casos, function(x){
    if (is.numeric(x)){
      x[is.na(x)]<- 0
    }
    x
  })
  return(casos)
}


