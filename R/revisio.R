# informes<- dir(paste0(arrelProjecte, "informe"), "\\.tsv$", full.names=TRUE, include.dirs=FALSE)
# TODO: parametre format = c("RData", "tsv)

#' Genera fitxers de revisió d'informes
#'
#' Els fitxers de revisió d'informes contenen els casos a revisar amb combinacions úniques de
#' les etiquetes `name`, `name:ca`, `alt_name:ca`, `alt_name`,
#' `noms_wd`, `ca.viquipedia` i `wikidata`.
#' Cal corregir els casos de `name:ca` i `alt_name:ca` incorrectes i esborrar-los o deixar-los en blanc si no és clar. Un cop revisat,
#' moveu els fitxers de revisions a «arrelProjecte/revisio/FET/» i prepareu les edicions a OSM amb la funció [preparaEdicions()].
#'
#' @param informes camí dels informes pels que es vol generar fitxers de revisió.
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació serà la subcarpeta `revisions`.
#' @param cerca expressió regular del patró de cerca (\link{regex}).
#' @param substitueix text a substituir.
#' @param ometSenseTraduccions si és `TRUE`, descarta els objectes sense nom en català a wikidata.
#' @param revisioUnificada si és `TRUE`, genera un unic fitxer de revisió. Sinó, genera un fixer de revisió per cada informe.
#' @param nomFitxerUnificat nom del fitxer de revisió unificat quan `revisioUnificada=TRUE`. Si `revisioUnificada=FALSE`, els fitxers de revisió seran `gsub("^informe", "revisio", informes)`.
#' @param filtres una llista amb els elements amb noms de columnes dels informes que serveixen per filtrar casos. Cada element és una funció que retorna un vector amb `FALSE` o `TRUE` per cada element de la columna que es passa com a paràmetre.
#' @param campsUnics camps que defineixen combinacions úniques dels informes que formaran els fitxers de revisió. Per defecte `name`, `name:ca`, `alt_name:ca`, `alt_name`, `noms_wd`, `ca.viquipedia`, `wikidata_tipus`, `wikidata` si existeixen.
#'
#' @return Retorna els camins dels fitxers de revisió generats.
#' @export
#' @seealso \link{gsub}
#' @name genera_revisions
#'
#' @examples
#' genera_revisions_regex_name(
#'   informes = dir("PPCC/calle-carrer/informes", "\\.tsv$", full.names = TRUE),
#'   arrelProjecte = "PPCC/calle-carrer",
#'   cerca = "([Cc])alle ",
#'   substitueix = "\\1arrer ", revisioUnificada = FALSE
#' )
#'
#' genera_revisions_regex_nomsWD(
#'   informes = dir("exotopònims/Grècia/informes", "\\.tsv$",
#'     full.names = TRUE
#'   ),
#'   arrelProjecte = "exotopònims/Grècia",
#'   cerca = " \\(.+\\)",
#'   substitueix = "", revisioUnificada = FALSE
#' )
genera_revisions <- function(informes, arrelProjecte, filtres,
                             campsUnics = c(
                               "name", "name:ca", "alt_name:ca", "alt_name", "noms_wd",
                               "ca.viquipedia", "wikidata_tipus", "wikidata"
                             )) {
  UseMethod("genera_revisions")
}


#' @export
genera_revisions.character <- function(informes, arrelProjecte, filtres,
                                       campsUnics = c(
                                         "name", "name:ca", "alt_name:ca", "alt_name", "noms_wd",
                                         "ca.viquipedia", "wikidata_tipus", "wikidata"
                                       )) {
  camins <- obte_camins(nomFitxer = informes, arrelProjecte = arrelProjecte, tipus = "informes")

  nomRevisions <- gsub("^informe", "revisio", camins$nomFitxer)
  fitxerRevisio <- file.path(camins$arrelProjecte, "revisions", nomRevisions)
  fitxerInforme <- camins$camins

  informes <- pbapply::pblapply(fitxerInforme, carrega_informe)
  revisions <- genera_revisions(informes = informes, filtres = filtres, campsUnics = campsUnics)
  names(revisions) <- nomRevisions

  dir.create(file.path(camins$arrelProjecte, "revisions", "FET"), showWarnings = FALSE, recursive = TRUE)
  # TODO: Variable per indicar si escriure o no (per crides des de genera_revisions_regex_*)
  # mapply(function(revisio, fitxer) {
  #   if (nrow(revisio) > 0) {
  #     if (!revisioUnificada) {
  #       desa_revisio(revisio = revisio, fitxerRevisio = fitxer)
  #     }
  #   } else {
  #     suppressWarnings(file.remove(fitxer))
  #   }
  # }, revisio = revisions, fitxer =  fitxerRevisio)

  return(revisions)
}


#' @export
genera_revisions.list <- function(informes, arrelProjecte, filtres,
                                  campsUnics = c(
                                    "name", "name:ca", "alt_name:ca", "alt_name", "noms_wd",
                                    "ca.viquipedia", "wikidata_tipus", "wikidata"
                                  )) {
  res <- lapply(informes, genera_revisio, filtres = filtres, campsUnics = campsUnics)
  names(res) <- gsub("^informe", "revisio", names(res))
  names(res) <- gsub("/+informes/+informe-", "/revisions/revisio-", names(res))
  names(res) <- gsub("/+informes/+", "/revisions/", names(res))

  return(res)
}


#' @name genera_revisions
#' @export
genera_revisio <- function(informe, filtres,
                           campsUnics = c(
                             "name", "name:ca", "alt_name:ca", "alt_name", "noms_wd",
                             "ca.viquipedia", "wikidata_tipus", "wikidata"
                           )) {
  stopifnot(inherits(informe, "data.frame"))

  if (!missing(filtres)) {
    sel <- mapply(function(filtre, columna) {
      filtre(informe[, columna])
    }, filtre = filtres, columna = names(filtres), SIMPLIFY = FALSE)
    sel <- list2DF(sel)
    informe <- informe[apply(sel, 1, all), ]
  }

  atribut_fitxer <- attr(informe, which = "fitxer")
  atribut_fitxer <- gsub("/+informes/+", "/revisions/", atribut_fitxer)
  atribut_fitxer <- gsub("informe-", "revisio-", atribut_fitxer)

  if (!inherits(informe, "data.frame")) {
    return(NA_character_)
  }
  if (nrow(informe) == 0) {
    revisio <- list2DF(stats::setNames(vector("list", length(campsUnics)), campsUnics))
    attr(revisio, which = "fitxer") <- atribut_fitxer

    return(revisio)
  }

  revisio <- lapply(informe[, intersect(campsUnics, names(informe))], function(x) {
    x[x %in% ""] <- NA_character_
    x
  })
  revisio <- unique(data.frame(revisio, check.names = FALSE))
  attr(revisio, which = "fitxer") <- atribut_fitxer

  return(revisio)
}


#' @rdname genera_revisions
#' @export
genera_revisions_regex_name <- function(informes, arrelProjecte, cerca, substitueix,
                                        revisioUnificada = FALSE, nomFitxerUnificat = "revisio-UNIFICADA.tsv", filtres,
                                        campsUnics = c(
                                          "name", "name:ca", "alt_name:ca", "alt_name", "noms_wd",
                                          "ca.viquipedia", "wikidata_tipus", "wikidata"
                                        )) {
  camins <- obte_camins(nomFitxer = informes, arrelProjecte = arrelProjecte, tipus = "informes")
  message("S'estan carregant els informes...")
  if (!missing(informes)) {
    dL <- genera_revisions(
      informes = informes, arrelProjecte = arrelProjecte, filtres = filtres, campsUnics = campsUnics
    )
  } else {
    dL <- genera_revisions(
      informes = camins$nomFitxer, arrelProjecte = arrelProjecte, filtres = filtres, campsUnics = campsUnics
    )
  }
  message("Preparant els fitxers de revisions...")
  if (length(dL) == 0) {
    warning("No hi ha informes")
    return(NA_character_)
  }

  dL <- lapply(dL, function(d) {
    if (!"name:ca" %in% names(d)) {
      d$`name:ca` <- NA_character_
    }
    d$`name:ca` <- ifelse(is.na(d$`name:ca`), gsub(cerca, substitueix, d$name), d$`name:ca`)
    if (!"alt_name:ca" %in% names(d)) {
      d$`alt_name:ca` <- NA_character_
    }
    d$`alt_name:ca` <- ifelse(is.na(d$`alt_name:ca`), gsub(cerca, substitueix, d$alt_name), d$`alt_name:ca`)
    d
  })

  if (revisioUnificada) {
    dir.create(file.path(camins$arrelProjecte, "revisions", "FET"), showWarnings = FALSE, recursive = TRUE)
    dL <- unique(do.call(rbind, dL))
    nomFitxer <- file.path(camins$arrelProjecte, "revisions", nomFitxerUnificat)
    desa_revisio(revisio = dL, fitxerRevisio = nomFitxer)
  } else if (!is.null(camins$camins)) {
    dir.create(file.path(camins$arrelProjecte, "revisions", "FET"), showWarnings = FALSE, recursive = TRUE)
    nomRevisions <- gsub("^informe", "revisio", camins$nomFitxer)
    fitxerRevisio <- file.path(camins$arrelProjecte, "revisions", nomRevisions)
    mapply(function(d, nomFitxer) {
      if (nrow(d) > 0) {
        desa_revisio(revisio = d, fitxerRevisio = nomFitxer)
      } else {
        suppressWarnings(file.remove(nomFitxer))
      }
    }, d = dL, nomFitxer = fitxerRevisio)
  }

  message(
    "\nFET! Reviseu i modifiqueu la revisió de l'informe que vulgueu de ",
    file.path(camins$arrelProjecte, "revisions"), "/.\n",
    "Cal corregir els casos de name:ca i alt_name:ca incorrectes i esborrar-los o deixar-los en blanc si no és clar.\n",
    "Moveu les revisions acabades a ", file.path(camins$arrelProjecte, "revisions", "FET"), "/."
  )

  return(dL)
}


#' @rdname genera_revisions
#' @export
genera_revisions_regex_nomsWD <- function(informes, arrelProjecte, cerca = " \\(.+\\)", substitueix = "",
                                          ometSenseTraduccions = TRUE,
                                          revisioUnificada = FALSE, nomFitxerUnificat = "revisio-UNIFICADA.tsv",
                                          filtres,
                                          campsUnics = c("name", "name:ca", "alt_name:ca", "alt_name", "noms_wd",
                                                         "ca.viquipedia", "wikidata_tipus", "wikidata")) {
  camins <- obte_camins(nomFitxer = informes, arrelProjecte = arrelProjecte, tipus = "informes")
  message("S'estan carregant els informes...")
  if (!missing(informes)) {
    dL <- genera_revisions(
      informes = informes, arrelProjecte = arrelProjecte, filtres = filtres, campsUnics = campsUnics
    )
  } else {
    dL <- genera_revisions(
      informes = camins$nomFitxer, arrelProjecte = arrelProjecte, filtres = filtres, campsUnics = campsUnics
    )
  }
  message("Preparant els fitxers de revisions...")
  if (length(dL) == 0) {
    warning("No hi ha informes")
    return(NA_character_)
  }

  dL <- lapply(dL, function(d) {
    if (ometSenseTraduccions) {
      senseTraduccio <- d$noms_wd %in% c("", NA)
      d <- d[!senseTraduccio, ]
    }
    if (nrow(d) > 0) {
      traduccions <- strsplit(as.character(d$noms_wd), "; ")
      traduccions <- lapply(traduccions, function(x) unique(gsub(cerca, substitueix, x)))
      if (!"name:ca" %in% names(d)) {
        d$`name:ca` <- NA_character_
      }
      d$`name:ca` <- ifelse(
        d$`name:ca` %in% c("", NA),
        vapply(traduccions, function(x) x[1], FUN.VALUE = character(1)),
        d$`name:ca`
      )
      if (!"alt_name:ca" %in% names(d)) {
        d$`alt_name:ca` <- NA_character_
      }
      d$`alt_name:ca` <- ifelse(
        d$`alt_name:ca` %in% c("", NA) & sapply(traduccions, length) > 1,
        vapply(traduccions, function(x) paste(x[-1], collapse = ";"), FUN.VALUE = character(1)),
        d$`alt_name:ca`
      )
    }
    d <- d[, intersect(campsUnics, names(d))]
  })

  nomRevisions <- gsub("^informe", "revisio", camins$nomFitxer)
  fitxerRevisio <- file.path(camins$arrelProjecte, "revisions", nomRevisions)

  if (revisioUnificada) {
    dir.create(file.path(camins$arrelProjecte, "revisions", "FET"), showWarnings = FALSE, recursive = TRUE)
    dL <- unique(do.call(rbind, dL))
    nomFitxer <- file.path(camins$arrelProjecte, "revisions", nomFitxerUnificat)
    desa_revisio(revisio = dL, fitxerRevisio = nomFitxer)
  } else if (!is.null(camins$camins)) {
    dir.create(file.path(camins$arrelProjecte, "revisions", "FET"), showWarnings = FALSE, recursive = TRUE)
    nomRevisions <- gsub("^informe", "revisio", camins$nomFitxer)
    fitxerRevisio <- file.path(camins$arrelProjecte, "revisions", nomRevisions)
    mapply(function(d, nomFitxer) {
      if (nrow(d) > 0) {
        desa_revisio(revisio = d, fitxerRevisio = nomFitxer)
      } else {
        suppressWarnings(file.remove(nomFitxer))
      }
    }, d = dL, nomFitxer = fitxerRevisio)
  }

  message(
    "\nFET! Reviseu i modifiqueu la revisió de l'informe que vulgueu de ",
    file.path(camins$arrelProjecte, "revisions"), "/.\n",
    "Cal corregir els casos de name:ca i alt_name:ca incorrectes i esborrar-los o deixar-los en blanc si no és clar.\n",
    "Moveu les revisions acabades a ", file.path(camins$arrelProjecte, "revisions", "FET"), "/."
  )

  return(dL)
}


#' Base de dades de revisions
#'
#' Genera revisions per casos que només es diferencien per si els noms comencen amb majúscules o minúscules, mantenint el format original.
#' Alerta amb les reutilitzacions entre projectes, a vegades la concordànça del gènere es perd i cal revisar les edicions (p. ex. calle-carrer VS plaza-plaça)
#'
#' @param arrelProjectes camí a partir del qual es cercaran recursivament les revisions fetes.
#'
#' @return Retorna un \code{data.frame} amb totes les revisions fetes que contenen valors per \code{name:ca} o \code{alt_name:ca}.
#' @export
#'
#' @examples
#' # Reutilitza les revisions pel projecte plaza-plaça
#' bdRevs <- bdRevisions(arrelProjectes = "PPCC")
#' bdRevs[, c("name", "alt_name")] <- lapply(bdRevs[, c("name", "alt_name")], function(x) {
#'   x <- gsub("^(avenida|calle|camino|parque|plaza) ", "plaza ", x)
#'   x <- gsub("^(Avenida|Calle|Camino|Parque|Plaza) ", "Plaza ", x)
#'   x
#' })
#' bdRevs[, c("name:ca", "alt_name:ca")] <- lapply(bdRevs[, c("name:ca", "alt_name:ca")], function(x) {
#'   x <- gsub("^(avinguda|carrer|camí|parc|plaça) ", "plaça ", x)
#'   x <- gsub("^(Avinguda|Carrer|Camí|Parc|Plaça) ", "Plaça ", x)
#'   x
#' })
#'
#' bdRevs <- unique(bdRevs)
#' \dontrun{
#' toponimsCat:::desa_revisio(
#'   revisio = bdRevs,
#'   fitxerRevisio = "PPCC/plaza-plaça/revisions/revisio-PPCC_reutilitzat_name-plaza.tsv"
#' )
#' }
bdRevisions <- function(arrelProjectes) {
  arrelProjectes <- gsub("/+$", "", arrelProjectes) # Normalitza camins per evitar problemes en modificar-los

  # Fusiona revisions fetes amb tots els informes que contenen objectes amb etiquetes iguals
  # fitxersRevisions<- dir(arrelProjectes, pattern=".+/revisions/FET/.+\\.tsv$", recursive=TRUE, full.names=TRUE, include.dirs=FALSE)
  fitxersRevisions <- dir(arrelProjectes, recursive = TRUE, full.names = TRUE, include.dirs = FALSE) # Falla amb pattern="/edicions/FET/.+\\.tsv$",
  fitxersRevisions <- grep("/+revisions/+FET/+.+\\.(RData|tsv)$", fitxersRevisions, value = TRUE)
  if (length(fitxersRevisions) == 0) {
    message(
      "No s'ha trobat cap fitxer de revisions a ", arrelProjectes,
      " (cerca recursiva de fitxers «.tsv» en carpetes «.+/revisions/FET/»)."
    )
    return(character())
  }

  revisionsFETES <- pbapply::pblapply(fitxersRevisions, carrega_revisio)
  names(revisionsFETES) <- fitxersRevisions
  revisio.casosFETS <- do.call(rbind, revisionsFETES)
  revisio.casosFETS <- revisio.casosFETS[!revisio.casosFETS$`name:ca` %in% c(NA, "") | !revisio.casosFETS$`alt_name:ca` %in% c(NA, ""), ]
  row.names(revisio.casosFETS) <- NULL

  if (nrow(revisio.casosFETS) == 0) {
    message("Les revisions fetes no contenen cap cas.")
    return(character())
  }

  ## Genera versions començant per majúscula i per minúscula (útil per reutilitzar revisions p.ex. de «carrer/Carrer»)
  revisio.casosFETS_min <- apply(revisio.casosFETS[, c("name", "name:ca", "alt_name", "alt_name:ca")], 1, function(x) {
    out <- paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    out[is.na(x)] <- NA_character_
    return(out)
  })
  revisio.casosFETS[, c("name", "name:ca", "alt_name", "alt_name:ca")] <- as.data.frame(t(revisio.casosFETS_min))
  revisio.casosFETS_min <- revisio.casosFETS <- unique(revisio.casosFETS)

  revisio.casosFETS_maj <- apply(revisio.casosFETS[, c("name", "name:ca", "alt_name", "alt_name:ca")], 1, function(x) {
    out <- paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    out[is.na(x)] <- NA_character_
    return(out)
  })
  revisio.casosFETS[, c("name", "name:ca", "alt_name", "alt_name:ca")] <- as.data.frame(t(revisio.casosFETS_maj))
  revisio.casosFETS_maj <- revisio.casosFETS
  revisio.casosFETS <- rbind(revisio.casosFETS_min, revisio.casosFETS_maj)

  revisio.casosFETS <- lapply(revisio.casosFETS, function(x) {
    x[x %in% ""] <- NA_character_
    x
  })
  revisio.casosFETS <- unique(data.frame(revisio.casosFETS, check.names = FALSE))

  if (any(dup <- duplicated(revisio.casosFETS[, c("name", "alt_name", "noms_wd", "ca.viquipedia", "wikidata")]))) {
    print(revisio.casosFETS[dup, c("name", "alt_name", "noms_wd", "ca.viquipedia", "wikidata")])
    warning("Hi ha discrepàncies entre fitxers de revisions. Es descartaran les entrades amb valors duplicats de
            «name», «alt_name», «noms_wd», «ca.viquipedia» i «wikidata».")
    dup <- duplicated(pk <- do.call(paste, revisio.casosFETS[, c("name", "alt_name", "noms_wd", "ca.viquipedia", "wikidata")]))
    dupPKstr <- pk[dup]
    revisio.casosFETS <- revisio.casosFETS[!pk %in% dupPKstr, ]
    # TODO: millora indicant els fitxers amb duplicats
  }

  return(revisio.casosFETS)
}
### TODO: reutilitza les revisions fetes i evita duplicats inconsistents
# Descarta casos ja revisats sense name:ca
# setdiff(d[, c("name", "alt_name", "wikidata")], revisio.casosDESCARTATS)
# Casos completats, ja pujats a OSM
# Revisions fetes (name:ca = '' per traduccions descartades)
# dir.create(file.path(arrelProjecte, "revisions", "FET"), showWarnings=FALSE, recursive=TRUE)
# fitxersRevisio<- dir(file.path(arrelProjecte, "revisions", "FET"), "\\.tsv$", full.names=TRUE, include.dirs=FALSE)
# revisionsFETES<- lapply(fitxersRevisio, function(x){
#   d<- utils::read.table(x, header=TRUE, sep="\t", quote="\"", check.names=FALSE, comment.char="")
#   d<- lapply(d, function(x){
#     x[x %in% ""]<- NA_character_
#     x
#   })
#   d<- unique(data.frame(d, check.names=FALSE))
# })
# names(revisionsFETES)<- gsub(file.path(arrelProjecte, "revisions", "FET/"), "", fitxersRevisio)
# revisio.casosFETS<- unique(do.call(rbind, revisionsFETES))
# revisio.casosCOMPLETATS<- revisio.casosFETS[!revisio.casosFETS$`name:ca` %in% c(NA, ""), ]
# revisio.casosDESCARTATS<- revisio.casosFETS[revisio.casosFETS$`name:ca` %in% c(NA, ""), ]


desa_revisio <- function(revisio, fitxerRevisio) {
  format <- tolower(gsub("^.+\\.(RData|tsv)$", "\\1", fitxerRevisio, ignore.case = TRUE))

  if (format == "tsv") {
    desa_revisio_tsv(revisio = revisio, fitxerRevisio = fitxerRevisio)
  } else if (format == "rdata") {
    save(revisio, file = fitxerRevisio, compress = "xz")
  } else {
    warning(
      "L'extensió del fitxer no permet identificar els formats implementats (.RData o .tsv). Es desara com a RData."
    )
    save(revisio, file = paste0(fitxerRevisio, ".RData"), compress = "xz")
  }
}


desa_revisio_tsv <- function(revisio, fitxerRevisio) {
  utils::write.table(revisio, file = fitxerRevisio, sep = "\t", na = "", row.names = FALSE)
}


carrega_revisio <- function(fitxerRevisio) {
  format <- tolower(gsub("^.+\\.(RData|tsv)$", "\\1", fitxerRevisio, ignore.case = TRUE))

  if (format == "rdata") {
    load(file = fitxerRevisio)
  } else if (format == "tsv") {
    revisio <- carrega_revisio_tsv(fitxerRevisio = fitxerRevisio)
  } else {
    warning(
      "L'extensió del fitxer no permet identificar els formats implementats (.RData o .tsv). Es desara com a RData."
    )
  }

  attr(revisio, "fitxer") <- fitxerRevisio

  return(revisio)
}


carrega_revisio_tsv <- function(fitxerRevisio) {
  # Intercepta «Warning: EOF within quoted string», que descarta fitxersRevisions i retorna el fitxer problemàtic
  revisio <- tryCatch(
    utils::read.table(
      fitxerRevisio,
      header = TRUE, sep = "\t", quote = "\"", check.names = FALSE, comment.char = "", encoding = "utf8"
    ),
    warning = function(w) list(warning = w, fitxer = fitxerRevisio)
  )
  if (!inherits(revisio, "data.frame")) {
    # Llença l'alerta però llegeix el què es pugui i segueix.
    warning(
      revisio$warning, " a ", revisio$fitxer, " \n Obrint el document amb LibreOffice Calc i desant-lo editant ",
      "la configuració del filtre a Delimitador de camps={Tabulació}, Delimitador de cadenes de caràcter=\", i ",
      "activant Posa les cadenes de text entre cometes."
    )
    revisio <- suppressWarnings(utils::read.table(
      fitxerRevisio,
      header = TRUE, sep = "\t", quote = "\"", check.names = FALSE, comment.char = "", encoding = "utf8"
    ))
  }

  return(revisio)
}
