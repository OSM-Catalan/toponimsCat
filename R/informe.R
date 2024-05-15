#' Obté informe
#'
#' Genera un informe amb tots els objectes d'OSM segons l'àrea i els filtres especificats.
#'
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació serà la subcarpeta `informes`.
#' @param fitxerInforme Nom de fitxer de l'informe (amb extensió `.RData` o `.tsv`). Camí absolut si no hi ha el
#'   paràmetre `arrelProjecte`.
#' @param consulta Consulta d'Overpass com a cadena.
#' @param actualitzaInforme si és `TRUE` i ja existeix el fitxer d'informe, el mou a la carpeta «ANTIC».
#' @param consulta_wikidata Si és `TRUE`, afegeix columnes amb informació extreta de wikidata (vegeu
#'   [afegeix_dades_wikidata()]).
#' @param verbose Si és `TRUE`, treu missatges si el fitxer ja existeix.
#'
#' @return Retorna un `data.frame` amb les dades de l'informe i escriu el resultat a `fitxerInforme` si no existeix el
#'   fitxer o `actualitzaInforme`.
#' @export
#'
#' @examples
#' consulta <- osmdata::opq(
#'   bbox = "relation(id:190090)",
#'   osm_types = "nwr",
#'   out = "tags",
#'   timeout = 1000
#' ) |>
#'   osmdata::add_osm_features(features = filtres_osm_cat(), value_exact = FALSE) |>
#'   osmdata::add_osm_feature(key = c("name", "!name:ca", "wikidata")) |>
#'   osmdata::opq_string()
#' informe <- obte_informe(
#'   arrelProjecte = "exotopònims/Rússia/",
#'   fitxerInforme = "informe-Rússia",
#'   consulta = consulta
#' )
obte_informe <- function(arrelProjecte, fitxerInforme, consulta,
                         actualitzaInforme = FALSE, consulta_wikidata = TRUE, verbose = FALSE) {
  camins <- obte_camins(nomFitxer = fitxerInforme, arrelProjecte = arrelProjecte, tipus = "informes")

  # Selecciona informes i mou els fitxers vells a la carpeta "$arrelProjecte/ANTIC
  actualitza <- FALSE
  if (!missing(fitxerInforme) && file.exists(camins$camins)) {
    if (verbose) message("El fitxer «", camins$camins, "» ja existeix. ", appendLF = FALSE)
    informe <- carrega_informe(camins$camins)

    if (actualitzaInforme) {
      if (verbose) message("\tn casos:", nrow(informe))
      if (nrow(informe) > 0) {
        if (verbose) message("Movent a ", gsub("/+informes/+", "/ANTIC/", fitxerInforme))
        if (!is.null(camins$arrelProjecte)) {
          dir.create(file.path(camins$arrelProjecte, "ANTIC/"), showWarnings = FALSE, recursive = TRUE)
        }
        file.rename(camins$camins, gsub("/+informes/+", "/ANTIC/", camins$camins))
        actualitza <- TRUE
      }
    } else {
      if (verbose) message("Afegiu \"actualitzaInforme=TRUE\" per actualitzar-lo.")
    }
  } else {
    actualitza <- TRUE
  }

  if (actualitza) {
    informe <- osmdata::osmdata_data_frame(consulta)
    if (consulta_wikidata) {
      informe <- afegeix_dades_wikidata(informe)
    }
    if (!missing(fitxerInforme)) {
      dir.create(dirname(camins$camins), showWarnings = FALSE, recursive = TRUE)
      desa_informe(informe = informe, fitxerInforme = camins$camins)
      attr(informe, which = "fitxer") <- camins$camins
    }
  }

  return(informe)
}


desa_informe <- function(informe, fitxerInforme) {
  format <- tolower(gsub("^.+\\.(RData|tsv)$", "\\1", fitxerInforme, ignore.case = TRUE))

  if (format == "tsv") {
    desa_informe_tsv(informe = informe, fitxerInforme = fitxerInforme)
  } else if (format == "rdata") {
    save(informe, file = fitxerInforme, compress = "xz")
  } else {
    warning(
      "L'extensió del fitxer no permet identificar els formats implementats (.RData o .tsv). Es desara com a RData."
    )
    save(informe, file = paste0(fitxerInforme, ".RData"), compress = "xz")
  }
}


desa_informe_tsv <- function(informe, fitxerInforme) {
  atri <- attributes(informe)
  if (all(c("meta", "overpass_call") %in% names(atri))) {
    comentari <- paste0(
      '# "Actualització de ', attr(informe, "meta")$timestamp,
      '"\t"consulta: ', gsub("\\n", "", attr(informe, "overpass_call")), '"'
    )
    informe <- osmdata_a_topoCat(informe)
  } else if ("comentari" %in% names(atri)) {
    comentari <- atri$comentari
  }

  cat(comentari, "\n", file = fitxerInforme, sep = "")
  suppressWarnings(
    utils::write.table(informe, file = fitxerInforme, append = TRUE, sep = "\t", na = "", row.names = FALSE)
  )
}


carrega_informe <- function(fitxerInforme) {
  format <- tolower(gsub("^.+\\.(RData|tsv)$", "\\1", fitxerInforme, ignore.case = TRUE))

  if (format == "rdata") {
    load(file = fitxerInforme)
  } else if (format == "tsv") {
    informe <- carrega_informe_tsv(fitxerInforme = fitxerInforme)
  }

  attr(informe, "fitxer") <- fitxerInforme

  return(informe)
}


carrega_informe_tsv <- function(fitxerInforme) {
  # Intercepta «Warning: EOF within quoted string», que descarta fitxersInformes i retorna el fitxer problemàtic
  informe <- tryCatch(
    utils::read.table(
      fitxerInforme,
      header = TRUE, sep = "\t", quote = "\"", skip = 1,
      check.names = FALSE, comment.char = "", encoding = "utf8"
    ),
    warning = function(w) list(warning = w, fitxer = fitxerInforme)
  )
  if (!inherits(informe, "data.frame")) {
    # Llença l'alerta però llegeix el què es pugui i segueix.
    warning(
      informe$warning, " a ", informe$fitxer, " \n Obrint el document amb LibreOffice Calc i desant-lo editant ",
      "la configuració del filtre a Delimitador de camps={Tabulació}, Delimitador de cadenes de caràcter=\", i ",
      "activant Posa les cadenes de text entre cometes."
    )
    informe <- suppressWarnings(utils::read.table(
      fitxerInforme,
      header = TRUE, sep = "\t", quote = "\"", skip = 1,
      check.names = FALSE, comment.char = "", encoding = "utf8"
    ))
  }

  attr(informe, "comentari") <- readLines(fitxerInforme, n = 1, encoding = "utf8")
  return(informe)
}


osmdata_a_topoCat <- function(informe, tags_character = TRUE) {
  names(informe) <- gsub("^osm_type$", "type", names(informe))
  names(informe) <- gsub("^osm_id$", "id", names(informe))
  etiquetes_agrupades <- setdiff(
    names(informe),
    c("type", "id", "name", "name:ca", "alt_name", "alt_name:ca", "noms_osm", "noms_wd", "wikidata", "wikidata_tipus")
  )

  cols_noms <- c(
    grep("^name:", names(informe), value = TRUE),
    intersect(names(informe), c("int_name", "loc_name", "short_name", "official_name"))
  )
  informe$noms_osm <- apply(informe[, cols_noms, drop = FALSE], 1, function(x) {
    x <- stats::na.omit(x)
    if (length(x) > 0) {
      paste(paste0(names(x), "=", x), collapse = "; ")
    } else {
      NA_character_
    }
  })

  informe_topoCat <- osmapiR::osmapi_objects(informe, tag_columns = etiquetes_agrupades)
  informe_topoCat$members <- NULL
  if (tags_character) {
    informe_topoCat$tags <- vapply(informe_topoCat$tags, function(x) {
      paste(paste0(x$key, "=", x$value), collapse = "; ")
    }, FUN.VALUE = character(1))
    class(informe_topoCat) <- "data.frame"
  }

  ord <- c(
    "type", "id", "name", "name:ca", "alt_name", "alt_name:ca", "noms_osm",
    "noms_wd", "ca.viquipedia", "wikidata_tipus", "wikidata", "tags"
  )
  ord <- intersect(ord, names(informe_topoCat))
  extra <- setdiff(names(informe_topoCat), ord)

  return(informe_topoCat[, c(ord, extra)])
}


#' Descarta objectes sense traduccions dels informes
#'
#' Sobreescriu els informes descarant-ne els objectes (files) sense traduccions de wikidata.
#'
#' @param informes Camins dels informes o una llista amb data.frames d'informes.
#'
#' @return Retorna invisiblement els camins dels informes modificats.
#' @export
#
# @examples
descarta_objectes_sense_traduccions <- function(informes) UseMethod("descarta_objectes_sense_traduccions")


#' @export
descarta_objectes_sense_traduccions.character <- function(informes) {
  informes_actualitzats <- pbapply::pbvapply(informes, function(x) {
    informe <- carrega_informe(x)
    if (!"noms_wd" %in% names(informe)) {
      warning("L'informe no conté cap columna `noms_wd`. S'omet: ", x)
      return(NA_character_)
    }

    nObjectes <- nrow(informe)
    informe <- informe[!informe$noms_wd %in% c(NA, ""), ]
    if (isTRUE(nObjectes == nrow(informe))) { # Si no hi ha canvis al fitxer, no reescriguis
      return(NA_character_)
    }
    desa_informe(informe = informe, fitxerInforme = x)
    return(x)
  }, FUN.VALUE = character(1))

  return(stats::na.omit(informes_actualitzats))
}


#' @export
descarta_objectes_sense_traduccions.list <- function(informes) {
  informes_actualitzats <- pbapply::pblapply(informes, function(x) {
    if (!"noms_wd" %in% names(x)) {
      warning("L'informe no conté cap columna `noms_wd`. S'omet: ", x)
      return(x)
    }

    x[!x$noms_wd %in% c(NA, ""), ]
  })

  return(informes_actualitzats)
}
