#' Genera fitxers amb edicions combinant els informes i revisions
#'
#' Fusiona les revisions fetes amb tots els informes que contenen objectes amb etiquetes iguals.
#' Reaprofita les revisions per casos que només es diferencien per si els noms comencen amb majúscules o minúscules,
#' mantenint el format original.
#'
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació serà la subcarpeta `edicions` i
#'   buscarà els informes i revisions a partir d'aquest camí si no hi ha paràmetres `informes` o `revisions`.
#' @param informes Camins dels informes o llista de les taules d'informes.
#' @param revisions Un `data.frame` amb revisions validades o un vector de camins de fitxers de revisions fetes.
#' @param format "RData", "tsv", "osc" o "xml". Serà l'extensió i format del fitxer a escriure.
#'
#' @return Una llista amb taules `osmapi_OsmChange` o `xml_document` per format `osc` o `xml`. Si `informes` conté els
#'   camins dels fitxers d'informe, s'escriuran els fitxers d'edicions a la carpeta `edicions/*.format` del projecte.
#'
#' @export
#
# @examples
prepara_edicions <- function(arrelProjecte, informes, revisions, format = c("RData", "tsv", "osc", "xml")) {
  format <- match.arg(format)
  format_osmapir <- if (format %in% c("RData", "tsv")) {
    "R"
  } else {
    "osc"
  }

  if (missing(revisions) || is.character(revisions)) {
    camins_rev <- obte_camins(nomFitxer = revisions, arrelProjecte = arrelProjecte, tipus = "revisions/FET")
    message("S'està preparant la base de dades de revisions...")
    revisio.casosFETS <- bdRevisions(arrelProjectes = camins_rev$arrelProjecte)
    if (length(revisio.casosFETS) == 0) {
      message("No hi ha revisions a ", paste0(camins_rev$arrelProjecte, "/revisions/FET"))
      return(data.frame())
    }
  } else if (is.data.frame(revisions)) {
    revisio.casosFETS <- revisions
    if (nrow(revisio.casosFETS) == 0) {
      message("No hi ha revisions.")
      return(data.frame())
    }
  }

  message("\nS'estan preparant les edicions...")

  if (missing(informes) || is.character(informes)) {
    camins_inf <- obte_camins(nomFitxer = informes, arrelProjecte = arrelProjecte, tipus = "informes")
    edicions <- prepara_edicions.character(
      informes = camins_inf$camins, revisions = revisio.casosFETS,
      arrelProjecte = camins_inf$arrelProjecte, format = format
    )
  } else {
    if (is.data.frame(informes)) {
      informes <- list(informes)
    }
    if (is.list(informes)) {
      edicions <- prepara_edicions.list(informes = informes, revisions = revisio.casosFETS, format = format)
    } else {
      stop(
        "L'argument `informes` ha de ser un vector de caràcters amb els camins dels fitxers d'informes, ",
        "un `data.frame` o una llista de `data.frames` d'informes."
      )
    }
  }


  message("Edicions a punt! Useu la funció envia_edicions per pujar les edicions a OSM.")

  return(edicions)
}


#' @param informes Camins dels informes.
#' @param revisions Un `data.frame` amb revisions validades.
#' @param arrelProjecte description
#' @param format "RData", "tsv", "osc" o "xml". Serà l'extensió del fitxer a escriure
#'
#' @return una llista amb taules `osmapi_OsmChange` o `xml_document` per format `osc` o `xml`.
#' @noRd
prepara_edicions.character <- function(informes, revisions, arrelProjecte, format = "RData") {
  format_osmapir <- if (format %in% c("RData", "tsv")) {
    "R"
  } else {
    "osc"
  }

  dir.create(file.path(arrelProjecte, "edicions"), showWarnings = FALSE, recursive = TRUE)

  osmchas <- pbapply::pblapply(informes, function(x) {
    informe <- carrega_informe(x)
    osmcha <- prepara_edicio(informe = informe, revisio.casosFETS = revisions, format_osmapir = format_osmapir)
    attr(osmcha, which = "comentari") <- paste0("\"# Edicions per l'informe ", x, "\"\t")

    nomEdicions <- gsub("^informe", "edicio", basename(x))
    nomEdicions <- gsub(paste0("\\.[A-Za-z0-9]+$"), paste0(".", format), nomEdicions) # format com a extensió
    fitxerEdicio <- file.path(gsub("/+informes", "", dirname(x)), "edicions", nomEdicions)

    if (format_osmapir == "R" && nrow(osmcha) > 0 ||
      format_osmapir == "osc" && length(xml2::xml_children(osmcha)) > 0) {
      desa_edicio(edicio = osmcha, fitxerEdicio = fitxerEdicio, format = format)
    } else {
      suppressWarnings(file.remove(fitxerEdicio))
    }

    osmcha
  })

  names(osmchas) <- gsub("^informe", "edicio", basename(informes))
  names(osmchas) <- gsub(paste0("\\.[A-Za-z0-9]+$"), paste0(".", format), names(osmchas))

  return(osmchas)
}


#' @param informes Llista de `data.frame`s amb informes
#' @param revisions Un `data.frame` amb revisions validades.
#' @param format "RData", "tsv", "osc" o "xml".
#'
#' @return una llista amb taules `osmapi_OsmChange` o `xml_document` per format `osc` o `xml`.
#' @noRd
prepara_edicions.list <- function(informes, revisions, format = "RData") {
  format_osmapir <- if (format %in% c("RData", "tsv")) {
    "R"
  } else {
    "osc"
  }

  osmchas <- pbapply::pblapply(informes, function(x) {
    prepara_edicio(informe = x, revisio.casosFETS = revisions, format_osmapir = format_osmapir)
  })

  names(osmchas) <- gsub("^informe-", "edicio-", names(informes))

  return(osmchas)
}


#' @param informe Un `data.frame` amb un informe.
#' @param revisio.casosFETS Un `data.frame` amb revisions validades.
#' @param format "R", "osc" o "xml" per passar a [osmapiR::osmchange_modify()].
#'
#' @return una taula `osmapi_OsmChange` o `xml_document` quan `format_osmapir = "osc"`.
#' @noRd
prepara_edicio <- function(informe, revisio.casosFETS, format_osmapir = "R") {
  informe <- lapply(informe, function(x) {
    x[x %in% ""] <- NA_character_
    x
  })
  informe <- data.frame(informe, check.names = FALSE)
  edicio <- merge(informe[, setdiff(names(informe), c("name:ca", "alt_name:ca"))], revisio.casosFETS)
  ordCols <- intersect(
    c(
      "osm_type", "osm_id", "name", "name:ca", "alt_name", "alt_name:ca",
      "noms_wd", "ca.viquipedia", "wikidata_tipus", "wikidata"
    ),
    names(edicio)
  )
  # ordCols <- c(ordCols, setdiff(names(edicio), ordCols))
  edicio <- edicio[, ordCols]
  names(edicio)[1:2] <- c("type", "id")
  edicio <- osmapiR::osmapi_objects(edicio, tag_columns = c("name", "name:ca", "alt_name", "alt_name:ca"))
  osmcha <- osmapiR::osmchange_modify(edicio, tag_keys = c("name:ca", "alt_name:ca"), format = format_osmapir)

  return(osmcha)
}


#' Envia les edicions i actualitza els informes de les edicions carregades
#'
#' Arxiva els fitxers d'edicions a `$arrelProjecte/edicions/FET` i actualitza els informes eliminant els casos ja
#' carregats o eliminant els informes desactualitzats (arxivatz a ``$arrelProjecte/informes/ANTIC``).
#'
#' @param edicions Una llista de taules de canvis o els camins de fitxers que els continguin (`.osc` o `.RData`).
#' @param arrelProjecte  camí a l'arrel del projecte. La carpeta de destinació serà la subcarpeta \code{edicions} i
#'   buscarà els informes i revisions a partir d'aquest camí.
#' @param comentaris Vector de comentaris corresponent a cada conjunt de canvis.
#' @param hashtags Etiquetes dels conjunts de canvis.
#' @param ... Paràmetres per [osmapiR::osm_create_changeset()]. Permet afegir etiquetes als conjunts de canvis
#'   (p.ex. `source`).
#' @param esborraInformesDesactualitzats si és `TRUE`, elimina els informes desactualitzats per tornar-los a generar de
#'   nou.
#' Altrament, elimina els objectes dels informes que han estat actualitzats.
#'
#' @return Camins dels informes actualitzats.
#' @export
#
# @examples
envia_edicions <- function(edicions, arrelProjecte,
                           comentaris, hashtags = "#toponimsCat", ..., esborraInformesDesactualitzats = FALSE) {
  UseMethod("envia_edicions")
}


#' @export
envia_edicions.character <- function(edicions, arrelProjecte,
                                     comentaris, hashtags = "#toponimsCat", ...,
                                     esborraInformesDesactualitzats = FALSE) {
  camins <- obte_camins(nomFitxer = edicions, arrelProjecte = arrelProjecte, tipus = "edicions")
  edicions <- lapply(camins$camins, carrega_edicio)
  if (missing(comentaris)) {
    comentaris <- paste0(
      "Afegeixo etiquetes `name:ca` i `alt_name:ca` pel projecte ",
      gsub("^edicio-|\\.[A-Za-z0-9]+$", "", camins$nomFitxer), "."
    )
  }

  envia_edicions(
    edicions = edicions, arrelProjecte = arrelProjecte,
    comentaris = comentaris, hashtags = "#toponimsCat", ...,
    esborraInformesDesactualitzats = esborraInformesDesactualitzats
  )
}


#' @export
envia_edicions.list <- function(edicions, arrelProjecte,
                                comentaris, hashtags = "#toponimsCat", ...,
                                esborraInformesDesactualitzats = FALSE) {
  camins_ed <- obte_camins(nomFitxer = names(edicions), arrelProjecte = arrelProjecte, tipus = "edicions")
  fitxers_inf <- file.path(camins_ed$arrelProjecte, "informes", gsub("^edicio-", "informe-", camins_ed$nomFitxer))
  fitxers_inf <- lapply(fitxers_inf, function(x) {
    if (file.exists(x)) {
      return(x)
    }
    fitxer <- grep(gsub("\\.[A-Za-z0-9]+$", ".", basename(x)), dir(dirname(x)), value = TRUE)
    if (length(fitxer) == 0) {
      fitxer <- NA_character_
    }
    return(fitxer)
  })

  if (missing(comentaris)) {
    comentaris <- paste0(
      "Afegeixo etiquetes `name:ca` i `alt_name:ca` pel projecte ",
      gsub("^edicio-|\\.[A-Za-z0-9]+$", "", camins_ed$nomFitxer), "."
    )
  }

  dir.create(file.path(camins_ed$arrelProjecte, "edicions", "FET"), showWarnings = FALSE, recursive = TRUE)

  mapply(
    function(edicio, fitxer_ed, fitxer_inf, nom_edicio, comentari) {
      envia_edicio(edicio, comentari = comentari, hashtags = hashtags, ...)

      fitxer_ed_arxivat <- file.path(
        dirname(fitxer_ed), "FET",
        gsub("(\\.[A-Za-z0-9])+$", "_v1\\1", basename(fitxer_ed))
      )
      i <- 1
      while (file.exists(fitxer_ed_arxivat)) {
        fitxer_ed_arxivat <- gsub("_v[0-9]+(\\.[A-Za-z0-9])+$", paste0("_v", i, "\\1"), fitxer_ed_arxivat)
        i <- i + 1
      }
      file.rename(fitxer_ed, fitxer_ed_arxivat)

      out <- paste("INF NO TROBAT:", nom_edicio)
      if (!all(is.na(fitxer_inf))) {
        if (esborraInformesDesactualitzats) {
          file.remove(fitxer_inf)
          out <- paste("ELIMINAT:", fitxers_inf)
        } else {
          actualitza_informe(fitxer_inf = fitxer_inf, edicio = edicio)
          out <- paste("ACTUALITZAT:", fitxers_inf)
        }
      }
      out
    },
    edicio = edicions, fitxer_ed = camins_ed$camins,
    fitxer_inf = fitxers_inf, nom_edicio = names(edicions),
    comentari = comentaris
  )
}


envia_edicio <- function(edicio, comentari, hashtags = "#toponimsCat", ...) {
  changeset_id <- osmapiR::osm_create_changeset(comment = comentari, hashtags = hashtags, ...)
  osmapiR::osm_diff_upload_changeset(changeset_id = changeset_id, osmcha = edicio)
  osmapiR::osm_close_changeset(changeset_id = changeset_id)
}


actualitza_informe <- function(fitxer_inf, edicio) {
  obj_fets <- if (is.data.frame(edicio)) {
    data.frame(type = edicio$type, id = edicio$id)
  } else if (inherits(edicio, "xml_document")) {
    if (!requireNamespace("xml2", quietly = TRUE)) {
      stop("El format d'edicio `osc` o `xml` requereix el paquet xml2. Instal·leu-lo amb\n\tinstall.packages(\"xml2\")")
    }
    canvis <- xml2::xml_children(edicio)
    as.data.frame(t(vapply(canvis, function(x) {
      obj <- xml2::xml_child(x)
      c(type = xml2::xml_name(obj), id = xml2::xml_attr(obj, attr = "id"))
    }, character(2))))
  }

  informe <- carrega_informe(fitxer_inf)
  elimina <- do.call(paste, obj_fets)
  obj_inf <- do.call(paste, informe[, c("osm_type", "osm_id")])
  informe <- informe[!obj_inf %in% elimina, ]
  desa_informe(informe = informe, fitxerInforme = fitxer_inf)

  return(informe)
}

#'
#' Useu la funció després de carregar a OSM els fitxers d'edicions. Arxiva els fitxers d'edicions a «$arrelProjecte/edicions/FET» i
#'  actualitza els informes eliminant els casos ja editats o eliminant els fitxers desactualitzats.
#' Mou els informes actualitzats a edicions/FET i arxiva els informes originals a ANTIC/.
#'
#' @param arrelProjecte  camí a l'arrel del projecte. La carpeta de destinació serà la subcarpeta \code{edicions} i buscarà els informes i revisions a partir d'aquest camí.
#' @param esborraInformesDesactualitzats si és \code{TRUE}, elimina els informes desactualitzats per tornar-los a generar de nou.
#' Altrament, elimina els objectes dels informes que han estat actualitzats.
#'
#' @return Camins dels informes actualitzats.
#' @export
#
# @examples
actualitzaInformesCarregats <- function(arrelProjecte, esborraInformesDesactualitzats = FALSE) {
  fitxersFets <- dir(file.path(arrelProjecte, "edicions"), pattern = "\\.tsv$", full.names = TRUE, include.dirs = FALSE)
  fitxersInformesOri <- gsub("/edicions/", "/informes/", fitxersFets)

  ret <- character()
  pb <- pbapply::timerProgressBar(max = length(fitxersFets))
  on.exit(close(pb))
  for (i in seq_along(fitxersFets)) {
    if (esborraInformesDesactualitzats) {
      suppressWarnings(file.remove(fitxersInformesOri[i]))
      ret <- c(ret, fitxersInformesOri[i])
      next
    }

    if (!file.exists(fitxersInformesOri[i])) {
      next
    }

    informeOri <- utils::read.table(fitxersInformesOri[i], header = TRUE, sep = "\t", quote = "\"", skip = 1, check.names = FALSE)
    carregat <- utils::read.table(fitxersFets[i], header = TRUE, sep = "\t", quote = "\"", skip = 1, check.names = FALSE, comment.char = "")
    informeOri <- data.table::as.data.table(informeOri)
    carregat <- data.table::as.data.table(carregat)
    data.table::set(carregat, j = c("name:ca", "alt_name:ca"), value = list(NA_character_, NA_character_))
    data.table::set(carregat, j = names(carregat), value = lapply(carregat, function(x) {
      x[is.na(x)] <- ""
      as.character(x)
    }))
    data.table::set(informeOri, j = names(informeOri), value = lapply(informeOri, function(x) {
      x[is.na(x)] <- ""
      as.character(x)
    }))
    data.table::setcolorder(carregat, neworder = names(informeOri))

    informeNou <- data.table::fsetdiff(informeOri, carregat)

    if (!data.table::fsetequal(informeNou, informeOri)) {
      comentari <- readLines(fitxersInformesOri[i], n = 1)
      cat(comentari, "\n", file = fitxersInformesOri[i], sep = "")
      suppressWarnings(utils::write.table(informeNou, file = fitxersInformesOri[i], append = TRUE, sep = "\t", na = "", row.names = FALSE))
      ret <- c(ret, fitxersInformesOri[i])
    }
    pbapply::setTimerProgressBar(pb, i)
  }

  arxivats <- gsub("/edicions/", "/edicions/FET/", fitxersFets)
  arxivats <- gsub("\\.tsv$", "_v0.tsv", arxivats)
  i <- 1
  while (any(file.exists(arxivats))) {
    arxivats <- ifelse(file.exists(arxivats), gsub("_v[0-9]+\\.tsv$", paste0("_v", i, ".tsv"), arxivats), arxivats)
    i <- i + 1
  }

  dir.create(file.path(arrelProjecte, "edicions", "FET"), showWarnings = FALSE, recursive = TRUE)
  file.rename(fitxersFets, arxivats)

  return(ret)
}


desa_edicio <- function(edicio, fitxerEdicio, format = c("osc", "RData", "tsv", "xml")) {
  format <- match.arg(format)
  ext <- gsub("^.+\\.(osc|RData|tsv|xml)$", "\\1", fitxerEdicio, ignore.case = TRUE)
  if (!tolower(ext) %in% c("osc", "rdata", "tsv", "xml")) {
    warning("L'extensió del fitxer no es correpon amb el format. Afegint l'extensió del format al nom del fitxer.")
    fitxerEdicio <- paste0(fitxerEdicio, ".", format)
  }

  if (format == "tsv") {
    desa_edicio_tsv(edicio = edicio, fitxerEdicio = fitxerEdicio)
  } else if (format %in% c("osc", "xml")) {
    desa_edicio_osc(edicio = edicio, fitxerEdicio = fitxerEdicio)
  } else if (format == "RData") {
    save(edicio, file = fitxerEdicio, compress = "xz")
  }

  return(fitxerEdicio)
}


desa_edicio_tsv <- function(edicio, fitxerEdicio) {
  atri <- attributes(edicio)
  if (all(c("meta", "overpass_call") %in% names(atri))) {
    comentari <- paste0(
      '# "Actualització de ', attr(edicio, "meta")$timestamp,
      '"\t"consulta: ', gsub("\\n", "", attr(edicio, "overpass_call")), '"'
    )
    edicio <- osmdata_a_topoCat(edicio)
  } else if ("comentari" %in% names(atri)) {
    comentari <- atri$comentari
  }

  cat(comentari, "\n", file = fitxerEdicio, sep = "")
  suppressWarnings(
    utils::write.table(edicio, file = fitxerEdicio, append = TRUE, sep = "\t", na = "", row.names = FALSE)
  )
}


desa_edicio_osc <- function(edicio, fitxerEdicio) {
  xml2::write_xml(edicio, file = fitxerEdicio)
}


carrega_edicio <- function(fitxerEdicio) {
  format <- tolower(gsub("^.+\\.(RData|tsv|osc|xml)$", "\\1", fitxerEdicio, ignore.case = TRUE))

  if (format == "rdata") {
    load(file = fitxerEdicio)
  } else if (format == "tsv") {
    edicio <- carrega_edicio_tsv(fitxerEdicio = fitxerEdicio)
  } else if (format %in% c("osc", "xml")) {
    edicio <- carrega_edicio_osc(fitxerEdicio = fitxerEdicio)
  }

  attr(edicio, "fitxer") <- fitxerEdicio

  return(edicio)
}


carrega_edicio_tsv <- function(fitxerEdicio) {
  # Intercepta «Warning: EOF within quoted string», que descarta fitxersEdicios i retorna el fitxer problemàtic
  edicio <- tryCatch(
    utils::read.table(
      fitxerEdicio,
      header = TRUE, sep = "\t", quote = "\"", skip = 1,
      check.names = FALSE, comment.char = "", encoding = "utf8"
    ),
    warning = function(w) list(warning = w, fitxer = fitxerEdicio)
  )
  if (!inherits(edicio, "data.frame")) {
    # Llença l'alerta però llegeix el què es pugui i segueix.
    warning(
      edicio$warning, " a ", edicio$fitxer, " \n Obrint el document amb LibreOffice Calc i desant-lo editant ",
      "la configuració del filtre a Delimitador de camps={Tabulació}, Delimitador de cadenes de caràcter=\", i ",
      "activant Posa les cadenes de text entre cometes."
    )
    edicio <- suppressWarnings(utils::read.table(
      fitxerEdicio,
      header = TRUE, sep = "\t", quote = "\"", skip = 1,
      check.names = FALSE, comment.char = "", encoding = "utf8"
    ))
  }

  attr(edicio, "comentari") <- readLines(fitxerEdicio, n = 1, encoding = "utf8")
  return(edicio)
}


carrega_edicio_osc <- function(fitxerEdicio) {
  xml2::read_xml(fitxerEdicio)
}
