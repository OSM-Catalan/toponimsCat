# TODO: mirar ?shQuote ?Quotes i r"(text amb " i ' a l'hora)" per generar ordres correctes independentment de si contenen «"'`»

#' Subdivisions d'àrees amb Overpass
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


#' Genera informe
#'
#' Genera un informe amb tots els objectes d'OSM segons l'àrea i els filtres especificats.
#'
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació serà la subcarpeta \code{informes}.
#' @param fitxerInforme vector amb els camins de fitxers d'informe.
#' @param filtreArea filtre d'àrea per la consulta d'Overpass.
#' @param filtreObjectes filtre d'etiquetes d'objectes d'OSM per la consulta d'Overpass.
#' @param consulta consulta completa d'Overpass. Si existeix el paràmetre, ignora \code{filtreArea} i \code{filtreObjectes}.
#' @param actualitzaFitxer si és \code{TRUE} i ja existeix el fitxer d'informe, el mou a la carpeta «ANTIC».
#'
#' @return Ordre per generar l'informe amb \code{write_osm_objects_report} de \href{https://github.com/OSM-Catalan/LangToolsOSM}{LangToolsOSM}.
#' @export
#'
#' @examples
#' ordre0<- generaInforme(arrelProjecte="exotopònims/Rússia/",
#'                        fitxerInforme="informe-Rússia.tsv",
#'                        filtreArea="['name:ca'='Rússia'][admin_level=3]",
#'                        filtreObjectes="nwr[wikidata][!'name:ca']")
#' ordre1<- generaInforme(arrelProjecte="ppcc/calle-carrer/",
#'                        fitxerInforme="informe-Calle_carrer-Alacant.tsv",
#'                        filtreArea="['name:ca'='Alacant'][admin_level=7]",
#'                        filtreObjectes="nwr[name~'^[Cc]alle '][!'name:ca']")
#' \dontrun{
#' # Crida les ordres de LantToolsOSM (cal que estigui instal·lat a l'entorn Python configurat a R)
#'   system(ordre0)
#'   system(ordre1)
#' }
generaInforme<- function(arrelProjecte, fitxerInforme, filtreArea, filtreObjectes,
                         consulta, actualitzaFitxer=FALSE){
  dir.create(file.path(arrelProjecte, "ANTIC/"), showWarnings=FALSE, recursive=TRUE)
  dir.create(file.path(arrelProjecte, "informes/"), showWarnings=FALSE, recursive=TRUE)

  fitxerInforme<- file.path(arrelProjecte, "informes", fitxerInforme)
  # Selecciona informes i mou els fitxers vells a la carpeta "$arrelProjecte/ANTIC
  actualitza<- FALSE
  if (file.exists(fitxerInforme)){
    message("El fitxer «", fitxerInforme, "» ja existeix. ", appendLF=FALSE)
    if (actualitzaFitxer){
      informe<- utils::read.table(fitxerInforme, header=TRUE, sep="\t", skip=1, check.names=FALSE)
      cat("\tn casos:", nrow(informe))
      if (nrow(informe) > 0){
        message("Movent a ", gsub("/+informes/+", "/ANTIC/", fitxerInforme))
        file.rename(fitxerInforme, gsub("/+informes/+", "/ANTIC/", fitxerInforme))
        actualitza<- TRUE
      }
    } else {
      message("Afegiu \"actualitzaFitxer=TRUE\" per actualitzar-lo.")
    }
  } else {
    actualitza<- TRUE
  }

  cmd<- character()
  if (actualitza){
    cmd<- paste0("write_osm_objects_report --lang ca -v",
                  " --output \"", fitxerInforme, "\" name:ca alt_name:ca alt_name")
    if (missing(consulta)){
      cmd<- paste0(cmd, " --filters \"", filtreObjectes, "\" --area \"", filtreArea, "\"")
    } else {
      cmd<- paste0(cmd, " --query ", consulta)
    }
  }

  return(cmd)
}


#' Descarta objectes sense traduccions dels informes
#'
#' Sobreescriu els informes descarant-ne els objectes (files) sense traduccions de wikidata.
#'
#' @param fitxersInformes camins dels informes.
#'
#' @return Retorna invisiblement els camins dels informes modificats.
#' @export
#
# @examples
descartaObjectesSenseTraduccions<- function(fitxersInformes){
  res<- character()
  for (i in seq_along(fitxersInformes)){
    informe<- try(utils::read.table(fitxersInformes[i], header=TRUE, sep="\t", skip=1, check.names=FALSE))
    if (!inherits(informe, "data.frame")){
      warning("Error de lectura per ", fitxersInformes[i])
      next
    }
    nObjectes<- nrow(informe)
    informe<- informe[!informe$translations %in% c(NA, ""), ]
    if (nObjectes == nrow(informe)){ # Si no hi ha canvis al fitxer, no reescriguis
      next
    }
    comentari<- readLines(fitxersInformes[i], n=1)
    cat(comentari, "\n", file=fitxersInformes[i], sep="")
    suppressWarnings(utils::write.table(informe, file=fitxersInformes[i], append=TRUE, sep="\t", na="", row.names=FALSE))
    res<- c(res, fitxersInformes[i])
  }

  invisible(res)
}


#' Recompte de casos dels informes
#'
#' @param arrelProjecte camí a l'arrel del projecte. Es llegiran tots els fitxers *.tsv de la subcarpeta \code{informes} si no s'especifiquen altres paràmetres.
#' @param informes vector de caràcters amb els camins a fitxers d'informes.
#' @param dades un \code{data.frame} amb una columna anomenada \code{informe} amb els camins a fitxers d'informes. Si s'especifica, s'ignoren la resta de paràmetres.
#'
#' @return Una taula amb files per cada informe i amb les columnes següents:
#'   \describe{
#'     \item{nObjectes}{nombre d'objectes d'OSM.}
#'     \item{nCasos}{nombre de casos únics de les etiquetes \code{«name»}, \code{«name:ca»}, \code{«alt_name:ca»},
#'  \code{«alt_name»}, \code{«translations»}, \code{«ca.wikipedia_page»} i \code{«wikidata_id»}.}
#'     \item{nObjectesNomWikidata}{nombre d'objectes d'OSM amb nom en català a wikidata (columna translations dels informes).}
#'     \item{nCasosNomWikidata}{nombre de casos únics amb nom en català a wikidata (columna translations dels fitxers de revisions).}
#'     \item{revisat}{si existeix o no un fitxer de revisió a la subcarpeta del projecte \code{revisions/FET}).}
#'  }
#' @export
#
# @examples
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
    return(data.frame())
  }

  dades$informe<- gsub("//", "/", dades$informe)  # Normalitza camins

  for (i in 1:nrow(dades)){
    objectesOSM<- try(utils::read.table(dades$informe[i], header=TRUE, sep="\t", skip=1, check.names=FALSE))
    if (inherits(objectesOSM, "data.frame")){
      casosRevisar<- unique(objectesOSM[, c("name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id")])
      dades$nObjects[i]<- nrow(objectesOSM)
      dades$nCasos[i]<- nrow(casosRevisar)

      if ("translations" %in% names(objectesOSM)){
        dades$nObjectesNomWikidata[i]<- sum(!objectesOSM$translations %in% c(NA, ""))
        dades$nCasosNomWikidata[i]<- sum(!casosRevisar$translations %in% c(NA, ""))
      }

      fitxerRevisio<- gsub("^informe-", "revisio-", basename(gsub("(_v[0-9]+)*\\.tsv$", "", dades$informe[i])))
      camiRevisions<- gsub("informes", "revisions/FET", dirname(dades$informe[i]), "FET")
      dades$revisat[i]<- any(grepl(fitxerRevisio, dir(camiRevisions)))
    } else{
      warning("Error a l'informe", i, ":", informes[i], "\n")
      print(objectesOSM)
    }
  }

  return(dades)
}


# informes<- dir(paste0(arrelProjecte, "informe"), "\\.tsv$", full.names=TRUE, include.dirs=FALSE)
#' Genera fitxers de revisió d'informes
#'
#' Els fitxers de revisió d'informes contenen els casos a revisar amb combinacions úniques de
#'  les etiquetes \code{«name»}, \code{«name:ca»}, \code{«alt_name:ca»}, \code{«alt_name»},
#'  \code{«translations»}, \code{«ca.wikipedia_page»} i \code{«wikidata_id»}.
#'  Cal corregir els casos de \code{«name:ca»} i \code{«alt_name:ca»} incorrectes i esborrar-los o deixar-los en blanc si no és clar. Un cop revisat,
#'  moveu els fitxers de revisions a «arrelProjecte/revisio/FET/» i prepareu les edicions a OSM amb la funció \code{\link{preparaEdicions}}.
#'
#' @param informes camí dels informes pels que es vol generar fitxers de revisió.
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació serà la subcarpeta \code{revisions}.
#' @param cerca expressió regular del patró de cerca (\link{regex}).
#' @param substitueix text a substituir.
#' @param ometSenseTraduccions si és \code{TRUE}, descarta els objectes sense nom en català a wikidata.
#' @param revisioUnificada si és \code{TRUE}, genera un unic fitxer de revisió. Sinó, genera un fixer de revisió per cada informe.
#' @param nomFitxerUnificat nom del fitxer de revisió unificat quan \code{revisioUnificada=TRUE}. Si \code{revisioUnificada=FALSE}, els fitxers de revisió seran \code{gsub("^informe", "revisio", informes)}.
#'
#' @return Retorna els camins dels fitxers de revisió generats.
#' @seealso \link{gsub}
#' @name generaRevisions
#'
#' @examples
#' generaRevisions_regexName(informes=dir("ppcc/calle-carrer/informes", "\\.tsv$", full.names=TRUE),
#'                           arrelProjecte="ppcc/calle-carrer",
#'                           cerca="([Cc])alle ",
#'                           substitueix="\\1arrer ", revisioUnificada=FALSE)
#'
#' generaRevisions_regexTranslations(informes=dir("exotopònims/Grècia/informes", "\\.tsv$",
#'                                                full.names=TRUE),
#'                                   arrelProjecte="exotopònims/Grècia",
#'                                   cerca=" \\(.+\\)",
#'                                   substitueix="", revisioUnificada=FALSE)

NULL

generaRevisions<- function(informes, arrelProjecte){
  ## Normalitza camins per evitar problemes en modificar-los
  arrelProjecte<- gsub("/$", "", arrelProjecte)
  informes<- gsub("//", "/", informes)

  ### TODO: reutilitza les revisions fetes ----
  # Descarta casos ja revisats sense name:ca
  # setdiff(d[, c("name", "alt_name", "wikidata_id")], revisio.casosDESCARTATS)
  # Casos completats, ja pujats a OSM
  # Revisions fetes (name:ca = '' per traduccions descartades)
  dir.create(file.path(arrelProjecte, "revisions", "FET"), showWarnings=FALSE, recursive=TRUE)
  fitxersRevisio<- dir(file.path(arrelProjecte, "revisions", "FET"), "\\.tsv$", full.names=TRUE, include.dirs=FALSE)
  revisionsFETES<- lapply(fitxersRevisio, function(x){
    utils::read.table(x, header=TRUE, sep="\t", check.names=FALSE)
  })
  names(revisionsFETES)<- gsub(file.path(arrelProjecte, "revisions", "FET/"), "", fitxersRevisio)
  revisio.casosFETS<- unique(do.call(rbind, revisionsFETES))
  # revisio.casosCOMPLETATS<- revisio.casosFETS[!revisio.casosFETS$`name:ca` %in% c(NA, ""), ]
  # revisio.casosDESCARTATS<- revisio.casosFETS[revisio.casosFETS$`name:ca` %in% c(NA, ""), ]

  dL<- list()
  for (i in seq_along(informes)){
    nomFitxer<- gsub(paste0("^", file.path(arrelProjecte, "informes/")), "", informes[i])
    nomFitxer<- gsub("^informe", "revisio", nomFitxer)
    d<- try(utils::read.table(informes[i], header=TRUE, sep="\t", skip=1, check.names=FALSE))

    if (!inherits(d, "data.frame")) next

    d<- unique(d[, c("name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id")])
    dL[[nomFitxer]]<- d
  }

  return(dL)
}


#' @rdname generaRevisions
#' @export
generaRevisions_regexName<- function(informes, arrelProjecte, cerca, substitueix, revisioUnificada=FALSE, nomFitxerUnificat="revisio-UNIFICADA.tsv"){
  dL<- generaRevisions(informes=informes, arrelProjecte=arrelProjecte)
  for (i in seq_along(dL)){
    nomFitxer<- names(dL)[i]
    d<- dL[[i]]
    d$`name:ca`<- ifelse(is.na(d$`name:ca`), gsub(cerca, substitueix, d$name), d$`name:ca`)
    d$`alt_name:ca`<- ifelse(is.na(d$`alt_name:ca`), gsub(cerca, substitueix, d$alt_name), d$`alt_name:ca`)

    # d<- d[!grepl("Maestro|Médico|Profesor|Padre|Virgen|Señora|San ", d$`name:ca`), ] # descarta carrers amb nom en castellà

    if (nrow(d) > 0){
      dL[[nomFitxer]]<- d
      if (!revisioUnificada){
        utils::write.table(d, file.path(arrelProjecte, "revisions", nomFitxer), sep="\t", na="", col.names=TRUE, row.names=FALSE)
      }
    } else {
      suppressWarnings(file.remove(file.path(arrelProjecte, "revisions", nomFitxer)))
    }
  }

  if (revisioUnificada){
    revisio.TOTS<- unique(do.call(rbind, dL))
    utils::write.table(revisio.TOTS, file.path(arrelProjecte, "revisions", nomFitxerUnificat), sep="\t", na="", col.names=TRUE, row.names=FALSE)
    ret<- file.path(arrelProjecte, "revisions", nomFitxerUnificat)
  } else {
    ret<- file.path(arrelProjecte, "revisions", names(dL))
  }
  message("FET! Reviseu i modifiqueu la revisió de l'informe que vulgueu de ",
          file.path(arrelProjecte, "revisions"), "/.\n",
          "Cal corregir els casos de name:ca i alt_name:ca incorrectes i esborrar-los i deixar-los en blanc si no és clar.\n",
          "Moveu les revisions acabades a ", file.path(arrelProjecte, "revisions", "FET"), "/."
  )

  return(ret)
}


#' @rdname generaRevisions
#' @export
generaRevisions_regexTranslations<- function(informes, arrelProjecte, cerca=" \\(.+\\)", substitueix="", ometSenseTraduccions=TRUE, revisioUnificada=FALSE, nomFitxerUnificat="revisio-UNIFICADA.tsv"){
  dL<- generaRevisions(informes=informes, arrelProjecte=arrelProjecte)
  for (i in seq_along(dL)){
    nomFitxer<- names(dL)[i]
    d<- dL[[i]]
    if (ometSenseTraduccions){
      senseTraduccio<- d$translations  %in% c("", NA)
      d<- d[!senseTraduccio, ]
      # message("Descartant ", sum(senseTraduccio), " objectes sense traducció a wikidata.")
    }

    if (nrow(d) == 0){
      suppressWarnings(file.remove(file.path(arrelProjecte, "revisions", nomFitxer)))
      next
    }

    traduccions<- strsplit(d$translations, ", ")
    traduccions<- lapply(traduccions, function(x) unique(gsub(cerca, substitueix, x)))
    d$`name:ca`<- ifelse(d$`name:ca` %in% c("", NA), sapply(traduccions, function(x) x[1]), d$`name:ca`)
    d$`alt_name:ca`<- ifelse(d$`alt_name:ca` %in% c("", NA) & sapply(traduccions, length) > 1,
                             sapply(traduccions, function(x) paste(x[-1], collapse=";")),
                             d$`alt_name:ca`)

    if (nrow(d) > 0){
      dL[[nomFitxer]]<- d
      if (!revisioUnificada){
        utils::write.table(d, file.path(arrelProjecte, "revisions", nomFitxer), sep="\t", na="", col.names=TRUE, row.names=FALSE)
      }
    } else {
      suppressWarnings(file.remove(file.path(arrelProjecte, "revisions", nomFitxer)))
    }
  }

  if (revisioUnificada){
    revisio.TOTS<- unique(do.call(rbind, dL))
    utils::write.table(revisio.TOTS, file.path(arrelProjecte, "revisions", nomFitxerUnificat), sep="\t", na="", col.names=TRUE, row.names=FALSE)
    ret<- file.path(arrelProjecte, "revisions", nomFitxerUnificat)
  } else {
    ret<- file.path(arrelProjecte, "revisions", names(dL))
  }
  message("FET! Reviseu i modifiqueu la revisió de l'informe que vulgueu de ",
          file.path(arrelProjecte, "revisions"), "/.\n",
          "Cal corregir els casos de name:ca i alt_name:ca incorrectes i esborrar-los i deixar-los en blanc si no és clar.\n",
          "Moveu les revisions acabades a ", file.path(arrelProjecte, "revisions", "FET"), "/."
  )

  return(ret)
}


#' Genera fitxers amb edicions combinant els informes i revisions
#'
#' Reaprofita les revisions per casos que només es diferencien per si els noms comencen amb majúscules o minúscules, mantenint el format original.
#'
#' @param arrelProjecte camí a l'arrel del projecte. La carpeta de destinació serà la subcarpeta \code{edicions} i buscarà els informes i revisions a partir d'aquest camí.
#' @param usuari nom de l'usuari a OpenStreetMap per pujar les edicions.
#' @param fitxerContrasenya camí a un fitxer amb la contrasenya de l'usuari per evitar que LangToolsOSM la demani. El fitxer conté una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;) i el llegirà \href{https://osmapi.metaodi.ch/osmapi/OsmApi.html#OsmApi}{Osmapi}.
#'
#' @return Vector d'ordres per carregar els fitxers generats amb \code{update_osm_objects_from_report} de \href{https://github.com/OSM-Catalan/LangToolsOSM}{LangToolsOSM}.
#' @export
#
# @examples
preparaEdicions<- function(arrelProjecte, usuari, fitxerContrasenya){
  arrelProjecte<- gsub("/$", "", arrelProjecte)  # Normalitza camins per evitar problemes en modificar-los

  # Fusiona revisions fetes amb tots els informes que contenen objectes amb etiquetes iguals
  fitxersRevisions<- dir(file.path(arrelProjecte, "revisions","FET"), "\\.tsv$", full.names=TRUE, include.dirs=FALSE)

  if (length(fitxersRevisions) == 0){
    message("No hi ha cap fitxer de revisions a ", file.path(arrelProjecte, "revisions","FET"), "/.")
    return(character())
  }

  revisionsFETES<- lapply(fitxersRevisions, function(x){
    utils::read.table(x, header=TRUE, sep="\t", check.names=FALSE)
  })
  names(revisionsFETES)<- gsub(paste0(file.path(arrelProjecte, "revisions", "FET"), "/+"), "", fitxersRevisions)
  revisio.casosFETS<- do.call(rbind, revisionsFETES)
  revisio.casosFETS<- revisio.casosFETS[!revisio.casosFETS$`name:ca` %in% c(NA, "") | !revisio.casosFETS$`alt_name:ca` %in% c(NA, ""), ]
  row.names(revisio.casosFETS)<- NULL

  if (nrow(revisio.casosFETS) == 0){
    message("Les revisions fetes no contenen cap cas.")
    return(character())
  }

  ## Genera versions començant per majúscula I per minúscula (útil reutilitzar revisions p.ex. de «carrer/Carrer»)
  revisio.casosFETS_min<- apply(revisio.casosFETS[, c("name", "name:ca", "alt_name", "alt_name:ca")], 1, function(x){
    paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  })
  revisio.casosFETS[, c("name", "name:ca", "alt_name", "alt_name:ca")]<- as.data.frame(t(revisio.casosFETS_min))
  revisio.casosFETS_min<- revisio.casosFETS<- unique(revisio.casosFETS)

  revisio.casosFETS_maj<- apply(revisio.casosFETS[, c("name", "name:ca", "alt_name", "alt_name:ca")], 1, function(x){
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  })
  revisio.casosFETS[, c("name", "name:ca", "alt_name", "alt_name:ca")]<- as.data.frame(t(revisio.casosFETS_maj))
  revisio.casosFETS_maj<- revisio.casosFETS
  revisio.casosFETS<- rbind(revisio.casosFETS_min, revisio.casosFETS_maj)


  if (any(dup<- duplicated(revisio.casosFETS[, c("name", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id")]))){
    print(revisio.casosFETS[dup, c("name", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id")])
    warning("Hi ha discrepàncies entre fitxers de revisions. Es descartaran les entrades amb valors duplicats de
            «name», «alt_name», «translations», «ca.wikipedia_page» i «wikidata_id».")
    dup<- duplicated(pk<- do.call(paste, revisio.casosFETS[, c("name", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id")]))
    dupPKstr<- pk[dup]
    revisio.casosFETS<- revisio.casosFETS[!pk %in% dupPKstr, ]
    # TODO: millora indicant els fitxers amb duplicats
  }

  dir.create(file.path(arrelProjecte, "edicions"), showWarnings=FALSE, recursive=TRUE)
  fitxersInformes<- dir(file.path(arrelProjecte, "informes"), "\\.tsv$", full.names=TRUE, include.dirs=FALSE)
  cmd<- character()
  for (i in seq_along(fitxersInformes)){
    nomFitxer<- gsub(file.path(arrelProjecte, "informes/*"), "", fitxersInformes[i])
    informe<- try(utils::read.table(fitxersInformes[i], header=TRUE, sep="\t", skip=1, check.names=FALSE))

    edicions<- merge(informe[, setdiff(names(informe), c("name:ca", "alt_name:ca"))], revisio.casosFETS)
    if (nrow(edicions) > 0){
      dFormated<- rbind(c("# EDITED with R", rep("", times=ncol(edicions) - 1)), names(edicions))
      dFormated<- rbind(dFormated, as.matrix(edicions), deparse.level=0)
      suppressWarnings(utils::write.table(dFormated, file.path(arrelProjecte, "edicions", nomFitxer), sep="\t", na="", col.names=FALSE, row.names=FALSE))

      cmd[i]<- paste0('update_osm_objects_from_report --username ', usuari)
      if (!missing(fitxerContrasenya)){
        cmd[i]<- paste0(cmd[i], ' --passwordfile "', fitxerContrasenya, '"')
      }
      cmd[i]<- paste0(cmd[i], ' -v --confirmed-edits --confirm-overwrites --input-file "', file.path(arrelProjecte, "edicions", nomFitxer), '" name:ca')
      if (!all(is.na(edicions$`alt_name:ca`))){
        cmd[i]<- paste0(cmd[i], " alt_name:ca")
      }
    }
  }

  message("Fitxers a punt! Un cop hagueu carregat les edicions a OSM amb les ordres retornades, useu la funció «actualitzaInformesCarregats» per arxivar les edicions a ",
          file.path(arrelProjecte, "edicions", "FET"), "."
  )

  return(cmd)
}


#' Actualitza els informes de les edicions carregades
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
actualitzaInformesCarregats<- function(arrelProjecte, esborraInformesDesactualitzats=FALSE){
  fitxersFets<- dir(file.path(arrelProjecte, "edicions"), pattern="\\.tsv$", full.names=TRUE, include.dirs=FALSE)
  fitxersInformesOri<- gsub("/edicions/", "/informes/", fitxersFets)

  ret<- character()
  for (i in seq_along(fitxersFets)){
    if (esborraInformesDesactualitzats){
      suppressWarnings(file.remove(fitxersInformesOri[i]))
      ret<- c(ret, fitxersInformesOri[i])
      next
    }

    if (!file.exists(fitxersInformesOri[i])){
      next
    }

    informeOri<- utils::read.table(fitxersInformesOri[i], header=TRUE, sep="\t", skip=1, check.names=FALSE)
    carregat<- utils::read.table(fitxersFets[i], header=TRUE, sep="\t", skip=1, check.names=FALSE)

    informeOri<- data.table::as.data.table(informeOri)
    carregat<- data.table::as.data.table(carregat)
    data.table::set(informeOri, j=names(informeOri), value=lapply(informeOri, function(x){ x[is.na(x)]<- ""; as.character(x) }))
    data.table::set(carregat, j=names(carregat), value=lapply(carregat, function(x){ x[is.na(x)]<- ""; as.character(x) }))
    data.table::set(carregat, j=c("name:ca", "alt_name:ca"), value=list(NA_character_, NA_character_))
    data.table::setcolorder(carregat, neworder=names(informeOri))

    informeNou<- data.table::fsetdiff(informeOri, carregat)

    if (!data.table::fsetequal(informeNou, informeOri)){
      comentari<- readLines(fitxersInformesOri[i], n=1)
      cat(comentari, "\n", file=fitxersInformesOri[i], sep="")
      suppressWarnings(utils::write.table(informeNou, file=fitxersInformesOri[i], append=TRUE, sep="\t", na="", row.names=FALSE))
      ret<- c(ret, fitxersInformesOri[i])
    }
  }

  ## Mou informes desactualitzats a ANTIC/
  # dir.create(paste0(arrelProjecte, "ANTIC/"), showWarnings=FALSE, recursive=TRUE)
  # file.rename(fitxersInformesOri, gsub("/informe/", "/ANTIC/", fitxersInformesOri))

  arxivats<- gsub("/edicions/", "/edicions/FET/", fitxersFets)
  arxivats<- gsub("\\.tsv$", "_v0.tsv", arxivats)
  i<- 1
  while (any(file.exists(arxivats))){
    arxivats<- ifelse(file.exists(arxivats), gsub("_v[0-9]+\\.tsv$", paste0("_v", i, ".tsv"), arxivats), arxivats)
    i<- i + 1
  }

  dir.create(file.path(arrelProjecte, "edicions", "FET"), showWarnings=FALSE, recursive=TRUE)
  file.rename(fitxersFets, arxivats)

  return(ret)
}
