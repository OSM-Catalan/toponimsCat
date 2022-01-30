comarques<- read.csv("ppcc/comarques.csv", check.names=FALSE)

## Crea ordres per generar reports ----
ometComarques0casos<- TRUE
actualitzaFitxers<- FALSE
filtre<- "nwr[name~'^Calle'][!'name:ca']"
outFile<- "ppcc/calle-carrer/commands-report-comarques.txt"

dir.create("ppcc/calle-carrer/OLD/", showWarnings=FALSE)

# Selecciona comarques i mou els fitxers vells a la carpeta "ppcc/calle-carrer/OLD
selComarques<- numeric()
for (i in 1:nrow(comarques)){
  cat(i, "/", nrow(comarques), comarques$regio[i], "-", comarques$`name:ca`[i])
  zona<- paste0("report-", comarques$regio[i], "-", comarques$`name:ca`[i], "_name-calle.csv")
  zona<- gsub("L'", "L\\\\'", zona)
  zona<- gsub("l'([AEIOUH])", "l\\\\'\\1", zona)
  zona<- gsub("d'([AEIOUH])", "d\\\\'\\1", zona)
  reportFile<- paste0("ppcc/calle-carrer/report/", zona)
  if (file.exists(reportFile)){
    if (actualitzaFitxers){
      report<- read.table(reportFile, header=TRUE, sep="\t", skip=1, check.names=FALSE)
      cat("\tn casos:", nrow(report))
      if (nrow(report) > 0 | !ometComarques0casos){
        file.rename(reportFile, gsub("/report/", "/OLD/", reportFile))
        selComarques<- c(selComarques, i)
        cat("\t\tSELECCIONAT")
      }
    }
  } else {
    selComarques<- c(selComarques, i)
    cat("\t\tSELECCIONAT")
  }
  cat("\n")
}
comarques$`name:ca`[selComarques]

sink(file=outFile)
for (i in selComarques){
  if (!is.na(comarques$`historic:admin_level`[i])){
    type<- "['historic:admin_level']"
  } else if (!is.na(comarques$admin_level[i])){
    type<- paste0("[admin_level=", comarques$admin_level[i], "]")
  }else{
    type<- ""
  }

  cmd<- paste0("write_osm_objects_report --lang ca -v",
               " --output \"report/report-", comarques$regio[i], "-", comarques$`name:ca`[i], "_name-calle.csv\" name:ca alt_name:ca alt_name")
  areaComarca<- paste0("['name:ca'='", comarques$`name:ca`[i] ,"']", type)
  if (comarques$parcial[i]){
    # TODO calen canvis a LangTools per permetre interseccions d'àreas:
    query<- paste0("\"[out:json][timeout:1000]; ",
                   "area", areaComarca, "->.comarca; ",
                   "area[name='Països Catalans']->.llengua; ",
                   "( ", filtre, "(area.comarca)(area.llengua); ); ",
                   "out tags qt;\"")
    cmd<- paste0(cmd, " --query ", query)
  } else {
    cmd<- paste0(cmd, " --filters \"", filtre, "\" --area \"", areaComarca, "\"")
  }

  cmd<- gsub("L'", "L\\\\'", cmd)
  cmd<- gsub("l'([AEIOUH])", "l\\\\'\\1", cmd)
  cmd<- gsub("d'([AEIOUH])", "d\\\\'\\1", cmd)
  cat(cmd, "\n")
}
sink()

## GOTO: situeu-vos a la carpeta *cd ppcc/calle-carrer/* i executeu les ordres
# de "ppcc/calle-carrer/commands-report-comarques.txt" a un terminal amb LangToolsOSM instal·lat
# Si hi ha errors (eg. overpass va massa enfeinat i no respon), torneu a correr la secció amb
actualitzaFitxers<- FALSE


## Avalua el nombre de casos a cada report generat ----
reports<- dir("ppcc/calle-carrer/report", "\\.csv$", full.names=TRUE, include.dirs=FALSE)
sort(file.info(reports)$size)
comarques$nObjects<- NA_integer_
comarques$nCasos<- NA_integer_
comarques$revisat<- FALSE
for (i in seq_along(reports)){
  osmObjects<- try(read.table(reports[i], header=TRUE, sep="\t", skip=1, check.names=FALSE))
  casosReview<- unique(osmObjects[, c("name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id")])
  if (inherits(osmObjects, "data.frame")){
    comarca<- gsub("^.+/report-[a-zA-Z]+-|_name-calle.csv", "", reports[i])
    comarca<- gsub("\\", "", comarca, fixed=TRUE)
    comarques$nObjects[comarques$`name:ca` == comarca]<- nrow(osmObjects)
    comarques$nCasos[comarques$`name:ca` == comarca]<- nrow(casosReview)
    reviewFile<- gsub("report", "review", reports[i])
    reviewFile<- gsub("/review/", "/review/FET/", reviewFile)
    if (file.exists(reviewFile)){
      comarques$revisat[comarques$`name:ca` == comarca]<- TRUE
    }
  } else{
    cat("Error pel report", i, ":", reports[i], "\n")
    print(osmObjects)
  }
}

write.csv(comarques, file="ppcc/calle-carrer/casos_comarques.csv", row.names=FALSE)


## Edita els reviews ----
# reports són els casos únics a revisar de "name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id"
comarques<- read.csv(file="ppcc/calle-carrer/casos_comarques.csv", check.names=FALSE)
comarques<- comarques[!comarques$nObjects %in% c(NA, 0), ] # Selecciona comarques amb casos pendents
comarques[order(comarques$nCasos, decreasing=TRUE), ]

## selecciona fitxers de les comarques seleccionades
reports<- dir("ppcc/calle-carrer/report", "\\.csv$", full.names=TRUE, include.dirs=FALSE)
selReports<- grep(paste(comarques$`name:ca`, collapse="|"), gsub("\\", "", reports, fixed=TRUE))
reports<- reports[selReports]

# Reviews fets (name:ca = '' per traduccions descartades)
dir.create("ppcc/calle-carrer/review/FET/", showWarnings=FALSE)
reviewFiles<- dir("ppcc/calle-carrer/review/FET", "\\.csv$", full.names=TRUE, include.dirs=FALSE)
reviewsFETS<- lapply(reviewFiles, function(x){
  read.delim(x, sep="\t", check.names=FALSE)
})
names(reviewsFETS)<- gsub("ppcc/calle-carrer/review/FET/", "", reviewFiles)
review.casosFETS<- unique(do.call(rbind, reviewsFETS))
review.casosCOMPLETATS<- review.casosFETS[!review.casosFETS$`name:ca` %in% c(NA, ""), ]
review.casosDESCARTATS<- review.casosFETS[review.casosFETS$`name:ca` %in% c(NA, ""), ]

path<- "ppcc/calle-carrer/review/"
for (i in seq_along(reports)){
  fileName<- gsub("^ppcc/calle-carrer/report/report", "review", reports[i])
  d<- read.delim(reports[i], skip=1, check.names=FALSE)
  d<- unique(d[, c("name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id")])
  d$`name:ca`<- ifelse(is.na(d$`name:ca`), gsub("[Cc]alle ", "Carrer ", d$name), d$`name:ca`)
  d$`alt_name:ca`<- ifelse(is.na(d$`alt_name:ca`), gsub("[Cc]alle ", "Carrer ", d$alt_name), d$`alt_name:ca`)

  d<- d[!grepl("Maestro|Médico|Profesor|Padre|Virgen|Señora|San ", d$`name:ca`), ] # descarta carrers amb nom en castellà
  ### TODO: reutilitza els reviews fets ----
  # Descarta casos ja revisats sense name:ca
  # setdiff(d[, c("name", "alt_name", "wikidata_id")], review.casosDESCARTATS)
  # Casos completats, ja pujats a OSM
  if (nrow(d) > 0){
    write.table(d, paste0(path, fileName), sep="\t", col.names=TRUE, row.names=FALSE, na="")
  }
}


# GOTO: reviseu i modifiqueu el review de la comarca que vulgueu de ppcc/calle-carrer/review/
# Els casos de name:ca incorrectes cal esborrar-los i deixar-los en blanc
# Moveu els reviews acabats a "ppcc/calle-carrer/review/FET/"


## Fusiona reviews fets amb tots els reports que contenen objectes amb etiquetes iguals ----
usuari<- "jmaspons" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
path<- "ppcc/calle-carrer/upload/"
reviewFiles<- dir("ppcc/calle-carrer/review/FET", "_name-calle\\.csv$", full.names=TRUE, include.dirs=FALSE)
# reviewFiles<- reviewFiles[1] # SELECCIONA COMARCA (pendent de processar)


##### Reutilitza per totes les comarques tots els reviews fets
reviewsFETS<- lapply(reviewFiles, function(x){
  read.delim(x, sep="\t", check.names=FALSE)
})
names(reviewsFETS)<- gsub("ppcc/calle-carrer/review/FET/", "", reviewFiles)
review.casosFETS<- unique(do.call(rbind, reviewsFETS))
review.casosFETS<- review.casosFETS[!review.casosFETS$`name:ca` %in% c(NA, ""), ]

sink(file="ppcc/calle-carrer/commands-upload.txt")
for (i in seq_along(reports)){
  fileName<- gsub("ppcc/calle-carrer/report/", "", reports[i])
  report<- read.delim(reports[i], skip=1, check.names=FALSE)

  dTrans<- merge(report[, setdiff(names(report), c("name:ca", "alt_name:ca"))], review.casosFETS)
  if (nrow(dTrans) > 0){
    dFormated<- rbind(c("# EDITED with R", rep("", times=ncol(dTrans) - 1)), names(dTrans))
    dFormated<- rbind(dFormated, as.matrix(dTrans), deparse.level=0)
    write.table(dFormated, paste0(path, fileName), sep="\t", col.names=FALSE, row.names=FALSE, na="")
    cat('update_osm_objects_from_report --username ', usuari, ' --batch 100 -v --confirmed-edits --confirm-overwrites --input-file "upload/', fileName, '" name:ca', sep="")
    if (!all(is.na(dTrans$`alt_name:ca`))){
      cat(" alt_name:ca\n")
    } else {
      cat("\n")
    }
  }
}
sink()

# GOTO: situeu-vos a la carpeta *cd ppcc/calle-carrer/* i executeu les ordres
# de "ppcc/calle-carrer/commands-upload.txt" a un terminal amb LangToolsOSM instal·lat


## Un cop carregats els canvis, mou els reports de les comarques actualitzades a upload/FET i arxiva els reports originals
fets<- dir("ppcc/calle-carrer/upload", pattern="\\.csv$", full.names=TRUE, include.dirs=FALSE)

reportsOri<- gsub("/upload/", "/report/", fets)
dir.create("ppcc/calle-carrer/OLD/", showWarnings=FALSE)
file.rename(reportsOri, gsub("/report/", "/OLD/", reportsOri))

arxivats<- gsub("upload/", "upload/FET/", fets)
arxivats<- gsub("\\.csv", "_v0.csv", arxivats)
i<- 1
while (any(file.exists(arxivats))){
  arxivats<- ifelse(file.exists(arxivats), gsub("_v[0-9]+\\.csv$", paste0("_v", i, ".csv"), arxivats), arxivats)
  i<- i + 1
}
file.rename(fets, arxivats)
