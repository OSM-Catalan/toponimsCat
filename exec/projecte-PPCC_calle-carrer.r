library(toponimsCat)

comarques<- read.csv("ppcc/comarques.csv", check.names=FALSE)

arrelProjecte<- "ppcc/calle-carrer"
actualitzaFitxer<- FALSE # actualitzaFitxer<- TRUE
filtre<- "nwr[name~'^[Cc]alle'][!'name:ca']"


## Generar informes per comarques dels PPCC amb LangToolsOSM ----
for (i in 1:nrow(comarques)){
  if (!is.na(comarques$`historic:admin_level`[i])){
    tipus<- "['historic:admin_level']"
  } else if (!is.na(comarques$admin_level[i])){
    tipus<- paste0("[admin_level=", comarques$admin_level[i], "]")
  }else{
    tipus<- ""
  }
  areaRegio<- paste0("['name:ca'='", gsub("\\'", "\\\\'", comarques$`name:ca`[i]) ,"']", tipus)
  fitxerInforme<- paste0("informe-", comarques$regio[i], "-", comarques$`name:ca`[i], "_name-calle.tsv")
  comarques$informe[i]<- file.path(arrelProjecte, "informes", fitxerInforme)

  if (!comarques$parcial[i]){
    cmd<- generaInforme(arrelProjecte=arrelProjecte, fitxerInforme=fitxerInforme,
                        filtreArea=areaRegio, filtreObjectes=filtre,
                        actualitzaFitxer=actualitzaFitxer)
  } else {
    consulta<- paste0("\"[out:json][timeout:1000]; ",
                      "area", areaRegio, "->.regio; ",
                      "area[name='Països Catalans']->.llengua; ",
                      "( ", filtre, "(area.regio)(area.llengua); ); ",
                      "out tags qt;\"")
    cmd<- generaInforme(arrelProjecte=arrelProjecte, fitxerInforme=fitxerInforme,
                        consulta=consulta, actualitzaFitxer=actualitzaFitxer)
  }

  if (length(cmd) == 0){
    cmd<- ""
  }

  comarques$cmd[i]<- cmd
}


## Executa les ordres
sel<- which(comarques$cmd != "")
comarques$`name:ca`[sel]

for (i in 1:length(sel)){
  message(i, " / ", length(sel), "\t", comarques[["name:ca"]][sel[i]])
  system(comarques$cmd[sel[i]])
}

## Si hi ha errors (eg. overpass va massa enfeinat i no respon), torneu a correr la secció amb
actualitzaFitxer<- FALSE


## Avalua el nombre de casos a cada informe generat ----
comarques<- recompteCasosInformes(dades=comarques)
comarques<- comarques[order(comarques$nCasos, decreasing=TRUE), ]

write.csv(comarques, file="ppcc/calle-carrer/casos_comarques.csv", row.names=FALSE)


## Edita les revisions ----
# les revisions són els casos únics a revisar de "name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id"
comarques<- read.csv(file="ppcc/calle-carrer/casos_comarques.csv", check.names=FALSE)
comarques<- comarques[!comarques$nObjects %in% c(NA, 0), ] # Selecciona comarques amb casos pendents
comarques[order(comarques$nCasos, decreasing=TRUE), ]

comarques$revisio<- generaRevisions_regexName(informes=comarques$informe, arrelProjecte=arrelProjecte,
                          cerca="([Cc])alle ", substitueix="\\1arrer ", revisioUnificada=FALSE)

# Reviseu i modifiqueu la revisió de l'informe que vulgueu de ppcc/calle-carrer/revisions/.
# Cal corregir els casos de name:ca i alt_name:ca incorrectes, esborrar-los o deixar-los en blanc si no és clar.
# És recomanable usar LibreOffice per evitar problemes de codificació i formats dels fitxers (https://www.softcatala.org/programes/libreoffice/)
# Moveu les revisions acabades a ppcc/calle-carrer/revisions/FET/.
# Si carregueu les edicions a OSM i actualitzeu els informes i revisions després d'acabar cada fitxer de revisió,
# és possible que es reutilitzin alguns casos a altres comarques. Podeu seguir l'script fins al final i tornar a
# començar per actualitzar els fitxers de revisions


## Fusiona les revisions fetes amb els informes i genera ordres per carregar-los a OSM ----
usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM

cmd<- preparaEdicions(arrelProjecte=arrelProjecte, usuari=usuari)
cmd<- na.omit(cmd)

## Afegeix paràmetres a les ordres. Veure «update_osm_objects_from_report --help» per les opcions de LangToolsOSM
nomComarca<- gsub(paste0(".+--input-file \\\"", arrelProjecte, "/edicions/informe-|_name-calle.tsv\\\".+"), "", cmd)
cmd<- paste0(cmd, " --changeset-hashtags \"#toponimsCat;#calle_carrer\" ",
            " --changeset-comment \"Afegeix name:ca a carrers de ", nomComarca, "\"")

## Executa les ordres
for (i in 1:length(cmd)){
  message(i, " / ", length(cmd), "\t", cmd[i])
  system(cmd[i])
}


## Arxiva els informes de les comarques actualitzades a edicions/FET i actualitza o elimina els informes originals desactualitzats ----
informesActualitzats<- actualitzaInformesCarregats(arrelProjecte=arrelProjecte, esborraInformesDesactualitzats=FALSE)
