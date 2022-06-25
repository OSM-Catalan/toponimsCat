library(toponimsCat)

arrelProjecte<- "exotopònims/Rússia"
fitxerInforme<- "informe-Rússia_wikidata.tsv"
filtreArea<- "['name:ca'='Rússia'][admin_level=2]"
filtreObjectes<- "nwr[wikidata][name][!'name:ca']"
revisioUnificada<- TRUE

# Expressions regulars sobre noms en català trets de wikidata
cerca<- " \\(.+\\)"  # Elimina aclaració entre parentesis habitual en alguns títols de la viquipèdia
substitueix<- ""


## Prepara taula de subdivisions ----
consulta<- subdivisionsConsultaOverpass(area=filtreArea, filtreSubdivisions="[admin_level=4]",
                                        etiquetes=c("name", "'name:ca'", "'wikidata'", "admin_level"))
cat(consulta)
## Executeu la consulta a https://overpass-turbo.eu i deseu el resultat al fitxer «divisions.csv» a l'arrel del projecte.


## Crea ordres per generar informes amb LangToolsOSM ----
actualitzaFitxer<- FALSE # actualitzaFitxer<- TRUE

divisions<- read.csv2(file.path(arrelProjecte, "divisions.csv"), check.names=FALSE)

for (i in 1:nrow(divisions)){
  areaDivisio<- paste0("['name:ca'='", gsub("\\'", "\\\\'", divisions$`name:ca`[i]) ,"']",
                       "[admin_level=", divisions$admin_level[i], "]")
  fitxerInforme<- paste0("informe-Rússia-", divisions$`name:ca`[i], "_", nomTipusObjectes, ".tsv")
  divisions$informe[i]<- file.path(arrelProjecte, "informes", fitxerInforme)

  if (FALSE){
    cmd<- generaInforme(arrelProjecte=arrelProjecte, fitxerInforme=fitxerInforme,
                        filtreArea=areaDivisio, filtreObjectes=filtreObjectes,
                        actualitzaFitxer=actualitzaFitxer)
  } else {
    # defineix area segons id de la divisió
    tipusObjecte<- gsub("\\[.+\\]", "", filtreObjectes)
    consulta<- paste0("\"[out:json][timeout:1000]; ",
                      "rel(", divisions$`@id`[i], ");map_to_area->.divisio; ",
                      tipusObjecte, "(area.divisio)", gsub(paste0("^", tipusObjecte), "", filtreObjectes), "; ",
                      "out tags qt;\"")
    cmd<- generaInforme(arrelProjecte=arrelProjecte, fitxerInforme=fitxerInforme,
                          consulta=consulta, actualitzaFitxer=actualitzaFitxer)
  }

  if (length(cmd) == 0){
    cmd<- ""
  }

  divisions$cmd[i]<- cmd
}


## Executa les ordres
sel<- which(divisions$cmd != "")
divisions$`name:ca`[sel]

for (i in 1:length(sel)){
  message(i, " / ", length(sel), "\t", divisions[["name:ca"]][sel[i]])
  system(divisions$cmd[sel[i]])
}


## Si hi ha errors (eg. overpass va massa enfeinat i no respon), torneu a correr la secció amb
actualitzaInformes<- FALSE


## Avalua el nombre de casos a cada informe generat ----
estat<- recompteCasosInformes(dades=divisions)
estat<- estat[order(estat$nCasosNomWikidata, decreasing=TRUE), ]

write.csv(estat, file=file.path(arrelProjecte, paste0("estat_", nomTipusObjectes, ".csv")), row.names=FALSE)


## Descarta els objectes d'OSM sense traducció de wikidata ----
estat<- read.csv(file.path(arrelProjecte, paste0("estat_", nomTipusObjectes, ".csv")))
descartaObjectesSenseTraduccions(fitxersInformes=estat$informe)


## Edita les revisions ----
# les revisions són els casos únics a revisar de "name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id"
estat<- read.csv(file=file.path(arrelProjecte, paste0("estat_", nomTipusObjectes, ".csv")), check.names=FALSE)
estatPendents<- estat[!estat$nObjects %in% c(NA, 0), ] # Selecciona informes amb casos pendents
estatPendents[order(estatPendents$nCasos, decreasing=TRUE), ]

estatPendents$revisio<- generaRevisions_regexTranslations(informes=estatPendents$informe, arrelProjecte=arrelProjecte,
                                                          cerca=cerca, substitueix=substitueix, revisioUnificada=revisioUnificada,
                                                          nomFitxerUnificat=paste0("revisio-UNIFICADA_", nomTipusObjectes, ".tsv"))

revisio<- lapply(estatPendents$revisio, function(x) read.table(x, header=TRUE, sep="\t", quote="\"", check.names=FALSE))

## Revisa i modifica les taules de revisió
lapply(revisio, function(x){
  # x$`name:ca`<- gsub("", "", x$`name:ca`)
  x
})

## Desa els fitxers revisats
mapply(function(revi, nomFitxer){
    fitxer<- file.path(arrelProjecte, "revisions", "FET", basename(nomFitxer))
    write.table(revi, file=fitxer, sep="\t", col.names=TRUE, row.names=FALSE, na="")
    return(fitxer)
  }, revi=revisio, nomFitxer=estatPendents$revisio)

# Reviseu i modifiqueu la revisió de l'informe que vulgueu de revisions/.
# Cal corregir els casos de name:ca i alt_name:ca incorrectes, esborrar-los o deixar-los en blanc si no és clar.
# És recomanable usar LibreOffice per evitar problemes de codificació i formats dels fitxers (https://www.softcatala.org/programes/libreoffice/)
# Moveu les revisions acabades a revisions/FET/.
# Si carregueu les edicions a OSM i actualitzeu els informes i revisions després d'acabar cada fitxer de revisió,
# és possible que es reutilitzin alguns casos a altres informes. Podeu seguir l'script fins al final i tornar a
# començar per actualitzar els fitxers de revisions


## Fusiona les revisions fetes amb els informes i genera ordres per carregar-los a OSM ----
usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
fitxerContrasenya<- "" # camí a un fitxer amb una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;)

cmd<- preparaEdicions(arrelProjecte=arrelProjecte, usuari=usuari, fitxerContrasenya=fitxerContrasenya)
cmd<- na.omit(cmd)

## Afegeix paràmetres a les ordres. Veure «update_osm_objects_from_report --help» per les opcions de LangToolsOSM
regio<- gsub(paste0(".+--input-file \\\"", arrelProjecte, "/edicions/informe-|_", nomTipusObjectes, ".tsv\\\".+"), "", cmd)
cmd<- paste0(cmd, " --changeset-hashtags \"#toponimsCat;#exotopònims\" ",
             " --changeset-comment \"Add «name:ca» in ", regio, "\"")
cat(cmd, sep="\n")

## Executa les ordres
for (i in 1:length(cmd)){
  message(i, " / ", length(cmd), "\t", cmd[i])
  system(cmd[i])
}


## Arxiva els informes actualitzats a edicions/FET i actualitza o elimina els informes originals desactualitzats ----
informesActualitzats<- actualitzaInformesCarregats(arrelProjecte=arrelProjecte, esborraInformesDesactualitzats=FALSE)
