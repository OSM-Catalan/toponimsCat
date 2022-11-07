library(toponimsCat)

## Copia name -> name:ca per valors inequivocament catalans

arrelProjecte<- "PPCC/name-name:ca"
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE ## TODO: unificar parametre a generaInforme i generaInformesPPCC
# TODO: Ajuntament Autovia Bosc Carretera Castell Clos Giratori Hort Jardí mas Palau Passeig Rambla Riu Ronda Ruta Rotonda Platja Polígon
## ULL VIU!: clos i parc col·lisionen amb el francés. carretera Rambla Ronda Ruta Via rotonda col·lisionen amb el castellà.
# FET
# filtre<- "nwr[name~'^([Aa]vinguda|[Cc]arrer|[Cc]amí|[Pp]laça|[Pp]arc) '][!'name:ca']"; sufixFitxers<- "_name-name:ca"
# PENDENT;
filtre<- "nwr[name~'^([Aa]tzucac|[Aa]vinguda|[Cc]amí|[Cc]aminal|[Cc]arreró|[Gg]iratori|[Jj]ardí|[Pp]assatge|[Pp]asseig|[Rr]otonda|[Uu]rbanització|[Vv]oral) '][!'name:ca']"
sufixFitxers<- "_name-name:ca_r1"
cerca<- ""
substitueix<- ""
revisioUnificada<- TRUE


## Generar informes pels municipis dels PPCC amb LangToolsOSM ----
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE

municipis<- generaInformesPPCC(arrelProjecte=arrelProjecte, filtre=filtre, actualitzaInformes=actualitzaInformes, sufixFitxers=sufixFitxers, divisions=toponimsCat::municipis)

## Executa les ordres
sel<- which(municipis$cmd != "")
municipis$`name:ca`[sel]

for (i in 1:length(sel)){
  message(i, " / ", length(sel), "\t", municipis[["name:ca"]][sel[i]])
  system(municipis$cmd[sel[i]])
}

## Si hi ha errors (eg. overpass va massa enfeinat i no respon), torneu a correr la secció amb
actualitzaInformes<- FALSE


## Avalua el nombre de casos a cada informe generat ----
municipis<- recompteCasos(dades=municipis)
ordCasos<- order(municipis$revisat, municipis$nCasos, municipis$`name:ca`,
                 decreasing=c(FALSE, TRUE, FALSE), method="radix")
ordCol<- c("name:ca", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat")
ordCol<- c(ordCol, setdiff(names(municipis), c(ordCol, "cmd")))
municipis<- municipis[ordCasos, ordCol]
municipis[intersect(order(municipis$nCasos, decreasing=TRUE), which(!municipis$revisat & municipis$nObjectes > 0)), ]

write.csv(municipis, file=file.path(arrelProjecte, "casos_municipis.csv"), row.names=FALSE)


## Edita les revisions ----
# les revisions són els casos únics a revisar de "name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id"
municipis<- read.csv(file=file.path(arrelProjecte, "casos_municipis.csv"), check.names=FALSE)
municipis<- municipis[!municipis$nObjectes %in% c(NA, 0), ] # Selecciona municipis amb casos pendents
municipis[order(municipis$nCasos, decreasing=TRUE), ]

municipis$revisio<- generaRevisions_regexName(informes=municipis$informe, arrelProjecte=arrelProjecte,
                                              cerca=cerca, substitueix=substitueix, revisioUnificada=revisioUnificada)

# Reviseu i modifiqueu la revisió de l'informe que vulgueu de revisions/.
# Cal corregir els casos de name:ca i alt_name:ca incorrectes, esborrar-los o deixar-los en blanc si no és clar.
# És recomanable usar LibreOffice per evitar problemes de codificació i formats dels fitxers (https://www.softcatala.org/programes/libreoffice/)
# Moveu les revisions acabades a revisions/FET/.
# Si carregueu les edicions a OSM i actualitzeu els informes i revisions després d'acabar cada fitxer de revisió,
# és possible que es reutilitzin alguns casos a altres municipis. Podeu seguir l'script fins al final i tornar a
# començar per actualitzar els fitxers de revisions


## Fusiona les revisions fetes amb els informes i genera ordres per carregar-los a OSM ----
usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
fitxerContrasenya<- "" # camí a un fitxer amb una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;)

cmd<- preparaEdicions(arrelProjecte=arrelProjecte, usuari=usuari, fitxerContrasenya=fitxerContrasenya)
cmd<- na.omit(cmd)

## Afegeix paràmetres a les ordres. Veure «update_osm_objects_from_report --help» per les opcions de LangToolsOSM
nomMunicipi<- gsub(paste0(".+--input-file \\\"", arrelProjecte, "/edicions/informe-[A-z]+-|", sufixFitxers, ".tsv\\\".+"), "", cmd)
cmd<- paste0(cmd, " --no-interaction --changeset-hashtags \"#toponimsCat;#name_name:ca\"",
             " --batch 100 --changeset-comment \"Afegeix name:ca a partir de name per carrers, places, avingudes, camins, jardins,  passatges,  passeigs, rotondes, urbanitzacions a ", nomMunicipi, "\"")
cat(cmd, sep="\n")

## Executa les ordres
for (i in 1:length(cmd)){
  message(i, " / ", length(cmd), "\t", cmd[i])
  system(cmd[i])
}


## Arxiva els informes dels municipis actualitzades a edicions/FET i actualitza o elimina els informes originals desactualitzats ----
informesActualitzats<- actualitzaInformesCarregats(arrelProjecte=arrelProjecte, esborraInformesDesactualitzats=TRUE)


## Repassa Parcs a la Catalunya Nord ----
arrelProjecte<- "PPCC/name-name:ca"
actualitzaFitxer<- FALSE
## ULL VIU!: Parc col·lisionen amb el francés
filtre<- "nwr['name:ca'~'^[Pp]arc ']"
sufixFitxers<- "_name-name:ca_parcs-CatNord"
cerca<- ""
substitueix<- ""
revisioUnificada<- TRUE

cmd<- generaInforme(arrelProjecte=arrelProjecte, actualitzaFitxer=actualitzaFitxer,
                    fitxerInforme="informe-CatNord_name-name:ca_parcs.tsv", filtreArea="Catalunya del Nord", filtreObjectes=filtre)
system(cmd)
## CORREGIT!
