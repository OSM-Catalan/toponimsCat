library(toponimsCat)

## Copia name -> name:ca per valors inequivocament catalans

arrelProjecte<- "PPCC/correccions-name:ca"
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE ## TODO: unificar parametre a generaInforme i generaInformesPPCC
# TODO: Atzucac Avinguda Camí Caminal Carrer Carreró Carretera Clos Giratori Jardí Passatge Passeig Plaça Rambla Ronda Ruta Rotonda Urbanització Via Voral
## ULL VIU!: clos i parc col·lisionen amb el francés. carretera col·lisiona amb el castellà.
filtre<- "nwr['name:ca'~'([Aa]venida|[Cc]alle|[Cc]amino|[Pp]arque|[Pp]laza) ']"
sufixFitxers<- "_correccions-name:ca"
cerca<- ""
substitueix<- ""
revisioUnificada<- TRUE


## Generar informes per les comarques dels PPCC amb LangToolsOSM ----
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE

comarques<- generaInformesPPCC(arrelProjecte=arrelProjecte, filtre=filtre, actualitzaInformes=actualitzaInformes, sufixFitxers=sufixFitxers)

## Executa les ordres
sel<- which(comarques$cmd != "")
comarques$`name:ca`[sel]

for (i in 1:length(sel)){
  message(i, " / ", length(sel), "\t", comarques[["name:ca"]][sel[i]])
  system(comarques$cmd[sel[i]])
}

## Si hi ha errors (eg. overpass va massa enfeinat i no respon), torneu a correr la secció amb
actualitzaInformes<- FALSE


## Avalua el nombre de casos a cada informe generat ----
comarques<- recompteCasos(dades=comarques)
ordCasos<- order(comarques$revisat, comarques$nCasos, comarques$`name:ca`,
                 decreasing=c(FALSE, TRUE, FALSE), method="radix")
ordCol<- c("name:ca", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat")
ordCol<- c(ordCol, setdiff(names(comarques), c(ordCol, "cmd")))
comarques<- comarques[ordCasos, ordCol]
comarques[intersect(order(comarques$nCasos, decreasing=TRUE), which(!comarques$revisat & comarques$nObjectes > 0)), ]

write.csv(comarques, file=file.path(arrelProjecte, "casos_comarques.csv"), row.names=FALSE)


## Edita les revisions ----
# les revisions són els casos únics a revisar de "name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id"
comarques<- read.csv(file=file.path(arrelProjecte, "casos_comarques.csv"), check.names=FALSE)
comarques<- comarques[!comarques$nObjectes %in% c(NA, 0), ] # Selecciona comarques amb casos pendents
comarques[order(comarques$nCasos, decreasing=TRUE), ]

comarques$revisio<- generaRevisions_regexName(informes=comarques$informe, arrelProjecte=arrelProjecte,
                                              cerca=cerca, substitueix=substitueix, revisioUnificada=revisioUnificada)

# Reviseu i modifiqueu la revisió de l'informe que vulgueu de revisions/.
# Cal corregir els casos de name:ca i alt_name:ca incorrectes, esborrar-los o deixar-los en blanc si no és clar.
# És recomanable usar LibreOffice per evitar problemes de codificació i formats dels fitxers (https://www.softcatala.org/programes/libreoffice/)
# Moveu les revisions acabades a revisions/FET/.
# Si carregueu les edicions a OSM i actualitzeu els informes i revisions després d'acabar cada fitxer de revisió,
# és possible que es reutilitzin alguns casos a altres comarques. Podeu seguir l'script fins al final i tornar a
# començar per actualitzar els fitxers de revisions


## Fusiona les revisions fetes amb els informes i genera ordres per carregar-los a OSM ----
usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
fitxerContrasenya<- "" # camí a un fitxer amb una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;)

cmd<- preparaEdicions(arrelProjecte=arrelProjecte, usuari=usuari, fitxerContrasenya=fitxerContrasenya)
cmd<- na.omit(cmd)

## Afegeix paràmetres a les ordres. Veure «update_osm_objects_from_report --help» per les opcions de LangToolsOSM
nomComarca<- gsub(paste0(".+--input-file \\\"", arrelProjecte, "/edicions/informe-[A-z]+-|", sufixFitxers, ".tsv\\\".+"), "", cmd)
cmd<- paste0(cmd, " --changeset-hashtags \"#toponimsCat;#correccions_name:ca\"",
             " --batch 100 --changeset-comment \"Afegeix name:ca a partir de name per carrers, places, parcs, avingudes i camins a ", nomComarca, "\"")
cat(cmd, sep="\n")

## Executa les ordres: millor en una terminal per processos interactius

# for (i in 1:length(cmd)){
#   message(i, " / ", length(cmd), "\t", cmd[i])
#   system(cmd[i])
# }


## Arxiva els informes de les comarques actualitzades a edicions/FET i actualitza o elimina els informes originals desactualitzats ----
informesActualitzats<- actualitzaInformesCarregats(arrelProjecte=arrelProjecte, esborraInformesDesactualitzats=TRUE)
