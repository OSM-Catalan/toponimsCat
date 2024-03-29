library(toponimsCat)

## Cerca i corregeix errors a name:ca i alt_name:ca

arrelProjecte<- "PPCC/correccions-name:ca"
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE ## TODO: unificar parametre a generaInforme i generaInformesPPCC
etiquetes<- c("name", "alt_name", "name:ca", "alt_name:ca")

# TODO: diferències entre name i name:ca per name amb patró català
# TODO: Canigò|Canigo -> Canigó. Debat sobre Canigou a OSM-66
## FET:
# filtre<- "nwr['name:ca'~'([Aa]venida|[Cc]alle|[Cc]amino|[Pp]arque|[Pp]laza) ']"
# filtre<- "nwr['alt_name:ca'~'([Aa]venida|[Cc]alle|[Cc]amino|[Pp]arque|[Pp]laza) ']"
# Moli-> Molí i cami -> camí
sufixFitxers<- "_correccions-moli_cami"; filtre<- paste0("nwr['", etiquetes, "'~'( |^)([Cc]ami|[Mm]oli)( |$)']")
cerca<- "([Mm])oli"
substitueix<- "\\1olí"

# Ela geminada
sufixFitxers<- "_correccions-ela_geminada"; filtre<- paste0("(", paste0("nwr['", etiquetes, "'~'l[.-_*]l'] ", collapse="(area.divisio); "), ")")
cerca<- "l(\\.|-|_|\\*)l"
substitueix<- "l·l"



## Generar informes pels PPCC amb LangToolsOSM ----
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE

ppcc<- generaInformesPPCC(arrelProjecte=arrelProjecte, filtre=filtre, actualitzaInformes=actualitzaInformes, sufixFitxers=sufixFitxers, divisions=PPCC)
comarques<- generaInformesPPCC(arrelProjecte=arrelProjecte, filtre=filtre, actualitzaInformes=actualitzaInformes, sufixFitxers=sufixFitxers)

## TODO: arregla hack per correccions d'ela geminada. osmdata pot ajudar quan estigui alliberada nova versió
ppcc$cmd<- gsub(")(area.divisio)", "(area.divisio);)", ppcc$cmd, fixed = TRUE)

divisions<- ppcc

## Executa les ordres
sel<- which(divisions$cmd != "")
divisions$`name:ca`[sel]

pb<- timerProgressBar(max=length(sel))
for (i in 1:length(sel)){
  message("\n", i, " / ", length(sel), "\t", divisions[["name:ca"]][sel[i]])
  system(divisions$cmd[sel[i]])
  setTimerProgressBar(pb, i)
}
close(pb)

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
                                              cerca=cerca, substitueix=substitueix, revisioUnificada=revisioUnificada,
                                              nomFitxerUnificat=paste0("revisio-UNIFICADA", sufixFitxers, ".tsv"))

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
             " --batch 100 --changeset-comment \"Corregeix errors ortogràfics de name:ca a ", nomComarca, "\"")
cat(cmd, sep="\n")

## Executa les ordres: millor en una terminal per processos interactius

# for (i in 1:length(cmd)){
#   message(i, " / ", length(cmd), "\t", cmd[i])
#   system(cmd[i])
# }


## Arxiva els informes de les comarques actualitzades a edicions/FET i actualitza o elimina els informes originals desactualitzats ----
informesActualitzats<- actualitzaInformesCarregats(arrelProjecte=arrelProjecte, esborraInformesDesactualitzats=TRUE)
