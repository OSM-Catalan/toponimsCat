library(toponimsCat)

## Copia name -> name:ca per valors inequivocament catalans

arrelProjecte<- "PPCC/name-name:ca"
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE ## TODO: unificar parametre a generaInforme i generaInformesPPCC
# TODO: "El Carrerou", "Le Carrerou, "La Carrerade", "La Carrerada" Clos Escala Pas Port Rambla Ronda Ruta + plurals
# Veure https://github.com/osm-bzh/osmbr-mapstyle/blob/master/osm_ca.yml per objectes prioritaris a traduïr
# https://osm-catalan.github.io/osmllengcat per veure el què falta
## ULL VIU!: clos i parc col·lisionen amb el francés. carretera Rambla Ribera Ronda Ruta Via rotonda col·lisionen amb el castellà.
# FET
# filtre<- "nwr[name~'^([Aa]vinguda|[Cc]arrer|[Cc]amí|[Pp]laça|[Pp]arc) '][!'name:ca']"; sufixFitxers<- "_name-name:ca"
# filtre<- "nwr[name~'^([Aa]tzucac|[Aa]vinguda|[Cc]amí|[Cc]aminal|[Cc]arreró|[Gg]iratori|[Jj]ardí|[Pp]assatge|[Pp]asseig|[Rr]otonda|[Uu]rbanització|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r1"
# filtre<- "nwr[name~'^([Aa]juntament|[Aa]tzucac|[Aa]vinguda|[Bb]osc|[Cc]amí|[Cc]aminal|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]orrec|[Gg]iratori|[Hh]ort[s]*|[Jj]ardí|[Mm]as|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]laça|[Pp]latja|[Pp]olígon]|[Rr]ec|[Rr]iera|[Rr]iu|[Uu]rbanització|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r2"
# filtre<- "nwr[name~'^([Aa]gulla|[Aa]juntament|[Aa]tzucac|[Aa]vinguda|[Bb]osc|[Cc]amí|[Cc]aminal|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]òrrec|[Ee]stany|[Gg]iratori|[Hh]ort[s]*|[Jj]ardí|[Ll]lac|[Mm]as|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]la|[Pp]laça|[Pp]latja|[Pp]olígon]|[Rr]ec|[Rr]iu|[Uu]rbanització|[Vv]eïnat|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r3"
# filtre<- "nwr[name~'^([Aa]gulla|[Aa]juntament|[Aa]tzucac|[Aa]utovia]|[Aa]vinguda|[Bb]osc|[Cc]amí|[Cc]aminal|[Cc]an|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]oll|[Cc]ollet|[Cc]òrrec|[Ee]stany|[Ff]ont|[Gg]iratori|[Hh]ort[s]*|[Jj]ardí|[Ll]lac|[Mm]as|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]la|[Pp]laça|[Pp]latja|[Pp]olígon]|[Pp]uig|[Rr]ec|[Rr]iu|[Ss]erra|[Ss]errat|[Tt]orrent|[Tt]uró|[Uu]rbanització|[Vv]eïnat|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r4"
# i indica insensible a caixa. Evita [Aa]: nwr[name~'^(Atzucac||Jardí|Mas|Palau|Passatge|Passeig|Platja|Polígon|Riu|Urbanització|Voral) ', i][!'name:ca'](area.searchArea);

# PENDENT;
filtre<- "nwr[name~'^([Aa]gulla|[Aa]juntament|[Aa]tzucac|[Aa]utovia]|[Aa]vinguda|[Bb]arranc|[Bb]osc|[Cc]al|[Cc]amí|[Cc]aminal|[Cc]amp|[Cc]an|[Cc]arena|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]im|[Cc]oll|[Cc]ollet|[Cc]ol·legi|[Cc]oma|[Cc]òrrec|[Ee]scola|[Ee]stany|[Ff]ont|[Gg]iratori|[Hh]ort[s]*|[Jj]ardí|[Ll]lac|[Mm]as|[Oo]baga|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]la|[Pp]laça|[Pp]latja|[Pp]olígon]|[Pp]uig|[Rr]ec|[Rr]iu|[Ss]antuari|[Ss]èquia|[Ss]erra|[Ss]errat|[Tt]orrent|[Tt]uró|[Tt]ossal|[Uu]rbanització|[Vv]eïnat|[Vv]oral) '][!'name:ca']"
sufixFitxers<- "_name-name:ca_r5"
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
municipis<- municipis[!is.na(municipis$`name:ca`), ]
municipis[intersect(order(municipis$nCasos, decreasing=TRUE), which(!municipis$revisat & municipis$nObjectes > 0)), ]

write.csv(municipis, file=file.path(arrelProjecte, "casos_municipis.csv"), row.names=FALSE)


## Edita les revisions ----
# les revisions són els casos únics a revisar de "name", "name:ca", "alt_name:ca", "alt_name", "translations", "ca.wikipedia_page", "wikidata_id"
municipis<- read.csv(file=file.path(arrelProjecte, "casos_municipis.csv"), check.names=FALSE)
municipis<- municipis[!municipis$nObjectes %in% c(NA, 0), ] # Selecciona municipis amb casos pendents
municipis[order(municipis$nCasos, decreasing=TRUE), ]

## Crea fitxers de revisió per zones amb idiomes diferents
municipis$revisio[municipis$regio == "CatNord"]<- generaRevisions_regexName(informes=municipis$informe[municipis$regio == "CatNord"],
                                                                            arrelProjecte=arrelProjecte, cerca=cerca, substitueix=substitueix, revisioUnificada=revisioUnificada,
                                                                            nomFitxerUnificat=paste0("revisio-UNIFICADA-CatNord", sufixFitxers, ".tsv"))
municipis$revisio[municipis$regio == "Franja"]<- generaRevisions_regexName(informes=municipis$informe[municipis$regio == "Franja"],
                                                                            arrelProjecte=arrelProjecte, cerca=cerca, substitueix=substitueix, revisioUnificada=revisioUnificada,
                                                                            nomFitxerUnificat=paste0("revisio-UNIFICADA-Franja", sufixFitxers, ".tsv"))
municipis$revisio[municipis$regio == "Sardenya"]<- generaRevisions_regexName(informes=municipis$informe[municipis$regio == "Sardenya"],
                                                                            arrelProjecte=arrelProjecte, cerca=cerca, substitueix=substitueix, revisioUnificada=revisioUnificada,
                                                                            nomFitxerUnificat=paste0("revisio-UNIFICADA-Sardenya", sufixFitxers, ".tsv"))
municipis$revisio[!municipis$regio %in% c("CatNord", "Franja", "Sardenya")]<- generaRevisions_regexName(informes=municipis$informe[!municipis$regio %in% c("CatNord", "Franja", "Sardenya")],
                                                                            arrelProjecte=arrelProjecte, cerca=cerca, substitueix=substitueix, revisioUnificada=revisioUnificada,
                                                                            nomFitxerUnificat=paste0("revisio-UNIFICADA-CatSud", sufixFitxers, ".tsv"))

# Reviseu i modifiqueu la revisió de l'informe que vulgueu de revisions/.
# Cal corregir els casos de name:ca i alt_name:ca incorrectes, esborrar-los o deixar-los en blanc si no és clar.
# És recomanable usar LibreOffice per evitar problemes de codificació i formats dels fitxers (https://www.softcatala.org/programes/libreoffice/)
# Moveu les revisions acabades a revisions/FET/.
# Si carregueu les edicions a OSM i actualitzeu els informes i revisions després d'acabar cada fitxer de revisió,
# és possible que es reutilitzin alguns casos a altres municipis. Podeu seguir l'script fins al final i tornar a
# començar per actualitzar els fitxers de revisions


## Fitxers amb revisions sense error ortogràfics ----
revisions_bones<- revisionsSenseErrors(fitxersRevisions=unique(municipis$revisio))
revisions_bones<- revisionsSenseErrors(fitxersRevisions=unique(municipis$revisio), sufix_nou="_correcte-LT.tsv", LanguageTool=TRUE)

## Revisions duplicades
duplicats<- attributes(bdRevisions(arrelProjectes=arrelProjecte))$duplicats
duplicats[, grep("name", names(duplicats))] ## arregla


## Fusiona les revisions fetes amb els informes i genera ordres per carregar-los a OSM ----
usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
fitxerContrasenya<- "" # camí a un fitxer amb una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;)

cmd<- preparaEdicions(arrelProjecte=arrelProjecte, usuari=usuari, fitxerContrasenya=fitxerContrasenya)
cmd<- na.omit(cmd)

## Afegeix paràmetres a les ordres. Veure «update_osm_objects_from_report --help» per les opcions de LangToolsOSM
nomMunicipi<- gsub(paste0(".+--input-file \\\"", arrelProjecte, "/edicions/informe-[A-z]+-|", sufixFitxers, ".tsv\\\".+"), "", cmd)
cmd1<- paste0(cmd, " --no-interaction --changeset-hashtags \"#toponimsCat;#name_name:ca\" --changeset-source \"name tag\"", #  --no-interaction
             " --batch 70 --changeset-comment \"Afegeix name:ca a partir de name per barrancs, escoles, col·legis i tossals a ", nomMunicipi, " (LTv5.9)\"")
cat(cmd1, sep="\n")

## Executa les ordres
for (i in 1:length(cmd)){
  message(i, " / ", length(cmd1), "\t", cmd1[i])
  system(cmd1[i])
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
