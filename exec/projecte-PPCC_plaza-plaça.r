library(toponimsCat)


arrelProjecte<- "PPCC/plaza-plaça"
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE
filtre<- "nwr[name~'^[Pp]laza'][!'name:ca']"
sufixFitxers<- "_name-plaza"
cerca<- "([Pp])laza "
substitueix<- "\\1laça "
revisioUnificada<- TRUE

## Generar informes per comarques dels PPCC amb LangToolsOSM ----
comarques<- generaInformesPPCC(arrelProjecte=arrelProjecte, filtre=filtre,
                               actualitzaInformes=actualitzaInformes, sufixFitxers=sufixFitxers)

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
comarques<- recompteCasosInformes(dades=comarques)
ordCasos<- order(comarques$revisat, comarques$nCasos, comarques$regio, comarques$`name:ca`,
                 decreasing=c(FALSE, TRUE, FALSE, FALSE), method="radix")
ordCol<- c("regio", "name:ca", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat")
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
                                              nomFitxerUnificat=paste0("revisio-PPCC", sufixFitxers, ".tsv"))

# Reviseu i modifiqueu la revisió de l'informe que vulgueu de revisions/.
# Cal corregir els casos de name:ca i alt_name:ca incorrectes, esborrar-los o deixar-los en blanc si no és clar.
# És recomanable usar LibreOffice per evitar problemes de codificació i formats dels fitxers (https://www.softcatala.org/programes/libreoffice/)
# Moveu les revisions acabades a revisions/FET/.
# Si carregueu les edicions a OSM i actualitzeu els informes i revisions després d'acabar cada fitxer de revisió,
# és possible que es reutilitzin alguns casos a altres comarques. Podeu seguir l'script fins al final i tornar a
# començar per actualitzar els fitxers de revisions


## Reutilitza les revisions d'altres projectes ----
bdRevs<- bdRevisions(arrelProjectes="PPCC")
attributes(bdRevs)$duplicats

bdRevs[, c("name", "alt_name")]<- lapply(bdRevs[, c("name", "alt_name")], function(x){
  x<- gsub("^(avenida|calle|camino|parque|plaza) ", "plaza ", x)
  x<- gsub("^(Avenida|Calle|Camino|Parque|Plaza) ", "Plaza ", x)
  x
})
bdRevs[, c("name:ca", "alt_name:ca")]<- lapply(bdRevs[, c("name:ca", "alt_name:ca")], function(x){
  x<- gsub("^(avinguda|carrer|camí|parc|plaça) ", "plaça ", x)
  x<- gsub("^(Avinguda|Carrer|Camí|Parc|Plaça) ", "Plaça ", x)
  x
})
bdRevs<- unique(bdRevs)

revisio<- read.table(file.path(arrelProjecte, "revisions", paste0("revisio-PPCC", sufixFitxers, ".tsv")), header=TRUE, sep="\t", quote="\"", check.names=FALSE, na.strings="")

revisioR<- merge(bdRevs, revisio[, setdiff(names(revisio), c("name:ca", "alt_name:ca"))])[, names(bdRevs)]
revisioR<- unique(revisioR)

write.table(revisioR, file.path(arrelProjecte, "revisions", paste0("revisio-PPCC_reutilitzat", sufixFitxers, ".tsv")), sep="\t", na="", col.names=TRUE, row.names=FALSE)


## Fusiona les revisions fetes amb els informes i genera ordres per carregar-los a OSM ----
usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
fitxerContrasenya<- "" # camí a un fitxer amb una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;)

cmd<- preparaEdicions(arrelProjecte=arrelProjecte, usuari=usuari, fitxerContrasenya=fitxerContrasenya)
cmd<- na.omit(cmd)

## Afegeix paràmetres a les ordres. Veure «update_osm_objects_from_report --help» per les opcions de LangToolsOSM
nomComarca<- gsub(paste0(".+--input-file \\\"", arrelProjecte, "/edicions/informe-|", sufixFitxers, ".tsv\\\".+"), "", cmd)
cmd<- paste0(cmd, " --changeset-hashtags \"#toponimsCat;#plaza_plaça\"",
            " --batch 100 --changeset-comment \"Afegeix name:ca a places de ", nomComarca, "\"")
cat(cmd, sep="\n")

## Executa les ordres
for (i in 1:length(cmd)){
  message(i, " / ", length(cmd), "\t", cmd[i])
  system(cmd[i])
}


## Arxiva els informes de les comarques actualitzades a edicions/FET i actualitza o elimina els informes originals desactualitzats ----
informesActualitzats<- actualitzaInformesCarregats(arrelProjecte=arrelProjecte, esborraInformesDesactualitzats=FALSE)
