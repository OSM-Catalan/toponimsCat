library(toponimsCat)
library(pbapply)

## Copia name -> name:ca per valors inequivocament catalans

arrelProjecte<- "PPCC/name-name:ca"
actualitzaInformes<- FALSE # actualitzaInformes<- TRUE ## TODO: unificar parametre a generaInforme i generaInformesPPCC
cerca<- ""
substitueix<- ""
revisioUnificada<- TRUE
cl<- 40


## Cerca tasques ----
territoris<- generaInformesPPCC(arrelProjecte=arrelProjecte, filtre="nwr[name][!'name:ca']",
                                actualitzaInformes=actualitzaInformes, sufixFitxers="_cercaTasques",
                                divisions=toponimsCat::territoris)
## Executa les ordres
sel<- which(territoris$cmd != "")
territoris$name[sel]

pb<- timerProgressBar(max=length(sel))
for (i in 1:length(sel)){
  message("\n", i, " / ", length(sel), "\t", territoris$name[sel[i]])
  system(territoris$cmd[sel[i]])
  setTimerProgressBar(pb, i)
}
close(pb)
## Si hi ha errors (eg. overpass va massa enfeinat i no respon), torneu a correr la secció amb
actualitzaInformes<- FALSE


### Carrega fitxers ----
fitxers<- dir(paste0(arrelProjecte, "/informes"), "_cercaTasques.tsv$", full.names=TRUE)
nameL<- lapply(fitxers, function(x){
  d<- read.table(x, header=TRUE, sep="\t", quote="\"", skip=1, check.names=FALSE)
  grep("\"", d$name, value=TRUE, invert=TRUE) # TODO: omet noms que contenen cometes pq són problemàtics amb fitxers.csv
})
names(nameL)<- gsub("^PPCC/name-name:ca/informes/informe-|_cercaTasques.tsv$", "", fitxers)
nameV<- unlist(nameL)
sort(table(nameV), decreasing=TRUE)

cat_correccio<- hunspell::hunspell(nameV, dict=hunspell::dictionary(lang="ca"))
nameCat<- nameV[sapply(cat_correccio, length) == 0]
nNameCat<- sort(table(nameCat), decreasing=TRUE)
barplot(nNameCat[nNameCat > 10], decreasing=TRUE)
summary(as.numeric(nNameCat))
data.frame(minN=1:50, casos=sapply(1:50, function(x) length(nNameCat[nNameCat > x]))) # n casos > x
selMinN<- 10
selNames<- names(nNameCat)[nNameCat > selMinN]

LT_correccio<- pblapply(selNames, function(x){
  LanguageToolR::languagetool(x, encoding="utf-8", linebreak_paragraph=FALSE, language="ca", disabled_rules=c("UPPERCASE_SENTENCE_START"),
                              enabled_rules=c(), enabled_only=FALSE, disabled_categories=c(),
                              enabled_categories=c(), list_unknown=FALSE, apply=FALSE, quiet=TRUE)
}, cl=cl)
LT_correccio_df<- do.call(rbind, LT_correccio[sapply(LT_correccio, nrow) > 0])
if (!is.null(LT_correccio_df)){
  utils::write.table(LT_correccio_df, file=paste0(arrelProjecte, "/LanguageToolERRORS-selNamesCat.tsv"), sep="\t", na="", col.names=TRUE, row.names=TRUE)
}

selNames_LTbo<- selNames[sapply(LT_correccio, nrow) == 0]
setdiff(selNames, selNames_LTbo) # errors per LanguageTool
selNames_bo<- selNames_LTbo[grepl("^[0-9a-zàèéíïòóúüç,·-]+$", selNames_LTbo, ignore.case=TRUE)]
setdiff(selNames_LTbo, selNames_bo) # descartat

filtre<- paste0("nwr[name~'^(", paste(selNames_bo, collapse="|"), ")$'][!'name:ca']")
# save(filtre, selNames, selNames_bo, LT_correccio, file=paste0(arrelProjecte, "/filtre_pendents-PPCC-selNamesCorrectesLT.RData"), compress="xz")
load(paste0(arrelProjecte, "/filtre_pendents-PPCC-selNamesCorrectesLT.RData"), verbose=TRUE)  # filtre selNames selNames_bo LT_correccio


### Genera base de dades amb la primera paraula de name ----
generics<- sapply(strsplit(nameCat, " "), function(x) x[1])

nGenerics<- sort(table(generics), decreasing=TRUE)
bd<- data.frame(generic=names(nGenerics), n=as.numeric(nGenerics), catala=FALSE, castella=FALSE, frances=FALSE, italia=FALSE, stringsAsFactors=FALSE)

cat_correccio<- hunspell::hunspell(bd$generic, dict=hunspell::dictionary(lang="ca"))
bd$catala<- sapply(cat_correccio, length) == 0

es_correccio<- hunspell::hunspell(bd$generic, dict=hunspell::dictionary(lang="es_ES"))
bd$castella<- sapply(es_correccio, length) == 0

fr_correccio<- hunspell::hunspell(bd$generic, dict=hunspell::dictionary(lang="fr"))
bd$frances<- sapply(fr_correccio, length) == 0

it_correccio<- hunspell::hunspell(bd$generic, dict=hunspell::dictionary(lang="it_IT"))
bd$italia<- sapply(es_correccio, length) == 0

candidats<- bd[bd$catala, ]
# candidats<- candidats[apply(candidats[, c("castella", "frances", "italia")], 1, function(x) !any(x)), ]
paraula<- candidats$generic[!grepl("\"", candidats$generic)]  # TODO: omet noms que contenen cometes pq són problemàtics amb fitxers.csv
LT_correccio<- pblapply(paraula, function(x){
  LanguageToolR::languagetool(x, encoding="utf-8", linebreak_paragraph=FALSE, language="ca", disabled_rules=c("UPPERCASE_SENTENCE_START"),
                              enabled_rules=c(), enabled_only=FALSE, disabled_categories=c(),
                              enabled_categories=c(), list_unknown=FALSE, apply=FALSE, quiet=TRUE)
}, cl=cl)
LT_correccio_df<- do.call(rbind, LT_correccio[sapply(LT_correccio, nrow) > 0])
if (!is.null(LT_correccio_df)){
  utils::write.table(LT_correccio_df, file=paste0(arrelProjecte, "/LanguageToolERRORS-paraulesCat.tsv"), sep="\t", na="", col.names=TRUE, row.names=TRUE)
}

paraula_LTbo<- paraula[sapply(LT_correccio, nrow) == 0]
setdiff(paraula, paraula_LTbo) # errors per LanguageTool
paraula_bo<- paraula_LTbo[grepl("^[0-9a-zàèéíïòóúüç,·-]+$", paraula_LTbo, ignore.case=TRUE)]
setdiff(paraula_LTbo, paraula_bo) # descartat

filtre<- paste0("nwr[name~'^(", paste(paraula_bo, collapse="|"), ") '][!'name:ca']")
# save(filtre, paraula, paraula_bo, LT_correccio, bd, file=paste0(arrelProjecte, "/filtre_pendents-PPCC-paraulesCorrectes.RData"), compress="xz")
load(paste0(arrelProjecte, "/filtre_pendents-PPCC-paraulesCorrectes.RData"), verbose=TRUE)  # filtre paraula paraula_bo LT_correccio bd

# TODO: Clot"El Carrerou", "Le Carrerou, "La Carrerade", "La Carrerada" Clos Escala Pas Port Rambla Ronda Ruta + plurals
# Veure https://github.com/osm-bzh/osmbr-mapstyle/blob/master/osm_ca.yml per objectes prioritaris a traduïr
# https://osm-catalan.github.io/osmllengcat per veure el què falta
## TODO col·lisions amb el francés: clos i parc
## TODO col·lisionen amb el castellà: carretera |[Cc]atedral cresta Rambla Ribera Riera Ronda Ruta Via rotonda
# FET
# filtre<- "nwr[name~'^([Aa]vinguda|[Cc]arrer|[Cc]amí|[Pp]laça|[Pp]arc) '][!'name:ca']"; sufixFitxers<- "_name-name:ca"
# filtre<- "nwr[name~'^([Aa]tzucac|[Aa]vinguda|[Cc]amí|[Cc]aminal|[Cc]arreró|[Gg]iratori|[Jj]ardí|[Pp]assatge|[Pp]asseig|[Rr]otonda|[Uu]rbanització|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r1"
# filtre<- "nwr[name~'^([Aa]juntament|[Aa]tzucac|[Aa]vinguda|[Bb]osc|[Cc]amí|[Cc]aminal|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]orrec|[Gg]iratori|[Hh]ort[s]*|[Jj]ardí|[Mm]as|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]laça|[Pp]latja|[Pp]olígon]|[Rr]ec|[Rr]iera|[Rr]iu|[Uu]rbanització|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r2"
# filtre<- "nwr[name~'^([Aa]gulla|[Aa]juntament|[Aa]tzucac|[Aa]vinguda|[Bb]osc|[Cc]amí|[Cc]aminal|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]òrrec|[Ee]stany|[Gg]iratori|[Hh]ort[s]*|[Jj]ardí|[Ll]lac|[Mm]as|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]la|[Pp]laça|[Pp]latja|[Pp]olígon]|[Rr]ec|[Rr]iu|[Uu]rbanització|[Vv]eïnat|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r3"
# filtre<- "nwr[name~'^([Aa]gulla|[Aa]juntament|[Aa]tzucac|[Aa]utovia]|[Aa]vinguda|[Bb]osc|[Cc]amí|[Cc]aminal|[Cc]an|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]oll|[Cc]ollet|[Cc]òrrec|[Ee]stany|[Ff]ont|[Gg]iratori|[Hh]ort[s]*|[Jj]ardí|[Ll]lac|[Mm]as|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]la|[Pp]laça|[Pp]latja|[Pp]olígon]|[Pp]uig|[Rr]ec|[Rr]iu|[Ss]erra|[Ss]errat|[Tt]orrent|[Tt]uró|[Uu]rbanització|[Vv]eïnat|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r4"
# filtre<- "nwr[name~'^([Aa]gulla|[Aa]juntament|[Aa]tzucac|[Aa]utovia]|[Aa]vinguda|[Bb]arranc|[Bb]osc|[Cc]al|[Cc]amí|[Cc]aminal|[Cc]amp|[Cc]an|[Cc]arena|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]im|[Cc]oll|[Cc]ollet|[Cc]ol·legi|[Cc]oma|[Cc]òrrec|[Ee]scola|[Ee]stany|[Ff]ont|[Gg]iratori|[Hh]ort[s]*|[Jj]ardí|[Ll]lac|[Mm]as|[Oo]baga|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]la|[Pp]laça|[Pp]latja|[Pp]olígon]|[Pp]uig|[Rr]ec|[Rr]iu|[Ss]antuari|[Ss]èquia|[Ss]erra|[Ss]errat|[Tt]orrent|[Tt]uró|[Tt]ossal|[Uu]rbanització|[Vv]eïnat|[Vv]oral) '][!'name:ca']"; sufixFitxers<- "_name-name:ca_r5"
# i indica insensible a caixa. Evita [Aa]: nwr[name~'^(Atzucac||Jardí|Mas|Palau|Passatge|Passeig|Platja|Polígon|Riu|Urbanització|Voral) ', i][!'name:ca'](area.searchArea);

# PENDENT;
# filtre<- "nwr[name~'^([Aa]gulla|[Aa]juntament|[Aa]teneu|[Aa]tzucac|[Aa]utovia]|[Aa]vinguda|[Bb]ac|[Bb]aixada|[Bb]arranc|[Bb]assa|[Bb]osc|[Cc]abana|[Cc]al|[Cc]amí|[Cc]aminal|[Cc]amp|[Cc]an|[Cc]apella|[Cc]arena|[Cc]arrer|[Cc]arreró|[Cc]astell|[Cc]asal|[Cc]im|[Cc]oll|[Cc]ollet|[Cc]ol·legi|[Cc]oma|[Cc]òrrec|[Ee]scola|[Ee]sglésia|[Ee]stany|[Ff]ont|[Ff]orn|[Gg]iratori|[Hh]ort[s]*|[Ii]nstitut|[Jj]ardí|[Ll]lac|[Mm]as|[Mm]ola|[Oo]baga|[Pp]alau|[Pp]assatge|[Pp]asseig|[Pp]enya|[Pp]la|[Pp]laça|[Pp]latja|[Pp]olígon]|[Pp]uig|[Pp]ujada|[Rr]ec|[Rr]efugi|[Rr]iu|[Ss]antuari|[Ss]èquia|[Ss]ot|[Ss]erra|[Ss]errat|[Tt]orrent|[Tt]uró|[Tt]ossal|[Tt]uta|[Uu]niversitat|[Uu]rbanització|[Vv]all|[Vv]eïnat|[Vv]oral) '][!'name:ca']"


## Generar informes pels municipis dels PPCC amb LangToolsOSM ----
sufixFitxers<- "_name-name:ca"
# load(paste0(arrelProjecte, "/filtre_pendents-PPCC-paraulesCorrectes.RData"), verbose=TRUE)  # filtre paraula LT_correccio bd
# paraula_LTbo<- paraula[sapply(LT_correccio, nrow) == 0]
# setdiff(paraula, paraula_LTbo) # errors per LanguageTool
# paraula_bo<- paraula_LTbo[grepl("^[0-9a-zàèéíïòóúüç',·-]+$", paraula_LTbo, ignore.case=TRUE)]
# setdiff(paraula_LTbo, paraula_bo) # descartat
#
# filtre<- paste0("nwr[name~'^(", paste(paraula_bo, collapse="|"), ") '][!'name:ca']")

load(paste0(arrelProjecte, "/filtre_pendents-PPCC-selNamesCorrectesLT.RData"), verbose=TRUE)  # filtre selNames selNames_bo LT_correccio

selNamesBD<- data.frame(name=selNames, LT=sapply(LT_correccio, nrow) == 0,
                        selCaracters=grepl("^[0-9a-zàèéíïòóúüç',· -]+$", selNames, ignore.case=TRUE))
selNamesBD$seleccionat<- selNamesBD$LT
write.table(selNamesBD, file=paste0(arrelProjecte, "/candidats_pendents-PPCC-selNamesCorrectesLT.tsv"), sep="\t", na="", col.names=TRUE, row.names=FALSE)

selNamesBD<- read.table(paste0(arrelProjecte, "/candidats_pendents-PPCC-selNamesCorrectesLT_seleccionat.tsv"), header=TRUE, sep="\t", quote="\"", check.names=FALSE)

selNames_bo<- selNamesBD$name[selNamesBD$seleccionat]
setdiff(selNames, selNames_bo) # descartats
setdiff(selNames, selNamesBD$name[selNamesBD$selCaracters]) # descartat

# parse error: ',' or ']' expected - 'Ebre' found. .... C-12 Eix de l&quot;
## TODO: Cal escapar apostrofs. Elimina de moment
selNames_bo<- grep("'", selNames_bo, value=TRUE, invert=TRUE)
filtre<- paste0("nwr[name~'^(", paste(selNames_bo, collapse="|"), ")$'][!'name:ca']")


filtre<- "nwr[!'name:ca'][name]"

actualitzaInformes<- FALSE # actualitzaInformes<- TRUE

municipis<- generaInformesPPCC(arrelProjecte=arrelProjecte, filtre=filtre, actualitzaInformes=actualitzaInformes, sufixFitxers=sufixFitxers, divisions=toponimsCat::municipis)

## Executa les ordres
sel<- which(municipis$cmd != "")
municipis$`name:ca`[sel]

pb<- timerProgressBar(max=length(sel))
for (i in 1:length(sel)){
  message("\n", i, " / ", length(sel), "\t", municipis[["name:ca"]][sel[i]])
  system(municipis$cmd[sel[i]])
  setTimerProgressBar(pb, i)
}
close(pb)
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
fitxersRevisions<- dir(paste0(arrelProjecte, "/revisions"), paste0("revisio-UNIFICADA-.+", sufixFitxers, "\\.tsv$"), full.names=TRUE)
revisions_bones<- revisionsSenseErrors(fitxersRevisions=fitxersRevisions)

dL<- pblapply(revisions_bones, function(x){
  d<- utils::read.table(x, header=TRUE, sep="\t", quote="\"", check.names=FALSE)
  d<- d[grep("'", d$name), ]
  utils::write.table(d, file=gsub("\\.tsv$", "-ambApostrof.tsv", x), sep="\t", na="", col.names=TRUE, row.names=FALSE)
  d
})

d<- dL$`PPCC/name-name:ca/revisions/revisio-UNIFICADA-CatSud_name-name:ca.tsv`
LT_correccio<- pblapply(d$name, function(x){
  LanguageToolR::languagetool(as.character(x), encoding="utf-8", linebreak_paragraph=FALSE, language="ca", disabled_rules=c("UPPERCASE_SENTENCE_START"),
                              enabled_rules=c(), enabled_only=FALSE, disabled_categories=c(),
                              enabled_categories=c(), list_unknown=FALSE, apply=FALSE, quiet=TRUE)
}, cl=cl)
LT_correccio_df<- do.call(rbind, LT_correccio[sapply(LT_correccio, nrow) > 0])
if (!is.null(LT_correccio_df)){
  utils::write.table(LT_correccio_df, file=paste0(arrelProjecte, "/LanguageToolERRORS-CatSud_ambApostrof.tsv"), sep="\t", na="", col.names=TRUE, row.names=TRUE)
}

dLT<- data.frame(d, LT=sapply(LT_correccio, nrow) == 0,
                        selCaracters=grepl("^[0-9a-zàèéíïòóúüç',· -]+$", d$name, ignore.case=TRUE))
dLT$seleccionat<- dLT$LT
dLT<- merge(dLT, LT_correccio_df[, !grepl("(offset|length|context_text)$", names(LT_correccio_df))], by.x="name", by.y="sentence", all=TRUE)

write.table(dLT, file=paste0(arrelProjecte, "/CatSud_ambApostrof_CorrectesLT.tsv"), sep="\t", na="", col.names=TRUE, row.names=FALSE)

dLT<- read.table(paste0(arrelProjecte, "/revisions/revisio-UNIFICADA-CatSud_name-name:ca-CorrectesLT_ambApostrof.tsv"), header=TRUE, sep="\t", quote="\"", check.names=FALSE)

selNames_bo<- dLT$name[dLT$seleccionat]
setdiff(dLT$name, selNames_bo) # descartats
setdiff(dLT$name, dLT$name[dLT$selCaracters]) # descartat

# save(dLT, selNames_bo, LT_correccio, file=paste0(arrelProjecte, "/revisio-UNIFICADA-CatSud_name-name:ca-CorrectesLT_ambApostrof.RData"), compress="xz")
load(paste0(arrelProjecte, "/revisio-UNIFICADA-CatSud_name-name:ca-CorrectesLT_ambApostrof.RData"), verbose=TRUE)  # dLt selNames_bo LT_correccio


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
             " --batch 70 --changeset-comment \"Afegeix name:ca a partir de name en català amb apòstrofs segons hunspell, LanguageTool i revisió humana a ", nomMunicipi, "\"")
cat(cmd1, sep="\n")

## Executa les ordres
pb<- timerProgressBar(max=length(cmd1))
for (i in 1:length(cmd1)){
  message("\n", i, " / ", length(cmd1), "\t", cmd1[i])
  system(cmd1[i])
  setTimerProgressBar(pb, i)
}
close(pb)

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
