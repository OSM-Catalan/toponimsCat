library(toponimsCat)
library(pbapply)

municipisL<- lapply(dir("PPCC", "municipis_.+\\.csv", full.names = TRUE), function(x){
  d<- read.csv2(x, check.names=FALSE)
  d$regio<- gsub("^PPCC/municipis_|\\.csv$", "", x)
  d
})
municipis<- do.call(rbind, municipisL)
table(municipis$regio)
table(comarques$regio)
comarques<- comarques

names(municipis)<- gsub("@", "", names(municipis))
municipis<- municipis[, c("type", "id", "name", "name:ca", "regio", "admin_level", "wikidata")]

municipis$regio[municipis$regio %in% "Balears"]<- "Illes"

save(municipis, file="data/municipisPPCC.RData", compress="xz")
write.csv(municipis, file="PPCC/municipis.csv", row.names = FALSE)
table(duplicated(municipis[, c("name:ca", "regio")]))


## Arregla fitxers municipi-id -> regio-municipi ----

ori<- dir("PPCC/name-name:ca", "\\.tsv$", recursive = TRUE, full.names = TRUE)
table(dirname(ori))

bd<- municipis
bd$ori<- paste0("-", bd$`name:ca`, "-", bd$id, "_")
bd$nou<- paste0("-", bd$regio, "-", bd$`name:ca`, "_")
table(duplicated(bd$nou))

nou<- ori
for (i in 1:length(bd$ori)){
  nou<- gsub(bd$ori[i], bd$nou[i], nou)
}

data.frame(ori, nou)

file.rename(ori, nou)

write.csv(territoris, file="PPCC/territoris.csv", row.names = FALSE, quote = FALSE)

save(territoris, file = "data/territoris.RData", compress = "xz")


## PENDENT: Afegeix comarca als municipis ----
arrelProjecte<- "PPCC/divisions"
actualitzaInformes<- FALSE
sufixFitxers<- "_comarca-municipis"


unique(municipis[, c("regio", "admin_level")])
unique(comarques[, c("regio", "admin_level", "historic:admin_level")])
comarques_admin_levelX<- comarques[comarques$regio %in% c("Andorra", "Múrcia"), ]
municipis_admin_levelX<- municipis[municipis$regio %in% c("Andorra", "Múrcia"), ]

filtreAnd<- "relation[admin_level=7]"
filtreCatNord<- "relation['historic:admin_level'=7]"
filtre8<- "relation[admin_level=8][boundary=administrative]"

comarques_admin_level8<- generaInformesPPCC(arrelProjecte=arrelProjecte, filtre=filtre8,
                                            actualitzaInformes=actualitzaInformes, sufixFitxers=sufixFitxers,
                                            divisions=comarques[!comarques$regio %in% c("Andorra", "Múrcia"), ])

## Executa les ordres
sel<- which(comarques_admin_level8$cmd != "")
comarques_admin_level8$`name:ca`[sel]

pb<- timerProgressBar(max=length(sel))
for (i in 1:length(sel)){
  message("\n", i, " / ", length(sel), "\t", comarques_admin_level8[["name:ca"]][sel[i]])
  system(comarques_admin_level8$cmd[sel[i]])
  setTimerProgressBar(pb, i)
}
close(pb)


dL<- lapply(comarques_admin_level8$informe, function(x){
  read.table(x, header=TRUE, sep="\t", quote="\"", skip=1, check.names=FALSE)
})
names(dL)<- gsub("PPCC/divisions/informes/informe-|_comarca-municipis.tsv", "", comarques_admin_level8$informe)

municipisL<- mapply(function(d, comarca){
  # intersect(names(municipis), names(x))
  # setdiff(names(municipis), names(x))
  # setdiff(names(x), names(municipis))
  d<- merge(municipis, d[, c("typeOSM", "idOSM")], by.x=c("type", "id"), by.y=c("typeOSM", "idOSM"))
  if (nrow(d) > 0){
    d$comarca<- comarca
    d$comarca.id<- comarques$id[comarques$`name:ca` == comarca]
  }
  d
}, d=dL, comarca=gsub("[A-z]+-", "", names(dL)))

municipis_admin_levelX$comarca<- municipis_admin_levelX$regio

municipis_admin_levelXL<- split(municipis_admin_levelX, municipis_admin_levelX$regio)

municipis_admin_levelXL$Múrcia$comarca<- municipis_admin_levelXL$Múrcia$`name:ca`
municipis_admin_levelXL$Múrcia$comarca.id<- municipis_admin_levelXL$Múrcia$id
municipis_admin_levelXL$Andorra$comarca.id<- territoris$id[territoris$name == "Andorra"]

municipis<- do.call(rbind, c(municipisL, municipis_admin_levelXL))

ordCols<- c("name:ca", "regio", "comarca")
ordCols<- c(ordCols, setdiff(names(municipis), ordCols))
# municipis<- municipis[order(municipis$regio, municipis$`name:ca`), ordCols]
municipis<- municipis[order(municipis$regio, municipis$comarca, municipis$`name:ca`), ordCols]
rownames(municipis)<- NULL

municipisOri<- toponimsCat::municipis
nrow(municipis); nrow(municipisOri)
ordCols<- c("name:ca", "regio")
ordCols<- c(ordCols, setdiff(names(municipisOri), ordCols))
municipisOri<- municipisOri[order(municipisOri$regio, municipisOri$`name:ca`), ordCols]

compareDF::view_html(compareDF::compare_df(municipis[, !grepl("comarca", names(municipis))], municipisOri, group_col="id"))
setdiff(municipis$`name:ca`, toponimsCat::municipis$`name:ca`)
dbTools::duplicatedPK(municipis, pk="id")

municipisEliminar<- municipis[c("473", "1145", "1672"), ] # duplicats amb comarques equivocades. Eliminar
claus<- do.call(paste, municipis[, c("id", "comarca")])
clausEliminar<- do.call(paste, municipisEliminar[, c("id", "comarca")])
municipis0<- municipis[!claus %in% clausEliminar, ]

nrow(municipis0); nrow(municipisOri); nrow(municipis)
compareDF::view_html(compareDF::compare_df(municipis0[, !grepl("comarca", names(municipis))], municipisOri, group_col="id"))
compareDF::view_html(compareDF::compare_df(municipis0, municipis, group_col="id"))
## CONCLUSIONS: sobre Montalban del Castell i Bassella, Xàtiva i Llívia es dupliquen perquè quadren amb 2 comarques
# municipisPerduts<- municipisOri[municipisOri$id %in% c(340178), ] # , 1919933, 40581
# municipis[municipis$id %in% c(340178), ] # , 1919933, 40581
#
# comarques[comarques$regio == "CatNord", c("name:ca", "id")]
# comarques[comarques$`name:ca` == "l'Alcalatén", c("name:ca", "id")]
# municipisPerduts$comarca<- c("l'Alcalatén") # "Fenolleda", "Sàsser",)
# municipisPerduts$comarca.id<- c(1111146) # NA_integer_, , 275098)

# municipis1<- rbind(municipis0, municipisPerduts)


ordCols<- c("name:ca", "regio", "comarca")
ordCols<- c(ordCols, setdiff(names(municipis), ordCols))
# municipis<- municipis[order(municipis$regio, municipis$`name:ca`), ordCols]
municipis<- municipis[order(municipis$regio, municipis$comarca, municipis$`name:ca`), ordCols]

m<- merge(municipis, municipisOri, all=TRUE)
m[is.na(m$comarca), ]

m0<- merge(municipis0, municipisOri, all=TRUE)
m0[is.na(m0$comarca), ]

# m1<- merge(municipis1, municipisOri, all=TRUE)
# m1[is.na(m1$comarca), ]

mL<- list(municipisOri, municipis, municipis0, m, m0)
names(mL)<- c("municipisOri", "municipis", "municipis0", "m", "m0")
sapply(mL, nrow)

library(data.table)
mL<- lapply(mL, unique)
mDTL<- lapply(mL, as.data.table)
municipisOriDT<- as.data.table(municipisOri)

lapply(mDTL, function(x){
  list(ori_nou=fsetdiff(municipisOriDT, x[, .SD, .SDcols=names(municipisOriDT)]),
       nou_ori=fsetdiff(x[, .SD, .SDcols=names(municipisOriDT)], municipisOriDT))
})
lapply(mDTL[-1], function(x){
  x[is.na(x$comarca), ]
})
lapply(mDTL[-1], function(x){
  x[is.na(x$comarca), ]
})
lapply(mDTL, function(x){
  dbTools::duplicatedPK(x, pk="id")
})
sapply(mDTL, nrow)
municipis1[is.na(municipis1$comarca), ]
municipis[is.na(municipis$comarca), ]
sapply(mDTL, nrow)
all.equal(municipis0, municipis1)
## CONCLUSIONS: Treure Montalban del Castell dels PPCC (comarca Fenolleda, id:1919933)
## municipis0 i son correctes
compareDF::view_html(compareDF::compare_df(municipis0, municipis1, group_col="id"))
nrow(municipis0)

municipis<- municipis0

save(municipis, file="data/municipisPPCC.RData", compress="xz")


## Ordena columnes ----
primers<- c("name:ca", "regio", "comarca", "id", "type", "wikidata", "type")
municipis<- toponimsCat::municipis
comarques<- toponimsCat::comarques
territoris<- toponimsCat::territoris
PPCC<- toponimsCat::PPCC

ord_municipis<- c(intersect(primers, names(municipis)))
ord_municipis<- c(ord_municipis, setdiff(names(municipis), ord_municipis))

ord_comarques<- c(intersect(primers, names(comarques)))
ord_comarques<- c(ord_comarques, setdiff(names(comarques), ord_comarques))

ord_territoris<- c(intersect(primers, names(territoris)))
ord_territoris<- c(ord_territoris, setdiff(names(territoris), ord_territoris))

ord_PPCC<- c(intersect(primers, names(PPCC)))
ord_PPCC<- c(ord_PPCC, setdiff(names(PPCC), ord_PPCC))

municipis<- municipis[, ord_municipis]
save(municipis, file="data/municipisPPCC.RData", compress="xz")

comarques<- comarques[, ord_comarques]
save(comarques, file="data/comarquesPPCC.RData", compress="xz")

territoris<- territoris[, ord_territoris]
save(territoris, file="data/territorisPPCC.RData", compress="xz")

PPCC<- PPCC[, ord_PPCC]
save(PPCC, file="data/PPCC.RData", compress="xz")

names(municipis)
names(comarques)
names(territoris)
names(PPCC)


## Actualitza comarques del Païs Valencià ----
# https://www.vilaweb.cat/noticies/noves-comarques-pais-valencia-mapa/
# Fusiona "l'Horta Oest" amb "l'Horta Sud"
actualitzaComarques<- comarques[comarques$regio == "PV", ]
actualitzaComarques<- comarques[comarques$`name:ca` %in% grep("Alcalaten|Horta", comarques$`name:ca`, value=TRUE), ]
eliminaComarques<- comarques[comarques$`name:ca` == "l'Horta Oest", ]
comarques<- comarques[comarques$id != eliminaComarques$id, ]
# save(comarques, file = "data/comarquesPPCC.RData", compress="xz")
municipis[municipis$comarca == "l'Horta Oest", ]
# CONCLUSIÓ: tots els municipis de l'Horta Oest ja tenen la comarca correcta

# Vilafranca passa del Maestrat als Ports; la Serratella, de la Plana Alta a l’Alt Maestrat, i Benafigos i Atzeneta del Maestrat passen de l’Alcalatén a l’Alt Maestrat
# Vilafranca passa als Ports. Vistabella del Maestrat, Atzeneta, Benafigos i la Serratella passen a l'Alt Maestrat

municipis<- toponimsCat::municipis
actualitzaMunicipis<- municipis[municipis$comarca %in% c("els Ports", "l'Alt Maestrat", "l'Alcalatén") & toponimsCat::municipis$regio == "PV", ]
Ports<- comarques[comarques$`name:ca` == "els Ports", c("name:ca", "id")]
AltMaestrat<- comarques[comarques$`name:ca` == "l'Alt Maestrat", c("name:ca", "id")]

# Cap als Ports
municipis[municipis$`name:ca` == "Vilafranca", c("comarca", "comarca.id")]
# Cap a l'Alt Maestrat
municipis[municipis$`name:ca` == "la Serratella", c("comarca", "comarca.id")]
municipis[municipis$`name:ca` == "Benafigos", c("comarca", "comarca.id")]
municipis[municipis$`name:ca` == "Atzeneta del Maestrat", c("comarca", "comarca.id")]

# CANVIA
# Cap als Ports
municipis[municipis$`name:ca` == "Vilafranca", c("comarca", "comarca.id")]<- Ports
# Cap a l'Alt Maestrat
municipis[municipis$`name:ca` == "la Serratella", c("comarca", "comarca.id")]<- AltMaestrat
municipis[municipis$`name:ca` == "Benafigos", c("comarca", "comarca.id")]<- AltMaestrat
municipis[municipis$`name:ca` == "Atzeneta del Maestrat", c("comarca", "comarca.id")]<- AltMaestrat

# save(municipis, file="data/municipisPPCC.RData", compress = "xz")
