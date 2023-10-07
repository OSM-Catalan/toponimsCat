library(icgc.osm)

arrelProjecte<- "PPCC/ICGC_NGCat-name:ca"
dir.create(file.path(arrelProjecte, "edicions", "FET"), showWarnings=FALSE, recursive=TRUE)
usuari<- "jmaspons" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
fitxerContrasenya<- "~/.osm/motdepas.txt"

# usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
# fitxerContrasenya<- "" # camí a un fitxer amb una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;)


## Selecció d'associacions per name:ca ----
# afegeix els name:ca buits i que concorden amb la toponímia de l'NGCat
nameca<- unique(osm_NGCat[!is.na(osm_NGCat$`name:ca`), c("name", "name:ca", "osm_id", "osm_type", "osm_idMun", "osm_idCom")])
table(is.na(nameca$osm_idCom))
table(is.na(nameca$osm_idMun))

nameca.com<- unique(merge(nameca, tesaurus_comarques[, c("osm_id", "osm_name")], by.x="osm_idCom", by.y="osm_id"))
names(nameca.com)<- gsub("^osm_name$", "osm_nameCom", names(nameca.com))

cmd<- by(nameca.com, nameca.com$osm_nameCom, function(x){
  comarca<- unique(x$osm_nameCom)
  fitxer<- file.path(arrelProjecte, "edicions", paste0("informe-", comarca, "_NGCat-name:ca.tsv"))
  write.table(x[order(x$osm_idMun), ], file=fitxer, sep="\t", na="", col.names=FALSE, row.names=FALSE)

  cmd<- paste0('update_osm_objects_from_report --username ', usuari, ' --passwordfile ', fitxerContrasenya)
  cmd<- paste0(cmd, " --no-interaction --changeset-hashtags \"#toponimsCat;#OSM-ICGC;#ICGC-NGCat_name:ca\" --changeset-source \"ICGC NGCat\"",
         " --batch 150 --changeset-comment \"Afegeix name:ca a partir de name que concorden amb les dades de l'ICGC (NGCat v10cs0f1r011) a ", comarca, "\"")
  cmd<- paste0(cmd, ' -v --confirmed-edits --confirm-overwrites --input-file "', fitxer, '" name:ca')

  return (cmd)
})

cmd<- as.character(cmd)
cmd<- na.omit(cmd)
cat(cmd)
## Afegeix paràmetres a les ordres. Veure «update_osm_objects_from_report --help» per les opcions de LangToolsOSM
nomMunicipi<- gsub(paste0(".+--input-file \\\"", arrelProjecte, "/edicions/informe-[A-zàèéíïòóúüç·' ]+-|", sufixFitxers, ".tsv\\\".+"), "", cmd)
cmd1<-
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
