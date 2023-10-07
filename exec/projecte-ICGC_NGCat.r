library(icgc.osm)
library(pbapply)

arrelProjecte<- "PPCC/ICGC_NGCat-name:ca"
dir.create(file.path(arrelProjecte, "edicions", "FET"), showWarnings=FALSE, recursive=TRUE)
usuari<- "jmaspons" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
fitxerContrasenya<- "~/.osm/motdepas.txt"

# usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
# fitxerContrasenya<- "" # camí a un fitxer amb una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;)


## Selecció d'associacions per name:ca ----
# afegeix els name:ca buits i que concorden amb la toponímia de l'NGCat

# name<- unique(osm_NGCat[is.na(osm_NGCat$`name:ca`) | osm_NGCat$Toponim != osm_NGCat$`name:ca`, c("Toponim", "name", "name:ca", "osm_id", "osm_type", "osm_idMun", "osm_idCom")])
name<- unique(osm_NGCat[
  osm_NGCat$Toponim != osm_NGCat$name | osm_NGCat$Toponim != osm_NGCat$`name:ca` | is.na(osm_NGCat$`name:ca`),
  c("Toponim", "name", "name:ca", "osm_id", "osm_type", "osm_idMun", "osm_idCom")
])
table(is.na(name$osm_idCom))
table(is.na(name$osm_idMun))
dup<- dbTools::duplicatedPK(name, pk=c("osm_id", "osm_type"))
dbTools::nonUniqueValuesByPK(dup, pk=c("osm_id", "osm_type"))
## CONCLUSIONS: diferents municipis. Continua, cap problema


name.com<- unique(merge(name, tesaurus_comarques[, c("osm_id", "osm_name")], by.x="osm_idCom", by.y="osm_id"))
names(name.com)<- gsub("^osm_name$", "osm_nameCom", names(name.com))
names(name.com)<- gsub("^osm_id$", "idOSM", names(name.com))
names(name.com)<- gsub("^osm_type$", "typeOSM", names(name.com))

dif<- name.com[name.com$Toponim != name.com$name | (!is.na(name.com$`name:ca`) & name.com$Toponim != name.com$`name:ca`), ]
dif[grepl("^[a-z]+", dif$name) & grepl("^[A-Z]+", dif$Toponim) & dif$osm_nameCom != "Val d'Aran", ]
dif[grepl("^[a-z]+", dif$name) & grepl("^[A-Z]+", dif$Toponim) & dif$osm_nameCom == "Val d'Aran", ]
dif[grepl("^[a-z]+", dif$`name:ca`) & grepl("^[A-Z]+", dif$Toponim) & dif$osm_nameCom == "Val d'Aran", ]
dif[dif$osm_nameCom == "Val d'Aran", ]

## CONCLUSIONS: diferències en les majúscules. Selecciona la versió de l'ICGC


dif.name<- name.com[which(name.com$name != name.com$`name:ca`), c("Toponim", "name", "name:ca", "osm_nameCom")]
View(dif.name)
# CONCLUSIONS: Alguns noms (provinents de wikipedia?) difereixen entre name i name:ca. Passa-ho a alt_name i alt_name:ca
# de moment, omet els casos i revisa a posteriori.
# PENDENT! afegir alt_name ----
# FET: revertir casos editats (comarques "Alt .+"). Es perd algun (Església de ...)

name.edita<- name.com[is.na(name.com$`name:ca`) | name.com$name == name.com$`name:ca`, ]
## Mostra per la comunitat
# n_comarca<- table(name.edita$osm_nameCom)
# openxlsx::write.xlsx(name.edita[name.edita$osm_nameCom == names(which.min(n_comarca)), ], file=paste0("majúscules_name-ICGC-", names(which.min(n_comarca)), ".xls"))

selCa<- name.edita$osm_nameCom != "Val d'Aran"
selOc<- !selCa
name.edita$`name:ca`[selCa]<- name.edita$Toponim[selCa]
name.edita$`name:oc`[selOc]<- name.edita$Toponim[selOc]
name.edita$name<- name.edita$Toponim


cmd<- by(name.edita, name.edita$osm_nameCom, function(x){
  comarca<- unique(x$osm_nameCom)
  if (comarca == "Val d'Aran"){
    fitxer<- file.path(arrelProjecte, "edicions", paste0("informe-", comarca, "_NGCat-name_name:oc.tsv"))
  } else {
    fitxer<- file.path(arrelProjecte, "edicions", paste0("informe-", comarca, "_NGCat-name_name:ca.tsv"))
  }
  dFormated<- rbind(c("# EDITED with R", rep("", times=ncol(x) - 1)), names(x))
  dFormated<- rbind(dFormated, as.matrix(x[order(x$osm_idMun), ]), deparse.level=0)
  write.table(dFormated, file=fitxer, sep="\t", na="", col.names=FALSE, row.names=FALSE)

  cmd<- paste0('update_osm_objects_from_report --username ', usuari, ' --passwordfile ', fitxerContrasenya)
  cmd<- paste0(cmd, " --changeset-hashtags \"#toponimsCat;#OSM-ICGC;#ICGC-NGCat_names\" --changeset-source \"ICGC NGCat i etiqueta name\"",
         " --batch 150 -v --confirmed-edits ", # --confirm-overwrites --no-interaction
         " --changeset-comment \"Corregeix les majúscules dels noms seguint l'ICGC (NGCat v10cs0f1r011) a partir de name que concorden a ", comarca, "\"")
  cmd<- paste0(cmd, ' --input-file "', fitxer, '"')
  if (comarca == "Val d'Aran"){
    cmd<- paste0(cmd, " name name:oc")
  } else {
    cmd<- paste0(cmd, " name name:ca")
  }

  return (cmd)
})

cmd<- as.character(cmd)
cmd<- na.omit(cmd)
cat(cmd, sep="\n")

## Executa les ordres (per sobreescriure cal copiar les comandes a una terminal perquè cal treure --no-interaction)
pb<- timerProgressBar(max=length(cmd))
for (i in 1:length(cmd)){
  message("\n", i, " / ", length(cmd), "\t", cmd[i])
  system(cmd[i])
  setTimerProgressBar(pb, i)
}
close(pb)

## Arxiva els informes dels municipis actualitzades a edicions/FET i actualitza o elimina els informes originals desactualitzats ----
informesActualitzats<- actualitzaInformesCarregats(arrelProjecte=arrelProjecte, esborraInformesDesactualitzats=TRUE)
