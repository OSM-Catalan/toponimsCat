# ## per recuperar carrers catellanitzats https://overpass-turbo.eu/s/1obh

# Les dates de les edicions entre holaquiensoy y Antonio Eugenio Burriel van des de l'1 d'agost de 2015 fins que Antonio Eugenio Burriel li reverteix tots els canvis el dia 16 de setembre de 2015.
# Més o menys, tinc clars quins municipis tenen totes les plaques en valencià, quines són bilingües i quines estan en castellà.
library(osmdata)
arrelProjecte<- "PPCC/restauracions-name"

data<- "2015-09-15T00:00:00Z"

# d0<- xml2::download_xml("https://overpass-api.de/api/interpreter?data=%5Bout%3Axml%5D%5Btimeout%3A500%5D%5Badiff%3A%272015-09-11T00%3A00%3A00Z%27%5D%3B%0Aarea%28id%3A3600349043%29-%3E.searchArea%3B%0A%28%0A%20%20nwr%5B%22name%22~%22Carrer%20.%2B%22%5D%28area.searchArea%29%3B%0A%29%3B%0Aout%20tags%20qt%3B")


nominatim_res<- getbb("País Valencià", format_out="data.frame", )
filtreArea<- bbox_to_string(nominatim_res)
q<- opq(bbox=filtreArea, out="tags center", datetime=data, adiff=TRUE, timeout=500)
q<- add_osm_feature(q, key="name", value="^Carrer .+", value_exact=FALSE, match_case=FALSE)

# df<- osmdata_data_frame(q, quiet=FALSE)
# save(df, file=paste0(arrelProjecte, "/carrers_perduts-PV.RData"), compress="xz")
load(paste0(arrelProjecte, "/carrers_perduts-PV.RData"), verbose=TRUE)

df_perdut<- df[which(df$adiff_action == "delete" & (df$adiff_visible == "TRUE")), ]
df_perdut<- df_perdut[, apply(df_perdut, 2, function(x) !all(is.na(x)))]
df_recuperat<- df[which(df$adiff_action == "delete" & is.na(df$adiff_visible)), ]
df_recuperat<- df_recuperat[, apply(df_recuperat, 2, function(x) !all(is.na(x)))]

q_actual<- opq_osm_id(id=df_perdut$osm_id, type=df_perdut$osm_type)
opq_string(q_actual)
df_actual<- osmdata_data_frame(q_actual, quiet=FALSE)


df_recuperat
df_actual

db<- df_recuperat[, c("osm_type", "osm_id", grep("^(alt_)*name", names(df_recuperat), value=TRUE))]
name_recuperat<- grep("name", names(df_recuperat), value=TRUE)
lapply(df_recuperat[, name_recuperat], unique)
names(db)<- gsub("^((alt_)*name)", "antic_\\1", names(db))


db<- merge(df_actual, db, by=c("osm_type", "osm_id"))
db<- db[, apply(db, 2, function(x) !all(is.na(x)))]
grep("name", names(db), value=TRUE)
db[, c(grep("^((antic_)*|(alt_)*)name", names(db), value=TRUE), "osm_type", "osm_id")]
ordCols<- c("name", "antic_name", "name:ca", "alt_name", "antic_alt_name", "alt_name:ca")
ordCols<- c(ordCols, setdiff(grep("name", names(db), value=TRUE), ordCols))
ordCols<- c(ordCols, setdiff(names(db), ordCols))
db_sel<- db[is.na(db$`name:ca`) & !is.na(db$name) & !grepl("\"", db$name), ordCols]
db_sel<- db_sel[, apply(db_sel, 2, function(x) !all(is.na(x)))]
if (!"name:ca" %in% names(db_sel)){
  db_sel$`name:ca`<- db_sel$antic_name
  ord<- c("name", "name:ca")
  ord<- c(ord, setdiff(names(db_sel), ord))
  db_sel<- db_sel[, ord]
}

write.table(db_sel, file=paste0(arrelProjecte, "/name_recuperats-PV-carrers2015.tsv"), sep="\t", na="", col.names=TRUE, row.names=FALSE)
## Revisa carrers recuperats

## Carrega canvis ----
usuari<- "jmaspons" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
fitxerContrasenya<- "~/.osm/motdepas.txt"

# usuari<- "$NomUsuari" # Modifiqueu-ho amb el vostre nom d'usuari a OSM
# fitxerContrasenya<- "" # camí a un fitxer amb una sola línia amb el nom d'usuari i la contrasenya separades per un punt i coma (;)

cmd<- paste0('update_osm_objects_from_report --username ', usuari, ' --passwordfile ', fitxerContrasenya,
             ' -v --confirmed-edits --confirm-overwrites --input-file "', file.path(arrelProjecte, "name_recuperats-PV-carrers2015_REVISAT.tsv"), '" name:ca',
             " --no-interaction --changeset-hashtags \"#toponimsCat;#name_name:ca\" --changeset-source \"2015 name tag\"", #  --no-interaction
             " --batch 100 --changeset-comment \"Afegeix name:ca a partir de name del ", data, " recuperat pel País Valencià.\"")
# if (!all(is.na(db_sel$`alt_name:ca`))){
#   cmd[i]<- paste0(cmd[i], " alt_name:ca")
# }
cat(cmd)


## Esbossos (interessant per consultar àrees quan encara no existien els polígons a OSM) ----
library(sf)

PVbbox<- opq(bbox="País Valencià")
PVarea<- getbb("País Valencià", format_out="sf_polygon")$multipolygon
PVbbox<- opq(bbox=st_bbox(PVarea), timeout = 500)
PVarea.pol<- getbb("País Valencià", format_out="polygon")
PVarea.mat<- getbb("País Valencià", format_out="matrix")
PVarea.string<- getbb("País Valencià", format_out="string")
st_area(PVarea)

# q.pol<- opq(bbox = PVarea.pol, out = "tags center", datetime = data, adiff = TRUE)
# q<- add_osm_feature(PVarea.pol, key="admin_level", value="8")
q<- add_osm_feature(PVbbox, key="admin_level", value="8")
q<- add_osm_feature(q, key="boundary", value="administrative")
pobles<- osmdata_sf(q)
a<- trim_osmdata(dat=pobles, bb_poly=PVarea.pol)

plot(pobles$osm_multipolygons[, "name"])
plot(a$osm_multipolygons[, "name"])
plot(PVarea)

b<- st_intersection(pobles$osm_multipolygons, PVarea)
plot(b[, "name"])

mapView(pobles$osm_multipolygons[, "name"]) + mapview(PVarea) + mapview(st_bbox(PVarea))
mapView(b[, "name"]) + mapview(PVarea) + mapview(st_bbox(PVarea))
# CONCLUSIÓ: b és correcte

# k<- opq_osm_id(id=as.character(municipis$id[municipis$regio == "PV"])[1], type="relation")
k<- opq(bbox=st_bbox(PVarea), timeout=500)
