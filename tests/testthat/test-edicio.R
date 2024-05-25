## Nota: arrelProjecte acabada en «/» pel projecte Rússia i sense per PPCC per comparar el comportament

test_that("prepara_edicions funciona", {
  revisio <- carrega_revisio("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData")
  revisio <- revisio[1:10, ]
  desa_revisio(
    revisio = revisio,
    fitxerRevisio = "projectes/calle-carrer/revisions/FET/revisio-Calle_carrer-l'Alacantí.RData"
  )
  edicions <- prepara_edicions(arrelProjecte = "projectes/calle-carrer")
  lapply(edicions, expect_s3_class, class = c("osmapi_OsmChange", "osmapi_objects", "data.frame"), exact = TRUE)
  expect_type(edicions, "list")
  expect_true(file.exists("projectes/calle-carrer/edicions/edicio-Calle_carrer-l'Alacantí.RData"))


  revisio <- carrega_revisio("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv")
  # revisio <- revisio[1:10, ]
  desa_revisio(
    revisio = revisio,
    fitxerRevisio = "projectes/Rússia/revisions/FET/revisio-Murmansk_wikidata.RData"
  )
  edicions <- prepara_edicions(arrelProjecte = "projectes/Rússia/", format = "osc")
  lapply(edicions, expect_s3_class, class = "xml_document")
  expect_type(edicions, "list")
  expect_true(file.exists("projectes/Rússia/edicions/edicio-Murmansk_wikidata.osc"))
})


# test_that("envia_edicions funciona", {
#   # file.copy("PPCC/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv", "PPCC/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv_ORI")
#   # file.copy("exotopònims/Rússia/informes/informe-Territori de Kamtxatka_wikidata.tsv", "exotopònims/Rússia/informe-Territori de Kamtxatka_wikidata.tsv_ORI")
#   # file.copy("PPCC/calle-carrer/edicions/FET/informe-Calle_carrer-l'Alacantí_v0.tsv", "PPCC/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv")
#   res <- actualitzaInformesCarregats(arrelProjecte = "PPCC/calle-carrer", esborraInformesDesactualitzats = FALSE) # suppressWarnings(actualitzaInformesCarregats(arrelProjecte="PPCC/calle-carrer", esborraInformesDesactualitzats=FALSE))
#   expect_type(res, "character")
#   # expect_true(file.exists(res))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca
#   expect_true(file.exists("PPCC/calle-carrer/edicions/FET/informe-Calle_carrer-l'Alacantí_v0.tsv"))
#   expect_false(file.exists("PPCC/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv"))
#
#   res <- actualitzaInformesCarregats(arrelProjecte = "exotopònims/Rússia/", esborraInformesDesactualitzats = FALSE)
#   expect_type(res, "character")
#   # expect_true(file.exists(res))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca
#   expect_true(file.exists("exotopònims/Rússia/edicions/FET/informe-Territori de Kamtxatka_wikidata_v0.tsv"))
#   expect_false(file.exists("exotopònims/Rússia/edicions/informe-Territori de Kamtxatka_wikidata.tsv"))
# })
