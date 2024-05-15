## Nota: arrelProjecte acabada en «/» pel projecte Rússia i sense per PPCC per comparar el comportament


test_that("preparaEdicions funciona", {
  file.copy("PPCC/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.tsv", "PPCC/calle-carrer/revisions/FET/revisio-Calle_carrer-l'Alacantí.tsv")
  res <- preparaEdicions(arrelProjecte = "PPCC/calle-carrer", usuari = "usuari")
  expect_type(res, "character")
  # expect_true(file.exists("PPCC/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv"))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca

  file.copy("exotopònims/Rússia/revisions/revisio-Territori de Kamtxatka_wikidata.tsv", "exotopònims/Rússia/revisions/FET/revisio-Territori de Kamtxatka_wikidata.tsv")
  res <- preparaEdicions(arrelProjecte = "exotopònims/Rússia/", usuari = "usuari")
  expect_type(res, "character")
  # expect_true(file.exists("exotopònims/Rússia/edicions/informe-Territori de Kamtxatka_wikidata.tsv"))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca
})


test_that("actualitzaInformesCarregats funciona", {
  # file.copy("PPCC/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv", "PPCC/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv_ORI")
  # file.copy("exotopònims/Rússia/informes/informe-Territori de Kamtxatka_wikidata.tsv", "exotopònims/Rússia/informe-Territori de Kamtxatka_wikidata.tsv_ORI")
  # file.copy("PPCC/calle-carrer/edicions/FET/informe-Calle_carrer-l'Alacantí_v0.tsv", "PPCC/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv")
  res <- actualitzaInformesCarregats(arrelProjecte = "PPCC/calle-carrer", esborraInformesDesactualitzats = FALSE) # suppressWarnings(actualitzaInformesCarregats(arrelProjecte="PPCC/calle-carrer", esborraInformesDesactualitzats=FALSE))
  expect_type(res, "character")
  # expect_true(file.exists(res))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca
  expect_true(file.exists("PPCC/calle-carrer/edicions/FET/informe-Calle_carrer-l'Alacantí_v0.tsv"))
  expect_false(file.exists("PPCC/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv"))

  res <- actualitzaInformesCarregats(arrelProjecte = "exotopònims/Rússia/", esborraInformesDesactualitzats = FALSE)
  expect_type(res, "character")
  # expect_true(file.exists(res))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca
  expect_true(file.exists("exotopònims/Rússia/edicions/FET/informe-Territori de Kamtxatka_wikidata_v0.tsv"))
  expect_false(file.exists("exotopònims/Rússia/edicions/informe-Territori de Kamtxatka_wikidata.tsv"))
})
