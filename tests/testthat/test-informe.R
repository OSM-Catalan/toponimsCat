## Nota: arrelProjecte acabada en «/» pel projecte Rússia i sense per PPCC per comparar el comportament

test_that("obte_informe funciona", {
  with_mock_dir("mock_informe", {
    consulta0 <- osmdata::getbb("Múrmansk", format_out = "osm_type_id") |>
      osmdata::opq(out = "tags", timeout = 500) |>
      osmdata::add_osm_feature(key = c("name", "!name:ca", "wikidata")) |>
      osmdata::opq_string()
    informe0 <- obte_informe(
      arrelProjecte = "projectes/Rússia/",
      fitxerInforme = "informe-Murmansk_wikidata.tsv",
      consulta = consulta0,
      actualitzaInforme = TRUE,
      consulta_wikidata = TRUE,
      verbose = TRUE
    )
    expect_message(
      informe0_noArrel <- obte_informe(
        fitxerInforme = "projectes/Rússia/informes/informe-Murmansk_wikidata.tsv",
        consulta = consulta0,
        actualitzaInforme = TRUE,
        consulta_wikidata = TRUE,
        verbose = TRUE
      ),
      "El fitxer .+ ja existeix."
    )

    consulta1 <- osmdata::getbb("l'Alacantí", format_out = "osm_type_id") |>
      osmdata::opq(out = "tags", timeout = 500) |>
      osmdata::add_osm_feature(key = c("name", "!name:ca")) |>
      osmdata::add_osm_features(features = list(name = "^[Cc]alle"), value_exact = FALSE) |>
      osmdata::opq_string()
    informe1 <- obte_informe(
      arrelProjecte = "projectes/calle-carrer",
      fitxerInforme = "informe-Calle_carrer-l'Alacantí.RData",
      consulta = consulta1,
      actualitzaInforme = TRUE,
      consulta_wikidata = TRUE,
      verbose = TRUE
    )
  })

  expect_true(file.exists("projectes/Rússia/informes/informe-Murmansk_wikidata.tsv"))
  expect_true(file.exists("projectes/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.RData"))

  informe0bis <- obte_informe(
    arrelProjecte = "projectes/Rússia/",
    fitxerInforme = "informe-Murmansk_wikidata.tsv",
    consulta = consulta1,
    actualitzaInforme = FALSE,
    consulta_wikidata = TRUE,
    verbose = TRUE
  )
  informe1bis <- obte_informe(
    arrelProjecte = "projectes/calle-carrer",
    fitxerInforme = "informe-Calle_carrer-l'Alacantí.RData",
    consulta = consulta1,
    actualitzaInforme = FALSE,
    consulta_wikidata = TRUE
  )

  lapply(list(informe0, informe1, informe0bis, informe1bis), expect_s3_class, class = "data.frame")
  expect_false(identical(informe0, informe0bis)) # format .tsv diferent
  expect_equal(informe1, informe1bis)
})
