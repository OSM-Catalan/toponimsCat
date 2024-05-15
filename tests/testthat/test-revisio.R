## Nota: arrelProjecte acabada en «/» pel projecte Rússia i sense per PPCC per comparar el comportament

test_that("genera_revisions funciona", {
  revisions <- list()
  revisions$camins <- genera_revisions(
    informes = dir(
      "projectes/calle-carrer/informes",
      pattern = "\\.(RData|tsv)$", full.names = TRUE, ignore.case = TRUE
    )
  )
  revisions$arrel_nom <- genera_revisions(
    informes = dir("projectes/calle-carrer/informes", pattern = "\\.(RData|tsv)$", ignore.case = TRUE),
    arrelProjecte = "projectes/calle-carrer"
  )
  revisions$arrel <- genera_revisions(arrelProjecte = "projectes/calle-carrer")

  fitxersInformes <- dir(
    "projectes/calle-carrer/informes",
    pattern = "\\.(RData|tsv)$", full.names = TRUE, ignore.case = TRUE
  )
  names(fitxersInformes) <- gsub("projectes/calle-carrer/informes/", "", fitxersInformes)
  informes <- lapply(fitxersInformes, carrega_informe)
  revisions$informes <- genera_revisions(informes = informes)

  lapply(revisions, expect_type, type = "list")
  lapply(revisions, lapply, expect_s3_class, class = "data.frame")
  lapply(
    revisions, lapply, expect_named,
    expected = c("name", "name:ca", "alt_name:ca", "alt_name", "noms_wd", "ca.viquipedia", "wikidata_tipus", "wikidata")
  )
})


test_that("genera_revisions_regex_name funciona", {
  revisions <- list()
  suppressWarnings(file.remove("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData"))
  revisions$camins <- genera_revisions_regex_name(
    informes = dir(
      "projectes/calle-carrer/informes",
      pattern = "\\.(RData|tsv)$", full.names = TRUE, ignore.case = TRUE
    ), cerca = "([Cc])alle ", substitueix = "\\1arrer "
  )
  expect_true(file.exists("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData"))
  file.remove("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData")

  revisions$arrel_nom <- genera_revisions_regex_name(
    informes = dir("projectes/calle-carrer/informes", pattern = "\\.(RData|tsv)$", ignore.case = TRUE),
    arrelProjecte = "projectes/calle-carrer", cerca = "([Cc])alle ", substitueix = "\\1arrer "
  )
  expect_true(file.exists("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData"))
  file.remove("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData")

  revisions$arrel <- genera_revisions_regex_name(
    arrelProjecte = "projectes/calle-carrer", cerca = "([Cc])alle ", substitueix = "\\1arrer "
  )
  expect_true(file.exists("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData"))
  file.remove("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData")

  fitxersInformes <- dir(
    "projectes/calle-carrer/informes",
    pattern = "\\.(RData|tsv)$", full.names = TRUE, ignore.case = TRUE
  )
  names(fitxersInformes) <- gsub("projectes/calle-carrer/informes/", "", fitxersInformes)
  informes <- lapply(fitxersInformes, carrega_informe)
  revisions$informes <- genera_revisions_regex_name(
    informes = informes, cerca = "([Cc])alle ", substitueix = "\\1arrer "
  )
  expect_true(file.exists("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData"))
  file.remove("projectes/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.RData")

  lapply(revisions, expect_type, type = "list")
  lapply(revisions, lapply, expect_s3_class, class = "data.frame")
  lapply(
    revisions, lapply, expect_named,
    expected = c("name", "name:ca", "alt_name:ca", "alt_name", "noms_wd", "ca.viquipedia", "wikidata_tipus", "wikidata")
  )
})


test_that("genera_revisions_regex_nomsWD funciona", {
  revisions <- list()

  suppressWarnings(file.remove("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv"))
  revisions$camins <- genera_revisions_regex_nomsWD(
    informes = dir(
      "projectes/Rússia/informes",
      pattern = "\\.(RData|tsv)$", full.names = TRUE, ignore.case = TRUE
    ), cerca = " \\(.+\\)", substitueix = "", ometSenseTraduccions = FALSE
  )
  expect_true(file.exists("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv"))
  file.remove("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv")

  revisions$arrel_nom <- genera_revisions_regex_nomsWD(
    informes = dir("projectes/Rússia/informes", pattern = "\\.(RData|tsv)$", ignore.case = TRUE),
    arrelProjecte = "projectes/Rússia", cerca = " \\(.+\\)", substitueix = "", ometSenseTraduccions = FALSE
  )
  expect_true(file.exists("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv"))
  file.remove("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv")

  revisions$arrel <- genera_revisions_regex_nomsWD(
    arrelProjecte = "projectes/Rússia", cerca = " \\(.+\\)", substitueix = "", ometSenseTraduccions = FALSE
  )
  expect_true(file.exists("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv"))
  file.remove("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv")

  fitxersInformes <- dir(
    "projectes/Rússia/informes",
    pattern = "\\.(RData|tsv)$", full.names = TRUE, ignore.case = TRUE
  )
  names(fitxersInformes) <- gsub("projectes/Rússia/informes/", "", fitxersInformes)
  informes <- lapply(fitxersInformes, carrega_informe)
  revisions$informes <- genera_revisions_regex_nomsWD(
    informes = informes, cerca = " \\(.+\\)", substitueix = "", ometSenseTraduccions = FALSE
  )
  expect_true(file.exists("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv"))
  file.remove("projectes/Rússia/revisions/revisio-Murmansk_wikidata.tsv")

  lapply(revisions, expect_type, type = "list")
  lapply(revisions, lapply, expect_s3_class, class = "data.frame")
  lapply(
    revisions, lapply, expect_named,
    expected = c("name", "name:ca", "alt_name:ca", "noms_wd", "wikidata_tipus", "wikidata")
  )
  lapply(revisions, expect_identical, expected = revisions[[1]])
})
