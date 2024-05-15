## Nota: arrelProjecte acabada en «/» pel projecte Rússia i sense per PPCC per comparar el comportament

test_that("recompteCasosInformes funciona", {
  res <- list()
  res$arrel <- recompteCasosInformes(arrelProjecte = "PPCC/calle-carrer")
  res$fitxers <- recompteCasosInformes(informes = dir("PPCC/calle-carrer/informes", pattern = "\\.tsv$", full.names = TRUE))
  res$df <- recompteCasosInformes(dades = data.frame(informe = dir("PPCC/calle-carrer/informes", pattern = "\\.tsv$", full.names = TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat"))

  res <- list()
  res$arrel <- recompteCasosInformes(arrelProjecte = "exotopònims/Rússia/")
  res$fitxers <- recompteCasosInformes(informes = dir("exotopònims/Rússia/informes", pattern = "\\.tsv$", full.names = TRUE))
  res$df <- recompteCasosInformes(dades = data.frame(informe = dir("exotopònims/Rússia/informes", pattern = "\\.tsv$", full.names = TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat"))

  res <- recompteCasosInformes(arrelProjecte = "")
  expect_equal(res, data.frame(
    informe = character(), nObjectes = numeric(), nCasos = numeric(),
    nObjectesNomWikidata = numeric(), nCasosNomWikidata = numeric(), revisat = logical()
  ))
})


test_that("recompteCasosEdicions funciona", {
  res <- list()
  res$fitxers <- recompteCasosEdicions(edicions = dir("PPCC/calle-carrer/edicions", pattern = "\\.tsv$", full.names = TRUE))
  res$arrel <- recompteCasosEdicions(arrelProjecte = "PPCC/calle-carrer")
  res$df <- recompteCasosEdicions(dades = data.frame(edicio = dir("PPCC/calle-carrer/edicions", pattern = "\\.tsv$", full.names = TRUE)))

  # expect_length(unique(res), 1)  ## Usa informes com a fitxers d'edicio (no funciona amb arrelProjecte)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata"))

  res <- list()
  res$fitxers <- recompteCasosEdicions(edicions = dir("exotopònims/Rússia/informes", pattern = "\\.tsv$", full.names = TRUE))
  res$arrel <- recompteCasosEdicions(arrelProjecte = "exotopònims/Rússia/")
  res$df <- recompteCasosEdicions(dades = data.frame(edicio = dir("exotopònims/Rússia/informes", pattern = "\\.tsv$", full.names = TRUE)))

  # expect_length(unique(res), 1)  ## Usa informes com a fitxers d'edicio (no funciona amb arrelProjecte)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata"))

  res <- recompteCasosEdicions(arrelProjecte = "")
  expect_equal(res, data.frame(informe = character(), nObjectes = numeric(), nCasos = numeric(), nObjectesNomWikidata = numeric(), nCasosNomWikidata = numeric()))
})


test_that("recompteCasos funciona", {
  res <- list()
  res$arrel <- recompteCasos(arrelProjecte = "PPCC/calle-carrer")
  res$fitxers <- recompteCasos(informes = dir("PPCC/calle-carrer/informes", pattern = "\\.tsv$", full.names = TRUE))
  res$df <- recompteCasos(dades = data.frame(informe = dir("PPCC/calle-carrer/informes", pattern = "\\.tsv$", full.names = TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c(
    "informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat",
    "nObjectesEditat", "nCasosEditat", "nObjectesNomWikidataEditat", "nCasosNomWikidataEditat"
  ))

  res <- list()
  res$arrel <- recompteCasos(arrelProjecte = "exotopònims/Rússia/")
  res$fitxers <- recompteCasos(informes = dir("exotopònims/Rússia/informes", pattern = "\\.tsv$", full.names = TRUE))
  res$df <- recompteCasos(dades = data.frame(informe = dir("exotopònims/Rússia/informes", pattern = "\\.tsv$", full.names = TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c(
    "informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat",
    "nObjectesEditat", "nCasosEditat", "nObjectesNomWikidataEditat", "nCasosNomWikidataEditat"
  ))

  res <- recompteCasos(arrelProjecte = "")
  expect_equal(res, data.frame(
    informe = character(), nObjectes = numeric(), nCasos = numeric(), nObjectesNomWikidata = numeric(), nCasosNomWikidata = numeric(), revisat = logical(),
    nObjectesEditat = numeric(), nCasosEditat = numeric(), nObjectesNomWikidataEditat = numeric(), nCasosNomWikidataEditat = numeric()
  ))
})
