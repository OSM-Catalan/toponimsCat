## Nota: arrelProjecte acabada en «/» pel projecte Rússia i sense per ppcc per comparar el comportament

test_that("generaInformes funciona", {
  ordre0<- generaInforme(arrelProjecte="exotopònims/Rússia/",
                         fitxerInforme="informe-Sibèria.tsv",
                         filtreArea="['name:ca'='Districte Federal de Sibèria'][admin_level=3]",
                         filtreObjectes="nwr[wikidata][name][!'name:ca']")

  if (length(ordre0) > 0)
    system(ordre0)
  expect_true(file.exists("exotopònims/Rússia/informes/informe-Sibèria.tsv"))

  ordre1<- generaInforme(arrelProjecte="ppcc/calle-carrer",
                         fitxerInforme="informe-Calle_carrer-l'Alacantí.tsv",
                         filtreArea="['name:ca'='l\\'Alacantí'][admin_level=7]",
                         filtreObjectes="nwr[name~'^[Cc]alle'][!'name:ca']")
  if (length(ordre1) > 0)
    system(ordre1)
  expect_true(file.exists("ppcc/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv"))

  ordre0<- generaInforme(arrelProjecte="exotopònims/Rússia/",
                         fitxerInforme="informe-Sibèria.tsv",
                         filtreArea="['name:ca'='Districte Federal de Sibèria'][admin_level=3]",
                         filtreObjectes="nwr[wikidata][!'name:ca']",
                         actualitzaFitxer=TRUE)
  ordre1<- generaInforme(arrelProjecte="ppcc/calle-carrer",
                         fitxerInforme="informe-Calle_carrer-l'Alacantí.tsv",
                         filtreArea="['name:ca'='l\\'Alacantí'][admin_level=7]",
                         filtreObjectes="nwr[name~'^[Cc]alle'][!'name:ca']",
                         actualitzaFitxer=TRUE)
  expect_type(ordre0, "character")
  expect_type(ordre1, "character")
  file.rename("ppcc/calle-carrer/ANTIC/informe-Calle_carrer-l'Alacantí.tsv", "ppcc/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv")
  file.rename("exotopònims/Rússia/ANTIC/informe-Sibèria.tsv", "exotopònims/Rússia/informes/informe-Sibèria.tsv")
})


test_that("recompteCasosInformes funciona", {
  res<- list()
  res$arrel<- recompteCasosInformes(arrelProjecte="ppcc/calle-carrer")
  res$fitxers<- recompteCasosInformes(informes=dir("ppcc/calle-carrer/informes", pattern="\\.tsv$", full.names=TRUE))
  res$df<- recompteCasosInformes(dades=data.frame(informe=dir("ppcc/calle-carrer/informes", pattern="\\.tsv$", full.names=TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjects", "nCasos", "revisat"))

  res<- list()
  res$arrel<- recompteCasosInformes(arrelProjecte="exotopònims/Rússia/")
  res$fitxers<- recompteCasosInformes(informes=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE))
  res$df<- recompteCasosInformes(dades=data.frame(informe=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjects", "nCasos", "revisat"))

  res<- recompteCasosInformes(arrelProjecte="")
  expect_equal(res, data.frame())
})


test_that("generaRevisions funciona", {
  res<- generaRevisions_regexName(informes=dir("ppcc/calle-carrer/informes", pattern="\\.tsv$", full.names=TRUE),
                                  arrelProjecte="ppcc/calle-carrer", cerca="([Cc])alle ", substitueix="\\1arrer ")
  expect_type(res, "character")
  expect_true(file.exists("ppcc/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.tsv"))

  res<- generaRevisions_regexTranslations(informes=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE),
                                  arrelProjecte="exotopònims/Rússia/", cerca=" \\(.+\\)", substitueix="")
  expect_type(res, "character")
  expect_true(file.exists("exotopònims/Rússia/revisions/revisio-Sibèria.tsv"))
})


test_that("preparaEdicions funciona", {
  file.rename("ppcc/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.tsv", "ppcc/calle-carrer/revisions/FET/revisio-Calle_carrer-l'Alacantí.tsv")
  res<- preparaEdicions(arrelProjecte="ppcc/calle-carrer", usuari="usuari")
  expect_type(res, "character")
  expect_true(file.exists("ppcc/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv"))

  file.rename("exotopònims/Rússia/revisions/revisio-Sibèria.tsv", "exotopònims/Rússia/revisions/FET/revisio-Sibèria.tsv")
  res<- preparaEdicions(arrelProjecte="exotopònims/Rússia/", usuari="usuari")
  expect_type(res, "character")
  expect_true(file.exists("exotopònims/Rússia/edicions/informe-Sibèria.tsv"))
})


test_that("actualitzaInformesCarregats funciona", {
  # file.copy("ppcc/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv", "ppcc/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv_ORI")
  # file.copy("exotopònims/Rússia/informes/informe-Sibèria.tsv", "exotopònims/Rússia/informes/informe-Sibèria.tsv_ORI")

  res<- suppressWarnings(actualitzaInformesCarregats(arrelProjecte="ppcc/calle-carrer", esborraInformesDesactualitzats=FALSE))
  expect_type(res, "character")
  expect_true(file.exists(res))
  expect_true(file.exists("ppcc/calle-carrer/edicions/FET/informe-Calle_carrer-l'Alacantí_v0.tsv"))
  expect_false(file.exists("ppcc/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv"))

  res<- actualitzaInformesCarregats(arrelProjecte="exotopònims/Rússia/", esborraInformesDesactualitzats=FALSE)
  expect_type(res, "character")
  expect_true(file.exists(res))
  expect_true(file.exists("exotopònims/Rússia/edicions/FET/informe-Sibèria_v0.tsv"))
  expect_false(file.exists("exotopònims/Rússia/edicions/informe-Sibèria.tsv"))
  # file.copy("ppcc/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv_ORI", "exotopònims/Rússia/informes/informe-Calle_carrer-l'Alacantí.tsv")
  # file.copy("exotopònims/Rússia/informes/informe-Sibèria.tsv_ORI", "exotopònims/Rússia/informes/informe-Sibèria.tsv")

})
