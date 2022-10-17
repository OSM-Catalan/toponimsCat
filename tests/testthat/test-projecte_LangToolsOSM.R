## Nota: arrelProjecte acabada en «/» pel projecte Rússia i sense per PPCC per comparar el comportament

test_that("generaInformes funciona", {
  ordre0<- generaInforme(arrelProjecte="exotopònims/Rússia/",
                         fitxerInforme="informe-Territori de Kamtxatka_wikidata.tsv",
                         filtreArea="['name:ca'='Territori de Kamtxatka'][admin_level=4]",
                         filtreObjectes="nwr[wikidata][name][!'name:ca']")

  if (length(ordre0) > 0)
    system(ordre0)
  expect_true(file.exists("exotopònims/Rússia/informes/informe-Territori de Kamtxatka_wikidata.tsv"))

  ordre1<- generaInforme(arrelProjecte="PPCC/calle-carrer",
                         fitxerInforme="informe-Calle_carrer-l'Alacantí.tsv",
                         filtreArea="['name:ca'='l\\'Alacantí'][admin_level=7]",
                         filtreObjectes="nwr[name~'^[Cc]alle'][!'name:ca']")
  if (length(ordre1) > 0)
    system(ordre1)
  expect_true(file.exists("PPCC/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv"))

  ordre0<- generaInforme(arrelProjecte="exotopònims/Rússia/",
                         fitxerInforme="informe-Territori de Kamtxatka_wikidata.tsv",
                         filtreArea="['name:ca'='informe-Territori de Kamtxatka_wikidata.tsv'][admin_level=4]",
                         filtreObjectes="nwr[wikidata][!'name:ca']",
                         actualitzaFitxer=TRUE)
  ordre1<- generaInforme(arrelProjecte="PPCC/calle-carrer",
                         fitxerInforme="informe-Calle_carrer-l'Alacantí.tsv",
                         filtreArea="['name:ca'='l\\'Alacantí'][admin_level=7]",
                         filtreObjectes="nwr[name~'^[Cc]alle'][!'name:ca']",
                         actualitzaFitxer=TRUE)
  expect_type(ordre0, "character")
  expect_type(ordre1, "character")
  # Restaura fitxers eliminats amb actualitzaFitxer=TRUE per evitar més consultes a opverpass
  file.copy("PPCC/calle-carrer/ANTIC/informe-Calle_carrer-l'Alacantí.tsv", "PPCC/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv")
  file.copy("exotopònims/Rússia/ANTIC/informe-Territori de Kamtxatka_wikidata.tsv", "exotopònims/Rússia/informes/informe-Territori de Kamtxatka_wikidata.tsv")
})


test_that("recompteCasosInformes funciona", {
  res<- list()
  res$arrel<- recompteCasosInformes(arrelProjecte="PPCC/calle-carrer")
  res$fitxers<- recompteCasosInformes(informes=dir("PPCC/calle-carrer/informes", pattern="\\.tsv$", full.names=TRUE))
  res$df<- recompteCasosInformes(dades=data.frame(informe=dir("PPCC/calle-carrer/informes", pattern="\\.tsv$", full.names=TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat"))

  res<- list()
  res$arrel<- recompteCasosInformes(arrelProjecte="exotopònims/Rússia/")
  res$fitxers<- recompteCasosInformes(informes=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE))
  res$df<- recompteCasosInformes(dades=data.frame(informe=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat"))

  res<- recompteCasosInformes(arrelProjecte="")
  expect_equal(res, data.frame(informe=character(), nObjectes=numeric(), nCasos=numeric(),
                               nObjectesNomWikidata=numeric(), nCasosNomWikidata=numeric(), revisat=logical()))
})


test_that("recompteCasosEdicions funciona", {
  res<- list()
  res$fitxers<- recompteCasosEdicions(edicions=dir("PPCC/calle-carrer/edicions", pattern="\\.tsv$", full.names=TRUE))
  res$arrel<- recompteCasosEdicions(arrelProjecte="PPCC/calle-carrer")
  res$df<- recompteCasosEdicions(dades=data.frame(edicio=dir("PPCC/calle-carrer/edicions", pattern="\\.tsv$", full.names=TRUE)))

  # expect_length(unique(res), 1)  ## Usa informes com a fitxers d'edicio (no funciona amb arrelProjecte)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata"))

  res<- list()
  res$fitxers<- recompteCasosEdicions(edicions=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE))
  res$arrel<- recompteCasosEdicions(arrelProjecte="exotopònims/Rússia/")
  res$df<- recompteCasosEdicions(dades=data.frame(edicio=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE)))

  # expect_length(unique(res), 1)  ## Usa informes com a fitxers d'edicio (no funciona amb arrelProjecte)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata"))

  res<- recompteCasosEdicions(arrelProjecte="")
  expect_equal(res, data.frame(informe=character(), nObjectes=numeric(), nCasos=numeric(), nObjectesNomWikidata=numeric(), nCasosNomWikidata=numeric()))
})


test_that("recompteCasos funciona", {
  res<- list()
  res$arrel<- recompteCasos(arrelProjecte="PPCC/calle-carrer")
  res$fitxers<- recompteCasos(informes=dir("PPCC/calle-carrer/informes", pattern="\\.tsv$", full.names=TRUE))
  res$df<- recompteCasos(dades=data.frame(informe=dir("PPCC/calle-carrer/informes", pattern="\\.tsv$", full.names=TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat",
                                  "nObjectesEditat", "nCasosEditat", "nObjectesNomWikidataEditat", "nCasosNomWikidataEditat"))

  res<- list()
  res$arrel<- recompteCasos(arrelProjecte="exotopònims/Rússia/")
  res$fitxers<- recompteCasos(informes=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE))
  res$df<- recompteCasos(dades=data.frame(informe=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE)))

  expect_length(unique(res), 1)
  expect_s3_class(res[[1]], "data.frame")
  expect_equal(names(res[[1]]), c("informe", "nObjectes", "nCasos", "nObjectesNomWikidata", "nCasosNomWikidata", "revisat",
                                  "nObjectesEditat", "nCasosEditat", "nObjectesNomWikidataEditat", "nCasosNomWikidataEditat"))

  res<- recompteCasos(arrelProjecte="")
  expect_equal(res, data.frame(informe=character(), nObjectes=numeric(), nCasos=numeric(), nObjectesNomWikidata=numeric(), nCasosNomWikidata=numeric(), revisat=logical(),
                               nObjectesEditat=numeric(), nCasosEditat=numeric(), nObjectesNomWikidataEditat=numeric(), nCasosNomWikidataEditat=numeric()))
})


test_that("generaRevisions funciona", {
  res<- generaRevisions_regexName(informes=dir("PPCC/calle-carrer/informes", pattern="\\.tsv$", full.names=TRUE),
                                  arrelProjecte="PPCC/calle-carrer", cerca="([Cc])alle ", substitueix="\\1arrer ")
  expect_type(res, "character")
  expect_true(file.exists("PPCC/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.tsv"))
  expect_true(file.exists(res))

  res<- generaRevisions_regexTranslations(informes=dir("exotopònims/Rússia/informes", pattern="\\.tsv$", full.names=TRUE),
                                          arrelProjecte="exotopònims/Rússia/", cerca=" \\(.+\\)", substitueix="",
                                          ometSenseTraduccions=FALSE)
  expect_type(res, "character")
  expect_true(file.exists("exotopònims/Rússia/revisions/revisio-Territori de Kamtxatka_wikidata.tsv"))
  expect_true(file.exists(res))
})


test_that("preparaEdicions funciona", {
  file.copy("PPCC/calle-carrer/revisions/revisio-Calle_carrer-l'Alacantí.tsv", "PPCC/calle-carrer/revisions/FET/revisio-Calle_carrer-l'Alacantí.tsv")
  res<- preparaEdicions(arrelProjecte="PPCC/calle-carrer", usuari="usuari")
  expect_type(res, "character")
  # expect_true(file.exists("PPCC/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv"))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca

  file.copy("exotopònims/Rússia/revisions/revisio-Territori de Kamtxatka_wikidata.tsv", "exotopònims/Rússia/revisions/FET/revisio-Territori de Kamtxatka_wikidata.tsv")
  res<- preparaEdicions(arrelProjecte="exotopònims/Rússia/", usuari="usuari")
  expect_type(res, "character")
  # expect_true(file.exists("exotopònims/Rússia/edicions/informe-Territori de Kamtxatka_wikidata.tsv"))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca
})


test_that("actualitzaInformesCarregats funciona", {
  # file.copy("PPCC/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv", "PPCC/calle-carrer/informes/informe-Calle_carrer-l'Alacantí.tsv_ORI")
  # file.copy("exotopònims/Rússia/informes/informe-Territori de Kamtxatka_wikidata.tsv", "exotopònims/Rússia/informe-Territori de Kamtxatka_wikidata.tsv_ORI")
  # file.copy("PPCC/calle-carrer/edicions/FET/informe-Calle_carrer-l'Alacantí_v0.tsv", "PPCC/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv")
  res<- actualitzaInformesCarregats(arrelProjecte="PPCC/calle-carrer", esborraInformesDesactualitzats=FALSE) #suppressWarnings(actualitzaInformesCarregats(arrelProjecte="PPCC/calle-carrer", esborraInformesDesactualitzats=FALSE))
  expect_type(res, "character")
  # expect_true(file.exists(res))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca
  expect_true(file.exists("PPCC/calle-carrer/edicions/FET/informe-Calle_carrer-l'Alacantí_v0.tsv"))
  expect_false(file.exists("PPCC/calle-carrer/edicions/informe-Calle_carrer-l'Alacantí.tsv"))

  res<- actualitzaInformesCarregats(arrelProjecte="exotopònims/Rússia/", esborraInformesDesactualitzats=FALSE)
  expect_type(res, "character")
  # expect_true(file.exists(res))  # FALSE si no hi ha casos amb valors a name:ca o alt_name:ca
  expect_true(file.exists("exotopònims/Rússia/edicions/FET/informe-Territori de Kamtxatka_wikidata_v0.tsv"))
  expect_false(file.exists("exotopònims/Rússia/edicions/informe-Territori de Kamtxatka_wikidata.tsv"))
})
