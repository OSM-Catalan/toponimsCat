# https://www.wikidata.org/w/api.php
# TEST: qid <- c(monitorOSM::comarques$wikidata[monitorOSM::comarques$regio == "CatNord"], NA_character_); qid <- c(qid, rev(qid))

#' Afegeix dades wikidata
#'
#' @param x Un `data.frame` amb una columna `wikidata`.
#'
#' @return Retorna `x` amb les columnes `noms_wd` (títol de viquipèdia, etiquetes i àlies), `ca.viquipedia` (títol de
#'   viquipèdia), `wikidata_tipus` (etiquetes de tots els tipus d'instàncies de wikidata (P31)).
#' @export
#'
#' @examples
afegeix_dades_wikidata <- function(x) {
  if ("wikidata" %in% names(x)) {
    if (any(c("noms_wd", "ca.viquipedia", "wikidata_tipus") %in% names(x))) {
      warning(
        "Columnes sobreescrites: ",
        paste(intersect(c("noms_wd", "ca.viquipedia", "wikidata_tipus"), names(x)), collapse = ", ")
      )
    }
    noms_wd <- get_names_from_wikidata(qid = x$wikidata)

    x$noms_wd <- vapply(noms_wd, function(y) {
      noms <- list_names(y$names)
      if (length(noms) > 0) {
        noms <- paste(noms, collapse = "; ")
      } else {
        noms <- NA_character_
      }
      noms
    }, FUN.VALUE = character(1))

    x$ca.viquipedia <- vapply(noms_wd, function(y) {
      if (is.null(y$names$wikipedia$title)) {
        wp <- NA_character_
      } else {
        wp <- paste0("ca:", y$names$wikipedia$title)
      }
      wp
    }, FUN.VALUE = character(1))

    x$wikidata_tipus <- vapply(get_instance_type_from_wikidata(qid = x$wikidata), function(y) {
      paste(y, collapse = "; ")
    }, FUN.VALUE = character(1))
  }

  return(x)
}


#' Get wikipedia from wikidata
#'
#' @param qid A vector of wikidata codes.
#' @param lang A language code matching the prefix of a wikipedia site. (eg. `ca` for `https://ca.wikipedia.org`).
#' @param batch_size Number of qids per API call.
#'
#' @return A named character vector containing the matched `wikipedia` page for `lang`.
#' @noRd
#'
#' @examples
get_wikipedia_from_wikidata <- function(qid, lang = "ca", batch_size = 50) {
  if (all(is.na(qid))) {
    return(stats::setNames(rep(NA_character_, length(qid)), qid))
  }

  qid_u <- unique(stats::na.omit(qid))
  data <- list()
  for (ndx in seq(1L, length(qid_u), by = batch_size)) {
    batch_ids <- qid_u[ndx:min(ndx + batch_size - 1, length(qid_u))]
    query <- paste0(
      "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=", paste(batch_ids, collapse = "|"),
      "&props=sitelinks&languages=", lang, "&format=json"
    )
    req <- httr2::request(query)
    resp <- httr2::req_perform(req)
    batch_data <- httr2::resp_body_json(resp)

    if ("error" %in% names(batch_data)) {
      stop("Wrong response from wikidata: ", batch_data)
    }
    data <- c(data, batch_data$entities)
  }

  out <- vapply(data, function(x) {
    title <- x$sitelinks[[paste0(lang, "wiki")]]$title
    ret <- if (is.null(title)) {
      NA_character_
    } else {
      paste0(lang, ":", title)
    }
    return(ret)
  }, FUN.VALUE = character(1))

  out <- out[match(qid, names(out))]
  return(out)
}


#' Get names from wikidata
#'
#' @param qid A vector of wikidata codes.
#' @param lang A language code matching the prefix of a wikipedia site. (eg. `ca` for `https://ca.wikipedia.org`).
#' @param batch_size Number of qids per API call.
#'
#' @return A list with the `qid` elements containing wikipedia title, labels, and aliases in the language specified by
#'   `lang`.
#' @noRd
#'
#' @examples
get_names_from_wikidata <- function(qid, lang = "ca", batch_size = 50) {
  if (all(is.na(qid))) {
    return(stats::setNames(vector("list", length(qid)), qid))
  }

  qid_u <- unique(stats::na.omit(qid))
  data <- list()
  for (ndx in seq(1L, length(qid_u), by = batch_size)) {
    batch_ids <- qid_u[ndx:min(ndx + batch_size - 1, length(qid_u))]
    query <- paste0(
      "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=", paste(batch_ids, collapse = "|"),
      "&props=labels|aliases|sitelinks&languages=", lang, "&format=json"
    )
    req <- httr2::request(query)
    resp <- httr2::req_perform(req)
    batch_data <- httr2::resp_body_json(resp)

    if ("error" %in% names(batch_data)) {
      stop("Wrong response from wikidata: ", batch_data)
    }
    data <- c(data, batch_data$entities)
  }

  out <- list()
  for (wikidata_id in names(data)) {
    value <- data[[wikidata_id]]
    names <- list(wikipedia = NULL, label = NULL, aliases = NULL, extra = NULL)
    if (paste0(lang, "wiki") %in% names(value$sitelinks)) {
      names$wikipedia <- value$sitelinks[[paste0(lang, "wiki")]]
    }
    if (lang %in% names(value$labels)) {
      names$label <- value$labels[[lang]]
    }
    if (lang %in% names(value$aliases)) {
      names$aliases <- value$aliases[[lang]]
    }

    if (is.null(names$label) && is.null(names$aliases) && is.null(names$wikipedia)) {
      names <- NULL
    } else { # Generate new translation options
      extra_names <- list_names(names)

      # Remove brackets and the text inside
      if (any(sel <- grepl("\\s*\\(.+\\)\\s*", extra_names))) {
        if (is.null(names$extra)) names$extra <- list()
        names$extra <- list(
          list(lang = lang, value = gsub("\\s*\\(.+\\)\\s*", "", extra_names[sel]), modifier = "rm brackets")
        )
      }

      extra_names <- list_names(names)

      # Capitalize all words
      capitalized <- vapply(extra_names, function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
      }, FUN.VALUE = character(1))
      if (any(sel <- !capitalized %in% extra_names)) {
        if (is.null(names$extra)) names$extra <- list()
        names$extra <- c(
          names$extra,
          list(list(lang = lang, value = capitalized[sel], modifier = "capitalize"))
        )
      }
    }

    out[[wikidata_id]] <- list(names = names)
  }

  out <- out[match(qid, names(out))]
  return(out)
}


#' Extract names in a character vector.
#'
#' @param names A single item from a [get_names_from_wikidata()] result.
#'
#' @return A vector of the unique names.
#' @noRd
#'
#' @examples
#' qid <- c("Q15479", "Q15478", "Q15477", "Q15476", "Q15475")
#' lapply(get_names_from_wikidata(qid), function(x) list_names(x$names))
list_names <- function(names) {
  translation_list <- character()
  if (!is.null(names)) {
    if (!is.null(names$wikipedia)) {
      translation_list <- c(translation_list, names$wikipedia$title)
    }
    if (!is.null(names$label)) {
      translation_list <- c(translation_list, names$label$value)
    }
    if (!is.null(names$aliases)) {
      translation_list <- c(
        translation_list,
        vapply(names$aliases, function(x) x$value, FUN.VALUE = character(1))
      )
    }
    if (!is.null(names$extra)) {
      translation_list <- c(
        translation_list,
        unlist(lapply(names$extra, function(x) x$value))
      )
    }
  }

  return(unique(translation_list))
}


#' Get instance type (P31) from wikidata
#'
#' @param qid A vector of wikidata codes.
#' @param lang A language code. (eg. `ca` for Catalan).
#' @param batch_size Number of qids per API call.
#'
#' @return A list with a item for each `qid` containing the labels of all instance type (P31).
#' @noRd
#'
#' @examples
get_instance_type_from_wikidata <- function(qid, lang = "ca", batch_size = 50) {
  if (all(is.na(qid))) {
    return(stats::setNames(vector("list", length(qid)), qid))
  }

  qid_u <- unique(stats::na.omit(qid))
  id_instance_type <- list() # P31 (instance of) ids of the wikidata elements
  for (ndx in seq(1L, length(qid_u), by = batch_size)) {
    batch_ids <- qid_u[ndx:min(ndx + batch_size - 1, length(qid_u))]
    query <- paste0(
      "https://www.wikidata.org/w/api.php?action=wbgetentities&format=json&props=claims&languages=en|", lang,
      "&ids=", paste(batch_ids, collapse = "|")
    )
    req <- httr2::request(query)
    resp <- httr2::req_perform(req)
    batch_data <- httr2::resp_body_json(resp)
    if ("error" %in% names(batch_data)) {
      stop("Wrong response from wikidata: ", batch_data)
    }
    for (key in names(batch_data$entities)) {
      val <- batch_data$entities[[key]]
      if ("P31" %in% names(val$claims)) {
        id_P31 <- vapply(val$claims$P31, function(x) x$mainsnak$datavalue$value$id, FUN.VALUE = character(1))
        id_instance_type[[key]] <- list(P31 = id_P31)
      }
    }
  }
  id_instance_type_unique <- unique(unlist(id_instance_type))

  instance_type <- list() # Labels of the P31 ids
  for (ndx in seq(1L, length(id_instance_type_unique), by = batch_size)) {
    batch_ids <- id_instance_type_unique[ndx:min(ndx + batch_size - 1, length(id_instance_type_unique))]
    query <- paste0(
      "https://www.wikidata.org/w/api.php?action=wbgetentities&format=json&props=labels&languages=en|", lang,
      "&ids=", paste(batch_ids, collapse = "|")
    )
    req <- httr2::request(query)
    resp <- httr2::req_perform(req)
    batch_data <- httr2::resp_body_json(resp)
    if ("error" %in% names(batch_data)) {
      stop("Wrong response from wikidata: ", batch_data)
    }
    for (key in names(batch_data$entities)) {
      val <- batch_data$entities[[key]]
      if (lang %in% names(val$labels)) {
        instance_type[[key]] <- val$labels[[lang]]$value
      } else if ("en" %in% names(val$labels)) {
        instance_type[[key]] <- val$labels[["en"]]$value
      }
    }
  }

  out <- list() # Labels of the P31 values of the wikidata elements
  for (wikidata_id in names(id_instance_type)) {
    types <- id_instance_type[[wikidata_id]]
    for (P31 in types$P31) {
      if (P31 %in% names(instance_type)) {
        out[[wikidata_id]] <- c(out[[wikidata_id]], instance_type[[P31]])
      }
    }
  }

  out <- out[match(qid, names(out))]
  return(out)
}


## TODO: ----
## def list_translations(translations: dict) -> list:
#   translations_list = []
# if translations:
#   if translations['wikipedia']:
#   translations_list = translations_list + [translations['wikipedia']['title']]
# if translations['label']:
#   translations_list = translations_list + [translations['label']['value']]
# if translations['aliases']:
#   translations_list = translations_list + [x['value'] for x in translations['aliases']]
# if translations['extra']:
#   translations_list = translations_list + [x['value'] for x in translations['extra']]
# return list(dict.fromkeys(translations_list).keys())



get_wikidata_from_wikipedia <- function(wikipedia) {
  # db = {}
  # pattern_lang = re.compile(r'^([a-z]+):.+')
  # pattern_title = re.compile(r'^[a-z]+:(.+)')
  # for i in wikipedia:
  #   try:
  #   lang = pattern_lang.search(string=i).group(1)
  # title = pattern_title.search(string=i).group(1)
  # except AttributeError:
  #   lang = None
  # title = None
  # if lang:
  #   if lang not in db.keys():
  #   db.update({lang: {title}})
  # else:
  #   db[lang].add(title)
  # out = {}
  # for lang, titles in db.items():
  #   wikidict = get_wikidata_from_langwikipedia(sitelinks=list(titles), lang=lang)
  # out.update(dict((lang + ':' + key, value) for (key, value) in wikidict.items()))
  # return(out)
}

# # wikipedia = ['ca:Sant Feliu del Bac', 'ca:Perpinyà', 'NO_LANG', 'fr:Perpignan']
## def get_wikidata_from_wikipedia(wikipedia: list) -> dict:
#   db = {}
# pattern_lang = re.compile(r'^([a-z]+):.+')
# pattern_title = re.compile(r'^[a-z]+:(.+)')
# for i in wikipedia:
#   try:
#   lang = pattern_lang.search(string=i).group(1)
# title = pattern_title.search(string=i).group(1)
# except AttributeError:
#   lang = None
# title = None
# if lang:
#   if lang not in db.keys():
#   db.update({lang: {title}})
# else:
#   db[lang].add(title)
# out = {}
# for lang, titles in db.items():
#   wikidict = get_wikidata_from_langwikipedia(sitelinks=list(titles), lang=lang)
# out.update(dict((lang + ':' + key, value) for (key, value) in wikidict.items()))
# return out


get_wikidata_from_langwikipedia <- function(sitelinks, lang = "ca", batch_size = 50) {
  # data = {}
  # for ndx in range(0, len(sitelinks), batch_size):
  #   batch_sitelinks = sitelinks[ndx:min(ndx + batch_size, len(sitelinks))]
  # query = 'https://' + lang + '.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&' + \
  # 'redirects=1&format=json&utf8=True&titles=' + '|'.join(batch_sitelinks)
  # response = requests.get(query)
  # batch_data = response.json()
  # if 'error' in batch_data.keys():
  #   raise Exception('Wrong response from wikidata: ' + batch_data)
  # data.update(batch_data['query']['pages'])
  # # import json
  # # print(json.dumps(data, indent=2))
  #
  # out = {}
  # for value in data.values():
  #   dict_item = {value['title']: None}
  # if 'pageprops' in value.keys() and value['pageprops']['wikibase_item']:
  #   dict_item[value['title']] = value['pageprops']['wikibase_item']
  # out.update(dict_item)
  # return(out)
}


## def get_wikidata_from_langwikipedia(sitelinks: list, lang: str, batch_size=50) -> dict:
#   data = {}
# for ndx in range(0, len(sitelinks), batch_size):
#   batch_sitelinks = sitelinks[ndx:min(ndx + batch_size, len(sitelinks))]
# query = 'https://' + lang + '.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&' + \
# 'redirects=1&format=json&utf8=True&titles=' + '|'.join(batch_sitelinks)
# response = requests.get(query)
# batch_data = response.json()
# if 'error' in batch_data.keys():
#   raise Exception('Wrong response from wikidata: ' + batch_data)
# data.update(batch_data['query']['pages'])
# # import json
# # print(json.dumps(data, indent=2))
#
# out = {}
# for value in data.values():
#   dict_item = {value['title']: None}
# if 'pageprops' in value.keys() and value['pageprops']['wikibase_item']:
#   dict_item[value['title']] = value['pageprops']['wikibase_item']
# out.update(dict_item)
# return out


## TESTS with WikidataR ----
# item <- WikidataR::get_item("Q582724")
# properties <- WikidataR::get_property("P625")
# sapply(properties[[1]]$claims, names)
# lapply(properties[[1]]$claims, function(x) x$mainsnak$datavalue)
# WikidataR::extract_claims(item, claims = c("P31", "P625", "P402"))
#
# # WikidataR::get_names_from_properties(WikidataR::extract_claims(item, c("P31", "P625", "P402")))
# WikidataR:::WD.globalvar$PID.constraint
# WikidataR::wd_query("Q582724", properties = c("P31", "P625", "P402"))
# WikipediR::page_content

# P625 :  coordinate location
# P31 :   instance of
# P402 :	OpenStreetMap relation ID
