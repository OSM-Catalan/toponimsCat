% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projecte_LangToolsOSM.R
\name{recompteCasos}
\alias{recompteCasos}
\alias{recompteCasosInformes}
\alias{recompteCasosEdicions}
\title{Recompte de casos dels informes, edicions o ambdós}
\usage{
recompteCasosInformes(arrelProjecte, informes, dades)

recompteCasosEdicions(arrelProjecte, edicions, dades)

recompteCasos(arrelProjecte, informes, dades)
}
\arguments{
\item{arrelProjecte}{camí a l'arrel del projecte. Es llegiran tots els fitxers *.tsv de la subcarpeta \code{informes} i/o \code{edicions/FET} si no s'especifiquen altres paràmetres.}

\item{informes}{vector de caràcters amb els camins a fitxers d'informes.}

\item{dades}{un \code{data.frame} amb una columna anomenada \code{informe} (o \code{edicio} per la funció \code{recompteCasosEdicions}) amb els camins als fitxers «.tsv». Si s'especifica, s'ignoren la resta de paràmetres.}

\item{edicions}{vector de caràcters amb els camins a fitxers d'edicions.}
}
\value{
Una taula amb files per cada informe i amb les columnes següents:
  \describe{
    \item{informe}{camí del fitxer d'informe.}
    \item{nObjectes}{nombre d'objectes d'OSM.}
    \item{nCasos}{nombre de casos únics de les etiquetes \code{«name»}, \code{«name:ca»}, \code{«alt_name:ca»},
 \code{«alt_name»}, \code{«translations»}, \code{«ca.wikipedia_page»} i \code{«wikidata_id»}.}
    \item{nObjectesNomWikidata}{nombre d'objectes d'OSM amb nom en català a wikidata (columna translations dels informes).}
    \item{nCasosNomWikidata}{nombre de casos únics amb nom en català a wikidata (columna translations dels fitxers de revisions).}
    \item{revisat}{si existeix o no unfitxer de revisió a la subcarpeta del projecte \code{revisions/FET}).}

  \strong{Només per la funció \code{recompteCasos}:}
    \item{nObjectesEditat}{nombre d'objectes editats d'OSM.}
    \item{nCasosEditat}{nombre de casos únics editats de les etiquetes \code{«name»}, \code{«name:ca»}, \code{«alt_name:ca»},
 \code{«alt_name»}, \code{«translations»}, \code{«ca.wikipedia_page»} i \code{«wikidata_id»}.}
    \item{nObjectesNomWikidataEditat}{nombre d'objectes editats d'OSM amb nom en català a wikidata (columna translations dels informes).}
    \item{nCasosNomWikidataEditat}{nombre de casos únics editats amb nom en català a wikidata (columna translations dels fitxers de revisions).}
}
}
\description{
Compta el nombre de casos i casos únics, si tenen informació de wikidata o no
i altres detalls. Per les edicions, la informació s'agrega a nivell d'informe,
és a dir, si hi ha diversos fitxers d'edicions per un mateix informe, els suma.
}
