% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projecte_LangToolsOSM.R
\name{descartaObjectesSenseTraduccions}
\alias{descartaObjectesSenseTraduccions}
\title{Descarta objectes sense traduccions dels informes}
\usage{
descartaObjectesSenseTraduccions(fitxersInformes)
}
\arguments{
\item{fitxersInformes}{camins dels informes.}
}
\value{
Retorna invisiblement els camins dels informes modificats.
}
\description{
Sobreescriu els informes descarant-ne els objectes (files) sense traduccions de wikidata.
}
