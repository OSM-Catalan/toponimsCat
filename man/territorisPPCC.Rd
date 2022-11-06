\name{territoris}
\alias{territoris}
\docType{data}
\title{Territoris dels Països Catalans}
\description{
Dades dels territoris dels Països Catalans.
}
\usage{territoris}
\format{
  Un \code{data.frame} amb dades dels 8 territoris per les següents 6 variables.
  \describe{
    \item{type}{Tipus d'objecte d'OSM.}
    \item{id}{Número identificador a OSM.}
    \item{name}{Etiqueta \code{name}.}
    \item{name:ca}{Etiqueta \code{name:ca}.}
    \item{regio}{Nom de la regió. Usat pel nom de fitxer a \code{\link{generaInformesPPCC}} ["Principat", "Illes", "País Valencià", "Catalunya Nord", "Franja de Ponent", "l'Alguer", "el Carxe", "Andorra"].}
    \item{wikidata}{Etiqueta \code{wikidata}.}
  }
}
\seealso{
  \code{\link{generaInformesPPCC}}
}
\examples{
territoris[, c("regio", "name")]
}
\keyword{datasets}
