\name{municipis}
\alias{municipis}
\docType{data}
\title{Municipis dels Països Catalans}
\description{
Dades de les Municipis dels Països Catalans.
}
\details{
Alguns noms de municipis tenen duplicats. Per construir noms únics (p. ex. per generar noms de fitxers), podeu usar \code{paste0(municipis$regio, "-", municipis$name).}
}
\usage{municipis}
\format{
  Un \code{data.frame} amb dades dels 1672 municipis  per les següents 8 variables.
  \describe{
    \item{type}{Tipus d'objecte d'OSM.}
    \item{id}{Número identificador a OSM.}
    \item{name}{Etiqueta \code{name}.}
    \item{name:ca}{Etiqueta \code{name:ca}.}
    \item{regio}{Subdivisió ["Andorra", "CatNord", "Franja", "Illes", "Múrcia", "Principat", "PV", "Sardenya"].}
    \item{admin_level}{Etiqueta \code{admin_level}.}
    \item{wikidata}{Etiqueta \code{wikidata}.}
  }
}
\seealso{
  \code{\link{generaInformesPPCC}}
}
\examples{
municipis[, c("name:ca", "regio", "name")]
}
\keyword{datasets}
