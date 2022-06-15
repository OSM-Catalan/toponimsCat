\name{comarques}
\alias{comarques}
\docType{data}
\title{Comarques dels Països Catalans}
\description{
Dades de les Comarques dels Països Catalans.
}
\usage{comarques}
\format{
  Un \code{data.frame} amb dades de les 96 comarques  per les següents 8 variables.
  \describe{
    \item{\code{type}}{Tipus d'objecte d'OSM.}
    \item{\code{id}}{Número identificador a OSM.}
    \item{\code{name}}{Etiqueta \code{name}.}
    \item{\samp{name:ca}}{Etiqueta \code{name:ca}.}
    \item{\code{parcial}}{La comarca està completament inclosa a l'àrea de parla catalans?}
    \item{\code{regio}}{Subdivisió ["Andorra", "Carxe", "CatNord", "Franja", "Illes", "Principat", "PV", "Sardenya"].}
    \item{\code{admin_level}}{Etiqueta \code{admin_level}.}
    \item{\samp{historic:admin_level}}{Etiqueta \code{historic:admin_level}.}
  }
}
\seealso{
  \code{\link{generaInformesPPCC}}
}
\examples{
comarques[, c("name:ca", "regio", "name")]
}
\keyword{datasets}
