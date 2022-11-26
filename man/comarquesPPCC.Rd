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
    \item{type}{Tipus d'objecte d'OSM.}
    \item{id}{Número identificador a OSM.}
    \item{name}{Etiqueta \code{name}.}
    \item{name:ca}{Etiqueta \code{name:ca}.}
    \item{parcial}{La comarca està completament inclosa a l'àrea de parla catalana?}
    \item{regio}{Subdivisió ["Andorra", "CatNord", "Franja", "Illes", "Múrcia", "Principat", "PV", "Sardenya"].}
    \item{admin_level}{Etiqueta \code{admin_level}.}
    \item{historic:admin_level}{Etiqueta \code{historic:admin_level}.}
  }
}
\seealso{
  \code{\link{generaInformesPPCC}}, \code{\link{PPCC}}, \code{\link{municipis}}, \code{\link{territoris}}.
}
\examples{
comarques[, c("name:ca", "regio", "name")]
}
\keyword{datasets}
