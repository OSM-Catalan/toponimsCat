\docType{package}
\name{toponimsCat-package}
\alias{toponimsCat}
\alias{toponimsCat-package}
\title{toponimsCat: Eines per ajudar a afegir l'etiqueta "name:ca" a OpenStreetMap}
\description{
Aquest paquet facilita la tasca d'afegir etiquetes «name:ca» i «alt_name:ca» a OSM, generant les ordres per LangToolsOSM, un programa amb python que s'encarrega de fer les consultes i pujar les edicions. Les funcions generen l'estructura de carpetes i ordenen els fitxers per projectes.
}
\details{
  \subsection{Estructura de fitxers d'un projecte}{

    \describe{
      \item{\code{informes}}{Conté fitxers d'informes amb tots els objectes d'OSM segons el filtre i àrea del projecte.}
      \item{\code{revisions}}{Conté fitxers de revisions amb combinacions úniques de les etiquetes \code{«name»}, \code{«name:ca»}, \code{«alt_name:ca»}, \code{«alt_name»}, \code{«translations»}, \code{«ca.wikipedia_page»} i \code{«wikidata_id»}. Aquests fitxers són els que cal revisar abans de preparar les edicions que es carregaran a OSM. Els fitxers revisats s'han de moure a \code{revisions/FET} i són els més valuoso, ja que inclouen la feina humana de revisar tots els casos, i per això té sentit afegir-los al repositori git.}
      \item{\code{edicions}}{Conté fitxers d'informes amb els valors nous dels camps \code{«name:ca»} i \code{«alt_name:ca»}a punt per carregar a OSM, generats a partir dels informes i revisions fetes (\code{revisions/FET}).}
    }
  }

  \subsection{Flux de treball}{

    \enumerate{
      \item{\code{\link{generaInforme}} / \code{\link{generaInformesPPCC}}: retorna ordres per generar informes que es desaran a la carpeta \code{informes}.}
      \item{\code{\link{recompteCasosInformes}}: Recompta el nombre de casos i traduccions dels informes. És optatiu, ja que no modifica res.}
      \item{\code{\link{generaRevisions_regexName}} / \code{\link{generaRevisions_regexTranslations}}: desa els fitxers de revisions a la carpeta \code{revisions} i omple els camps \code{«name:ca»} i \code{«alt_name:ca»} segons regles i expressions regulars.}
      \item{\code{\link{preparaEdicions}}: combina informes i revisions per generar els fitxers amb les edidions a la carpeta \code{edicions} a punt per carregar a OSM.}
      \item{\code{\link{actualitzaInformesCarregats}}: arxiva les edicions carregades i actualitza o elimina els informes desactualitzats.}
    }

  }

  \subsection{Exemples}{

    Podeu veure un exemple de projecte per afegir l'etiqueta \code{name:ca} a carrers dels PPCC que tenen l'etiqueta \code{name} començada amb «[Cc]alle» \href{https://github.com/OSM-Catalan/toponimsCat/tree/main/ppcc/calle-carrer}{aquí}. No és necessari desar tots els fitxers a git, però aquest projecte els inclou a tall d'exemple. També podeu consultar el \href{https://github.com/OSM-Catalan/toponimsCat/blob/main/exec/projecte-PPCC_calle-carrer.r}{codi} per generar i actualitzar el projecte.
  }

}
\seealso{
Enllaços d'interès:
\itemize{
  \item \url{https://github.com/OSM-Catalan/toponimsCat}
  \item Report bugs at \url{https://github.com/OSM-Catalan/toponimsCat/issues}
  \item \href{https://github.com/OSM-Catalan/LangToolsOSM}{LangToolsOSM}
  \item \href{https://wiki.openstreetmap.org/wiki/WikiProject_Catalan/Topon\%C3\%ADmia_i_exotop\%C3\%B2nims}{Espai de coordinació per la toponímia de la comunitat d'OpenStreetMap Països Catalans}
  \item \href{https://www.openstreetmap.org}{OpenStreetMap}
}

}
\author{
\strong{Maintainer}: Joan Maspons \email{joanmaspons@gmail.com}

}
\keyword{package}
