#' Revisions sense errors
#'
#' Crea fitxers de revisions descartant els casos amb errors ortogràfics detectats per \code{\link[hunspell]{hunspell}}.
#'
#' @param fitxersRevisions camins de fitxers de revisions a processar.
#' @param sufix_nou sufix pels fitxers de revisió nous sense els casos amb errors.
#' @param LanguageTool si és \code{TRUE}, cerca errors també amb \code{\link[LanguageToolR]{languagetool}}. Pot ser força lent amb moltes dades.
#'
#' @return Camins dels fitxers nous sense els casos en què name:ca o alt_name:ca tenen errors ortogràfics segons el diccionari hunspell.
#' @export
#'
#' @examples
revisionsSenseErrors<- function(fitxersRevisions, sufix_nou="_correcte.tsv", LanguageTool=FALSE){
  if (!requireNamespace("hunspell", quietly=TRUE)){
    stop("La funció revisionsSenseErrors requereix el paquet «hunspell» per la detecció d'errors ortogràfics. Instal·leu el paquet i assegureu-vos que el diccionari català està instal·lat al sistema:\n\tintall.packages(\"hunspell\")", call.=FALSE)
  }

  sapply(fitxersRevisions, function(x){
    d<- utils::read.table(x, header=TRUE, sep="\t", quote="\"", check.names=FALSE)
    errors_name.ca<- hunspell::hunspell(as.character(d$`name:ca`), dict=hunspell::dictionary(lang="ca"))
    errors_alt_name.ca<- hunspell::hunspell(as.character(d$`alt_name:ca`), dict=hunspell::dictionary(lang="ca"))

    dCorrecte<- d[sapply(errors_name.ca, length) == 0 & sapply(errors_alt_name.ca, length) == 0, ]
    if (nrow(dCorrecte) == 0){
      return(NA_character_)
    }

    if (LanguageTool && requireNamespace(LanguageToolR, quietly=TRUE)){
      lt_name.caL<- lapply(as.character(dCorrecte$`name:ca`), LanguageToolR::languagetool, encoding="utf-8",
                           linebreak_paragraph=FALSE, language="ca", disabled_rules=c(),
                           enabled_rules=c(), enabled_only=FALSE, disabled_categories=c(),
                           enabled_categories=c(), list_unknown=FALSE, apply=FALSE, quiet=TRUE)
      lt_alt_name.caL<- lapply(as.character(dCorrecte$`alt_name:ca`), LanguageToolR::languagetool, encoding="utf-8",
                           linebreak_paragraph=FALSE, language="ca", disabled_rules=c(),
                           enabled_rules=c(), enabled_only=FALSE, disabled_categories=c(),
                           enabled_categories=c(), list_unknown=FALSE, apply=FALSE, quiet=TRUE)
      dCorrecte<- dCorrecte[sapply(lt_name.caL, nrow) == 0 & sapply(lt_alt_name.caL, nrow) == 0, ]
    } else {
      warning("S'ha omés la revisió amb «LanguateTool» perquè el paquet no és instal·lat. Instal·leu el paquet:\n\tremotes::install_github(\"nevrome/LanguageToolR\")")
    }

    fitxerNou<- gsub("\\.tsv$", sufix_nou, x)
    utils::write.table(dCorrecte, file=fitxerNou, sep="\t", na="", col.names=TRUE, row.names=FALSE)

    return(fitxerNou)

    # TEST:
    # test<- data.frame(`name:ca`=d$`name:ca`, `err.name:ca`=sapply(errors_name.ca, paste, collapse="|"),
    #                   `alt_name:ca`=d$`alt_name:ca`, `err.alt_name:ca`=sapply(errors_alt_name.ca, paste, collapse="|"), check.names=FALSE)
    # View(merge(dCorrecte, test, all=TRUE))

    # Paraules soles: correcte<- hunspell::hunspell_check(x$`name:ca`, dict=hunspell::dictionary(lang="ca")); errors[!correcte]
    # suggeriments<- lapply(errors, hunspell::hunspell_suggest, dict=hunspell::dictionary(lang="ca"))
  })
}
