#' Revisions sense errors
#'
#' Crea fitxers de revisions descartant els casos amb errors ortogràfics detectats per \code{\link[hunspell]{hunspell}}.
#'
#' @param fitxersRevisions camins de fitxers de revisions a processar.
#' @param sufix_nou sufix pels fitxers de revisió nous sense els casos amb errors.
#' @param LanguageTool si és \code{TRUE}, cerca errors també amb \code{\link[LanguageToolR]{languagetool}}. Pot ser força lent amb moltes dades.
#' @param disabled_rules identificadors de regles de LanguageTool a deshabilitar.
#'
#' @return Camins dels fitxers nous sense els casos en què \code{name:ca} o \code{alt_name:ca} tenen errors ortogràfics segons el diccionari hunspell (i LanguageTool).
#' @export
#'
#' @examples
revisionsSenseErrors<- function(fitxersRevisions, sufix_nou="_correcte.tsv",
                                LanguageTool=FALSE, disabled_rules=c("UPPERCASE_SENTENCE_START", "ESPAI_DARRERE_PUNTICOMA"), cl=1){
  if (!requireNamespace("hunspell", quietly=TRUE)){
    stop("La funció revisionsSenseErrors requereix el paquet «hunspell» per la detecció d'errors ortogràfics. Instal·leu el paquet i assegureu-vos que el diccionari català està instal·lat al sistema:\n\tintall.packages(\"hunspell\")", call.=FALSE)
  }

  pbapply::pbsapply(fitxersRevisions, function(x){
    message("\nS'està analitzant ", x)
    d<- utils::read.table(x, header=TRUE, sep="\t", quote="\"", check.names=FALSE)
    errors_name.ca<- hunspell::hunspell(as.character(d$`name:ca`), dict=hunspell::dictionary(lang="ca"))
    errors_alt_name.ca<- hunspell::hunspell(as.character(d$`alt_name:ca`), dict=hunspell::dictionary(lang="ca"))

    dCorrecte<- d[sapply(errors_name.ca, length) == 0 & sapply(errors_alt_name.ca, length) == 0, ]
    if (nrow(dCorrecte) == 0){
      return(NA_character_)
    }

    if (LanguageTool){
      if (requireNamespace("LanguageToolR", quietly=TRUE)){
        message("\tLanguageTool for names")
        lt_name.caL<- pbapply::pblapply(as.character(dCorrecte$`name:ca`), function(y){
          if (y %in% c(NA, "")){
            return(data.frame())
          }
          LanguageToolR::languagetool(y, encoding="utf-8", linebreak_paragraph=FALSE, language="ca",
                                      disabled_rules=disabled_rules, enabled_rules=c(),
                                      enabled_only=FALSE, disabled_categories=c(),
                                      enabled_categories=c(), list_unknown=FALSE, apply=FALSE, quiet=TRUE)
        }, cl=cl)
        message("\n\tLanguageTool for alt_names")
        lt_alt_name.caL<- pbapply::pblapply(as.character(dCorrecte$`alt_name:ca`), function(y){
            if (y %in% c(NA, "")){
              return(data.frame())
            }
            LanguageToolR::languagetool(y, encoding="utf-8", linebreak_paragraph=FALSE, language="ca",
                                        disabled_rules=disabled_rules, enabled_rules=c(),
                                        enabled_only=FALSE, disabled_categories=c(),
                                        enabled_categories=c(), list_unknown=FALSE, apply=FALSE, quiet=TRUE)
        }, cl=cl)
        ## TODO: afegeix les dades de LT (lt_*name.caL) a d i permet filtrar a posteriori
        lt_name.caL_df<- do.call(rbind, lt_name.caL[sapply(lt_name.caL, nrow) > 0])
        if (!is.null(lt_name.caL_df)){
          utils::write.table(lt_name.caL_df, file=gsub("\\.tsv$", "_LanguageToolERRORS-name:ca.tsv", x), sep="\t", na="", col.names=TRUE, row.names=FALSE)
        }
        lt_alt_name.caL_df<- do.call(rbind, lt_alt_name.caL[sapply(lt_alt_name.caL, nrow) > 0])
        if (!is.null(lt_alt_name.caL_df)){
          utils::write.table(lt_alt_name.caL_df, file=gsub("\\.tsv$", "_LanguageToolERRORS-alt_name:ca.tsv", x), sep="\t", na="", col.names=TRUE, row.names=FALSE)
        }
        dCorrecte<- dCorrecte[sapply(lt_name.caL, nrow) == 0 & sapply(lt_alt_name.caL, nrow) == 0, ]
      } else {
        warning("S'ha omés la revisió amb «LanguateTool» perquè el paquet no és instal·lat. Instal·leu el paquet:\n\tremotes::install_github(\"nevrome/LanguageToolR\")")
      }
    }

    if (nrow(dCorrecte) > 0){
      fitxerNou<- gsub("\\.tsv$", sufix_nou, x)
      utils::write.table(dCorrecte, file=fitxerNou, sep="\t", na="", col.names=TRUE, row.names=FALSE)

      return(fitxerNou)
    } else {
      message("No hi ha missatges sense errors d'ortografia")

      return(NA)
    }

    # TEST:
    # test<- data.frame(`name:ca`=d$`name:ca`, `err.name:ca`=sapply(errors_name.ca, paste, collapse="|"),
    #                   `alt_name:ca`=d$`alt_name:ca`, `err.alt_name:ca`=sapply(errors_alt_name.ca, paste, collapse="|"), check.names=FALSE)
    # View(merge(dCorrecte, test, all=TRUE))

    # Paraules soles: correcte<- hunspell::hunspell_check(x$`name:ca`, dict=hunspell::dictionary(lang="ca")); errors[!correcte]
    # suggeriments<- lapply(errors, hunspell::hunspell_suggest, dict=hunspell::dictionary(lang="ca"))
  })
}
