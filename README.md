
<!-- README.md is generated from README.Rmd. Please edit that file & devtools::build_readme() -->

# toponimsCat: Topònims en català a l’OpenStreetMap

<!-- badges: start -->
<!-- badges: end -->

Eines per ajudar a afegir l’etiqueta `name:ca` i `alt_name:ca`. A la
carpeta `exec/` hi ha scripts d’R per generar i actualitzar els fitxers
de diferents projectes. A la carpeta `PPCC/` i `exotopònims/` hi ha les
dades dels diferents projectes de localització. Per descarregar i pujar
les dades a OpenstreetMap, cal instal·lar
[LangToolsOSM](https://github.com/OSM-Catalan/LangToolsOSM).

## Resum de l’estat dels projectes

|                                                                                                                                             | Objectes | Revisions | Objectes amb nom de wikidata | Revisions amb nom de wikidata |
|:--------------------------------------------------------------------------------------------------------------------------------------------|---------:|----------:|-----------------------------:|------------------------------:|
| [exotopònims/Rússia/estat_wikidata.csv](https://github.com/OSM-Catalan/toponimsCat/blob/main/exotopònims/Rússia/estat_wikidata.csv)         |     1846 |      1582 |                         1846 |                          1582 |
| [PPCC/avenida-avinguda/casos_comarques.csv](https://github.com/OSM-Catalan/toponimsCat/blob/main/PPCC/avenida-avinguda/casos_comarques.csv) |     2510 |       709 |                            2 |                             2 |
| [PPCC/calle-carrer/casos_comarques.csv](https://github.com/OSM-Catalan/toponimsCat/blob/main/PPCC/calle-carrer/casos_comarques.csv)         |     7652 |      4912 |                            0 |                             0 |
| [PPCC/camino-camí/casos_comarques.csv](https://github.com/OSM-Catalan/toponimsCat/blob/main/PPCC/camino-camí/casos_comarques.csv)           |     1960 |       882 |                            0 |                             0 |
| [PPCC/parque-parc/casos_comarques.csv](https://github.com/OSM-Catalan/toponimsCat/blob/main/PPCC/parque-parc/casos_comarques.csv)           |      732 |       623 |                            1 |                             1 |
| [PPCC/plaza-plaça/casos_comarques.csv](https://github.com/OSM-Catalan/toponimsCat/blob/main/PPCC/plaza-plaça/casos_comarques.csv)           |     1351 |       798 |                            3 |                             3 |

Numero de casos pendents d’afegir l’etiqueta «name:ca» per cada
projecte. Per projectes sense revisió unificada, pot haver-hi casos
duplicats en fitxers diferents.

## Estructura de fitxers d’un projecte

-   `informes` Conté fitxers d’informes amb tots els objectes d’OSM
    segons el filtre i àrea del projecte.
-   `revisions` Conté fitxers de revisions amb combinacions úniques de
    les etiquetes «name», «name:ca», «alt_name:ca», «alt_name»,
    «translations», «ca.wikipedia_page» i «wikidata_id». Aquests fitxers
    són els que cal revisar abans de preparar les edicions que es
    carregaran a OSM. Els fitxers revisats s’han de moure a
    `revisions/FET` i són els més valuoso, ja que inclouen la feina
    humana de revisar tots els casos, i per això té sentit afegir-los al
    repositori git.
-   `edicions` Conté fitxers d’informes amb els valors nous dels camps
    «name:ca» i «alt_name:ca» a punt per carregar a OSM, generats a
    partir dels informes i revisions fetes (fitxers a `revisions/FET`).

## Flux de treball i ús de les funcions principals

1.  `generaInforme` / `generaInformesPPCC`: retorna ordres per generar
    informes que es desaran a la carpeta informes.
2.  `recompteCasosInformes`: Recompta el nombre de casos i traduccions
    dels informes. És optatiu, ja que no modifica res.
3.  `generaRevisions_regexName` / `generaRevisions_regexTranslations`:
    desa els fitxers de revisions a la carpeta revisions i omple els
    camps «name:ca» i «alt_name:ca» segons regles i expressions
    regulars.
4.  `preparaEdicions`: combina informes i revisions per generar els
    fitxers amb les edidions a la carpeta edicions a punt per carregar a
    OSM.
5.  `actualitzaInformesCarregats`: arxiva les edicions carregades i
    actualitza o elimina els informes desactualitzats.

## Exemples

Podeu veure un exemple de projecte per afegir l’etiqueta «name:ca» a
carrers dels PPCC que tenen l’etiqueta «name» començada amb «\[Cc\]alle»
a
[PPCC/calle-carrer](https://github.com/OSM-Catalan/toponimsCat/tree/main/PPCC/calle-carrer).
No és necessari desar tots els fitxers a git, però aquest projecte els
inclou a tall d’exemple. També podeu consultar el
[codi](https://github.com/OSM-Catalan/toponimsCat/blob/main/exec/projecte-PPCC_calle-carrer.r)
per generar i actualitzar el projecte.

## Com col·laborar?

La part que requereix més feina és la revisió de les etiquetes «name:ca»
i «alt_name:ca» proposades (fitxers de les carpetes `revisions` dels
diferents projectes). Si voleu encarregar-vos d’algun país en concret o
algun tipus d’objecte als PPCC i no us en sortiu en generar els fitxers,
obriu un [tiquet](https://github.com/OSM-Catalan/toponimsCat/issues) i
miraré de generar-los.

També en podem parlar als espais de la [comunitat OpenStreetMap en
Català](https://wiki.openstreetmap.org/wiki/WikiProject_Catalan#Canals_de_comunicaci%C3%B3_i_mitjans_de_difusi%C3%B3).

### Com buscar projectes amb feina pendent?

Consulteu a
[Overpass](https://www.overpass-turbo.eu/?Q=%5Bout%3Ajson%5D%5Btimeout%3A250%5D%3B%0A%7B%7BgeocodeArea%3APa%C3%AFsos%20Catalans%7D%7D-%3E.searchArea%3B%0A(%0A%20%20nwr%5B!'name%3Aca'%5D%5B'name'~'%5E%5BPp%5Duente%20'%5D(area.searchArea)%3B%0A)%3B%0Aout%20body%3B%0A%3E%3B%0Aout%20skel%20qt%3B&C=40.41768;0.8844;7).
Exemple de consulta per objectes amb etiqueta «name» que comenci amb
«\[Pp\]uente» i no tinguin etiqueta «name:ca»:

    [out:json][timeout:250];
    {{geocodeArea:Països Catalans}}->.searchArea;
    (
      nwr[!'name:ca']['name'~'^[Pp]uente '](area.searchArea);
    );
    out body;
    >;
    out skel qt;

## Instal·lació del paquet d’R

Podeu descarregar l’[última versió del
paquet](https://github.com/OSM-Catalan/toponimsCat/releases/latest)
(fitxer toponimsCat_X.Y.tar.gz) i instal·lar-lo amb:

``` r
install.packages("toponimsCat_X.Y.tar.gz", repos=NULL)
```

Podeu instal·lar la versió en desenvolupament de toponimsCat des de
[GitHub](https://github.com/) amb:

``` r
# install.packages("devtools")
devtools::install_github("OSM-Catalan/toponimsCat")
```

## Enllaços d’interès

-   [LangToolsOSM](https://github.com/OSM-Catalan/LangToolsOSM)
-   Espai de coordinació per la [toponímia i
    exotopònims](https://wiki.openstreetmap.org/wiki/WikiProject_Catalan/Topon%C3%ADmia_i_exotop%C3%B2nims)
    de la comunitat d’OpenStreetMap Països Catalans.
-   [Openstreetmap](https://openstreetmap.org)
-   [taginfo](https://taginfo.openstreetmap.org/keys/name:ca): estat de
    les etiquetes «name:ca» a Openstreetmap.
