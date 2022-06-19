## Topònims en català a l'OpenStreetMap

Eines per ajudar a afegir l'etiqueta ```name:ca```. Podeu descarregar el paquet d'R [toponimsCat](https://github.com/OSM-Catalan/toponimsCat/releases). A la carpeta ```exec/``` hi ha scripts d'R per generar i actualitzar els fitxers de diferents projectes. A la carpeta ```ppcc/``` i ```exotopònims/``` hi ha les dades dels diferents projectes de localització. Per les descarregar i pujar les dades a OpenstreetMap, cal instal·lar [LangToolsOSM](https://github.com/OSM-Catalan/LangToolsOSM).


## Estructura de fitxers d’un projecte

* ```informes``` Conté fitxers d’informes amb tots els objectes d’OSM segons el filtre i àrea del projecte.
* ```revisions``` Conté fitxers de revisions amb combinacions úniques de les etiquetes «name», «name:ca»,
«alt_name:ca», «alt_name», «translations», «ca.wikipedia_page» i «wikidata_id».
Aquests fitxers són els que cal revisar abans de preparar les edicions que es carregaran a
OSM. Els fitxers revisats s’han de moure a revisions/FET i són els més valuoso, ja que
inclouen la feina humana de revisar tots els casos, i per això té sentit afegir-los al repositori
git.
* ```edicions``` Conté fitxers d’informes amb els valors nous dels camps «name:ca» i «alt_name:ca» a
punt per carregar a OSM, generats a partir dels informes i revisions fetes (revisions/FET).


## Flux de treball

1. ```generaInforme``` / ```generaInformesPPCC```: retorna ordres per generar informes que es desaran a la carpeta informes.
2. ```recompteCasosInformes```: Recompta el nombre de casos i traduccions dels informes. És optatiu, ja que no modifica res.
3. ```generaRevisions_regexName``` / ```generaRevisions_regexTranslations```: desa els fitxers
de revisions a la carpeta revisions i omple els camps «name:ca» i «alt_name:ca» segons
regles i expressions regulars.
4. ```preparaEdicions```: combina informes i revisions per generar els fitxers amb les edidions a
la carpeta edicions a punt per carregar a OSM.
5. ```actualitzaInformesCarregats```: arxiva les edicions carregades i actualitza o elimina els
informes desactualitzats.


## Exemples

Podeu veure un exemple de projecte per afegir l’etiqueta name:ca a carrers dels PPCC que tenen
l’etiqueta name començada amb «[Cc]alle» [ppcc/calle-carrer](https://github.com/OSM-Catalan/toponimsCat/tree/main/ppcc/calle-carrer). No és necessari desar tots els fitxers a git, però aquest projecte els inclou a tall d’exemple. També podeu consultar el [codi](https://github.com/OSM-Catalan/toponimsCat/blob/main/exec/projecte-PPCC_calle-carrer.r) per generar i actualitzar
el projecte.


## Com col·laborar?

Parlem-ne a als espais de la [comunitat OpenStreetMap en Català](https://wiki.openstreetmap.org/wiki/WikiProject_Catalan#Canals_de_comunicaci%C3%B3_i_mitjans_de_difusi%C3%B3) o en aquest mateix espai de github.


## Enllaços d’interès

* [LangToolsOSM](https://github.com/OSM-Catalan/LangToolsOSM)
* Espai de coordinació per la [toponímia i exotopònims](https://wiki.openstreetmap.org/wiki/WikiProject_Catalan/Topon%C3%ADmia_i_exotop%C3%B2nims) de la comunitat d’OpenStreetMap Països Catalans.
