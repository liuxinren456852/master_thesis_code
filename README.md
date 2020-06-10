---
title: "Framkomlighetsrapport - dokumentation"
author: "Gustav Lundberg"
date: '2020-06-05'
---
## Antaganden och pre-reqs

Denna dokumentation utgår från att Linux körs och alla sökvägar är relativt den som dokumentationen finns i. Det hela \textit{borde} fungera på Windows också men det har inte testats.

Databehandling sker i allmänhet i R (ver > 3.2) och följande R-kod installerar alla paket som behöver vara installerade:

```
install.packages(c("data.table", "lidR", "ggplot2", "pdist", "RcppCNPy", "xtable", "optparse", "stringr", "patchwork", "parallel", "png"))
```

De kan med fördel också installeras i en egen Anaconda-miljö och installeras via Anaconda som då tar hand om alla beroenden. Alla scripten har skrivits i RStudio Server 1.2, men de går att köra utan RStudio installerat och då genom att i en terminal köra `Rscript scriptnamn[argument]`. Alla script har en hjäplfil som kan printas med `Rscript scriptnamn --help` och de olika argumenten förklaras då. Om man inte vill läsa i själva filen vill säga.... Samtliga pythonfiler körs genom att i en terminal skriva `python scriptnamn[argument]`. Pythonfilerna har generellt sett **inte** någon tillhörande hjälpfil utan för dem behöver man titta i själva filen.

För att köra PVCNN++ krävs följande paket (från projektets [Github](https://github.com/mit-han-lab/pvcnn)):

  * Python >= 3.7
  * [PyTorch](https://github.com/pytorch/pytorch) >= 1.3
  * [numba](https://github.com/numba/numba)
  * [numpy](https://github.com/numpy/numpy)
  * [scipy](https://github.com/scipy/scipy)
  * [six](https://github.com/benjaminp/six)
  * [tensorboardX](https://github.com/lanpa/tensorboardX)>= 1.2
  * [tqdm](https://github.com/tqdm/tqdm)
  * [plyfile](https://github.com/dranjan/python-plyfile)
  * [h5py](https://github.com/h5py/h5py)

Utöver dessa rekommenderas CUDA >= 10.2 för att kunna köra modellerna på GPU. Detta antas i resten av dokumentationen och det har inte testats att köra utan detta argument (`--devices 0,1`)

\newpage
## Datakällor

### Orienteringskartor

Dessa finns i fyra "nivåer":

\paragraph{Råa orienteringskartor}
Lagras  i `/ocd_kartor` dels som `.ocd` som är de kartor som Marcus Henriksson gav och dels som `.omap` som är de förenklade kartorna från vilka png-filerna exporteras. Kräver [OpenOrienteeringMapper](https://www.openorienteering.org/apps/mapper/) (OOM) för att hantera.

\paragraph{.png exporter}
Finns i `/kartor` och är de oredigerade exporterna från OOM samt tillhörande world-fil (`.pgw`) med koordinater.

\paragraph{Korrigerade kartor}
Lagras i `/omap_corrected` och är kartorna som de ser ut efter första körningen i `.png_map_reader()`, dvs när alla pixlar är en av de åtta tillåtna.

\paragraph{Rensade kartor}
Lagras i `/omap_cleaned` och är kartorna efter att de rensats från enstaka pixlar med "fel" kategori. Här lagras också en `.Rdata`-fil med kartan som en data.table, det format som används i alla script. 

Ska man göra en ny karta kommer denna inte att finnas ibland de korrigerade utan endast som rensad. world-filen behövs även om det finns en `.Rdata`-fil. Kartan ska döpas som [Område]_[väderstreck] där område generellt är vilken orienteringskarta den kommer från då de första fem bokstäverna i områdesnamnet används för att söka upp vilka laserdata och ytmodelldata som ska användas.

### Laserdata

Lagras i `../laslager/[område]/laserdata` som `.las` eller `.laz`-filer. Kan ligga var som helst, men slutnivåerna `/[område]/laserdata` är hårdkodad på de flesta ställen medan resten av sökvägen kan ges som argument. Dessa bör täcka de områden som orienteringskartorna för samma område, men inte onödigt mycket mer så inläsningen isåfall tar lång tid. Data som använts har hämtats via [SLU](https://zeus.slu.se/get/?drop=) men levereras ursprungligen av Lantmäteriet. 

### Ytmodell

Finns i `../laslager/[område]/ytmodell` och samma villkor gäller för dessa som för laserdata. 

### Andra data

Skulle man vilja lägga till ytterligare datakällor till modellen behöver man se till att dessa läses in i `las_data_prep.R` som en data.table med variablerna X, och Y samt de data som man vill joina med. Man behöver också ändra antalet variabler i `pvcnn/configs/terrain/[modell]/__init__.py` på raden `configs.model.extra_feature_channels` så att det är antalet variabler man joinar in +3 (för normaliserade koordinater).

## Modellträning

Databehandlingen illustreras i figur 2.8 i rapporten, men kortfattat är stegen:

  1. Exportera en png-karta med tillhörande pgw-fil från OOM
  2. Ladda ner motsvarande laserdata och ytmodell
  3. Kör `las_data_prep.R` för att sammanfoga data
  4. Kör `pvcnn/data/terrain/prepare_data.py` för att skapa h5-filer
  5. Kör `pvcnn/train.py` för att träna modellen.

## Modellutvärdering

För att utvärdera en modell:

  1. Kopiera in label-filerna till h5-katalogen för utvärderingsdata med `pvcnn/utils/labelbcopy.py` 
  2. Kör `pvcnn/train.py` med argumentet `--evaluate`.
  3. För att skapa confusion matrix (i exemplet för area 16) för enskilda punkter körs i en R-session :
```
  source(little_helpers.R)
  confusion_matrix_xtable(paste0("PVCNN++, c=", opts$width), "validation data points",
    cm_path = paste0("pvcnn/runs/terrain.pvcnn2.test_area.",prefix,"/best_area_16.conf_mat.npy"))
```
  4. För att skapa confusion matrix för grid units körs `pvcnn_pred_grid_compare.R`. I den filen finns även (bortkommenterat) koden för att skapa de tre bilderna för varje segment som används i diskussionskapitlet.

## Prediktioner

Prediktionsrutinen är i stort sett densamma som för utvärdering med undantag för att koordinatfilen måste kopieras in istället för label-filen. Notera att listan nedan inte är testad i sin helhet utan bara steg för steg.
För att bara skapa prediktioner för ett datamaterial:

  1. Exportera en png-karta med tillhörande pgw-fil från OOM ELLER
  2. Skapa en .png-fil och .pgw-fil manuellt om orienteringskarta saknas. Kan vara enfärgad, men antalet pixlar samt värdena i .pgw-filen avgör hur griddningen och därmed kartorna görs.
  3. Ladda ner motsvarande laserdata och ytmodell
  4. Kör `las_data_prep.R` för att sammanfoga data
  5. Kör `pvcnn/data/terrain/prepare_data.py` för att skapa h5-filer
  6. Se till att rätt area-nummer sätts som test-area i `pvcnn/configs/terrain/[modell]/test_area/__init__.py`
  7. Kopiera in xyzrgb-filerna till h5-katalogen med `pvcnn/utils/xyzrgbcopy.py`
  8. Kör `pvcnn/train.py` med argumentet `--predict`.
  

