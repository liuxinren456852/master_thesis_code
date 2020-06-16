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

  1. Skapa en .pgw-fil med sex rader motsvarande upplösning X-led, skevning X-led, skevning Y-led, upplösning Y-led, startpunkt X-led, startpunkt Yled. T.ex.:  
  `2.54`  
  `0`  
  `0`  
  `-2.54`  
  `516850`  
  `6499635`.
  Döp filen till minst fem tecken som motsvarar området, t.ex. `kvarn_test.pgw` och lägg i en katalog med samma namn.
  2. Ladda ner motsvarande laserdata och ytmodell och lägg i en katalog med de första fem tecknen från pgw-filen. 
  3. Kör `las_data_prep.R` för att sammanfoga data t.ex. `Rscript las_data_prep.R -m ~/master_thesis_code/kvarn_test/ -o ~/master_thesis_code/pvcnn/data/terrain/kvarn_test_out/ -l /media/gustav/storage/laslager/ -p 600x300`. -p argumentet bestämmer hur många pixlar som ska finnas i dummy-data. multipliceras med rad 1 resp 4 i .pgw-filen för att få områdets dimensioner.
  4. Kör `pvcnn/data/terrain/prepare_data.py` för att skapa h5-filer. Sätt argumenten `-f`, `-d` och `-o` till samma sökväg som `-o` i steg 3.
  5. Se till att rätt area-nummer sätts som test-area i `pvcnn/configs/terrain/[modell]/test_area/__init__.py` samt rätt datakälla i `pvcnn/configs/terrain/__init__.py` (raden `configs.predict.root`). Ta bort `.area`-filen under varje Area-katalog.
  6. Kör `pvcnn/train.py` med argumentet `--predict`.
  7. Kör `pvcnn_full_area_pred.R` med new_area-argumentet saqtt till TRUE, t.ex. `Rscript pvcnn_full_area_pred.R -p ~/master_thesis_code/pvcnn/data/terrain/kvarn_test_out/ -m ~/master_thesis_code/kvarn_test -w 0.5 -n TRUE`. Detta ger en ny mapp `eval` under katalogen med predictions där kartorna finns.

## R-Filer

Merparten av filerna har egna kommentarer och förklaringar, framförallt av des argument. Här följer dock en lite förklaring av vad de flesta av dem används till.

### `dt_segment_lookup.R`

Innehåller funktionsfabrikerna som används i `las_data_prep`, framförallt `dt_fuzzy_join_factory()` som returnerar en funktion som gör lookups i flera target-data från ett source-data. Innehåller även specifikationen för hur sammanslagningarna görs (t.ex. närmaste punkt (`dt_closest()`) eller medelvärde av omgivande `dt_sq_average()`).

### `las_data_prep.R`

Skript för att slå samman laserdata, ytmodell och orienteringskarta (om det inte är prediktionsdata som ska skapas i vilket fall dimensionerna på den tänkta orienteringskartan anges under `-p`-argumentet).

### `las_from_npy.R`

Används för att skapa las-filer från en samling npy-filer. Främst tänkt för att lagra de färdigsammanslagna data i ett format som kan visualiseras eller användas för att skapa nya material.

### `las_reader.R`

Hjälpfunktioner för inläsningen av las-filer i `las_data_prep.R`.

### `little_helpers.R`

Diverse små plotfunktioner mm som används lite här och var i olika skript.

### `map_grid_maker.R`

Innehåller funktionen som skapa en grid av segment från en orienteringskarta.

### `model_comp.R`

Script för att skapa tabellerna i modelljämförelsekapitlet i uppsatsen. Själva siffrorna kommer från confusion-matrix-printen i slutet av `pvcnn_pred_grid_compare.R`.

### `MLR.R`

Script för att ta ett antal las-filer, dela in dem i voxlar och köra multipel logistisk regression på dessa. Sparar även undan såväl data som resultat och tar därför en del disk i anspråk (ca5-600mb) samt en del tid. Saknar indikator på hur långt skriptet kommit.b

### `npy_label_remaker.R`

Script för att ta bort en etikett i ett färdigsammanslaget material. Ser till att etiketterna som är kvar är i följd även om det egentligen inte behövs.

### `omap_segment_extraction.R`

Nödlösningsscript för att ta ut klassfördelning ur orienteringskartorna givet output från `test_split.R`.

### `omap_stats_maker.R`

Räknar storlek och klassfördelning för orienteringskartorna, sparar deta i en .csv som ka användas för diagram etc.

### `png_map_reader.R`

Innehåller alla funktioner för att läsa in, rensa och spara undan orienteringskartorna från png-filer.

### `pred_plots.R`

Script för att skapa 3-i-1-resultatkartorna i uppsatsen, t.ex. figur 5.1, givet output från `pvcnn_full_area_pred.R`.

### `pvcnn_full_area_pred.R`

Beräknar griddade prediktioner för hela kartor för olika entropi-nivåer. Sparar undan plottar av prediktionerna och entropi.

### `pvcnn-pred_grid_analysis.R`

Diverse småscript, bl.a. analysen av punkttäthetens effekt. Inte vidare strukturerat utan merparten "fulhack" då det bestämdes ganska sent hur den analyses skulle göras.

### `pvcnn-pred_grid_compare.R`

Script för att jämföra prediktionerna med orienteringskartorna for olika entropinivåer. Sparar undan prediktioner och confusion-matriser.

### `seg_list_writer.R`

Innehåller funktionen som sparar undan sammanfogad data som npy-filer. Motsvarar till viss del första delen av `pvcnn/data/terrain/prepare_data.py`.

### `test_split.R`

Script för att dela in segment i test, träning och valideringsdata. Skapar ny Areas för test resp. valideringsdata. Plottar även antalet segment i vardera mängd och område samt sparar totalerna.

### `true_colour_codes.R`

Innehåller funktioner för att skapa mappningen mellan fägkoder och kategorier samt för att plotta denna.

### `.R`


## Python-filer

Eftersom jag inte skrivit alla filerna i PVCNN vill jag inte kommentera dem för mycket utan fokuserar på de saker som är viktiga för arbetet i uppsatsen. Sökvägarna är relativt `pvcnn/`.

### `train.py`

Huvudfilen som trots namnet även används för utvärdering (`--evaluation`) och prediktioner (`--predictions`). Har jmf originalet byggts ut för att skapa punktprediktioner huvudsakligen genom att kopiera beteendet för modellutvärdering. 

### `data/terrain/prepare_data.py`

Scriptet som skapar h5-filer från npy-filerna som `las_data_prep.R` skapar. Hade kunnat skippas med en annan dataLoader (som då hade agerat direkt på npy-filerna), i synnerhet för för prediktionerna, men skapar överlappande block och samplar om punkterna så inalles är den nog fördelaktig för träningen. 

### `configs/...`

Inställningarna för modellerna sätts i en hierarkisk struktur där saker som slumpfrö och rotkatalog för data sätts högt upp och gäller alla modeller och testområden medan förlustfunktionsvikter, voxelupplösning och batchstorlek sätts längre ner för varje enskild modell. Om `train.py` ger märkliga felmeddelanden och till slut hänvisar till `utils/config.py` har du satt ett oacceptabelt värde i någon config-fil under `configs/`.

### `evaluate/terrain/eval.py`

Innehåller funktionen som används för att utvärdera modellen på hela valideringsdata (och inte bara ett sample som görs i varje iteration). Innehåller kod för att göra ogjort den resampling som `data/terrain/prepare_data.py` och därmed avgöra vilken punkt en prediktion gäller som behöver fixas om man bygger en ny dataLoader. Värt att notera är även att varje punkt utvärderas flera gånger och den prediktion som högst konfidens är den som används. Kategorierna är även hårdkodade i filen vilket behöver hanteras om man byter data. Laddar in label-filen för att jämföra med så denna måste finnas i varje segments katalog för att utvärdering ska kunna köras.

### `predict/terrain/pred.py`

I stort sett samma fil som `evaluate/terrain/eval.py` men modifierad för att läsa in xyzrgb-filen för varje segment istället samt producera npy-filer med prediktioner och entropi för varje ensklid punkt istället. Raderna 138-139 kan behöva avkommenteras och 140-144 kommenteras om man ska köra prediktioner på  träningskartorna då dessa gjordes utan att .dataset-markören innehåller antalet punkter i segmentet. Detta är fixat i senare versioner av `las_data_prep.R`.