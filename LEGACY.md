# Oralstats — earlier versions, classic modules and intro video

> This document gathers everything related to the **previous versions of Oralstats** (≤ v1.3) and its classic `oralstats.view` / `oralstats.creation` modules. For the current, actively maintained tool see the main **[README](README.md)** (Oralstats v1.8 — LASP).

## Milestones

- 🔴 **Version 1.3 (beta)** (30/06/2022): with this version it became possible to create and export files directly in the same Shiny environment.
- Version 1.2 and 1.1: earlier viewing-only releases (see demos below).

## Brief introduction video

A short introduction to the classic Oralstats interface:

https://youtu.be/SoELWc8N3zI

## Demos (older versions)

You can see an available demo for the classic version (1.3) at this [link](https://adrin-cabedo.shinyapps.io/oralstats_v_1_3/). You can also check other versions of `oralstats.view` at this [link](https://adrin-cabedo.shinyapps.io/padrezorro/) (old version, version 1.1) or at this [link](https://adrin-cabedo.shinyapps.io/fonocortesia2/) (version 1.2). These demos allow the exploration of intonational phrases for two of the *mods* explained below: first one, *Padre Zorro among genres*; second one, *Fonocortesia*.

## Viewing (classic module, v1.3)

With `oralstats.view` you can choose from a huge variety of statistical methods (decision trees, discriminant analysis, ANOVAs) but also from several visualization procedures (pie charts, frequency tables, timeline charts...). See the following figures to take a look at some of these features:

![](images/first_page.png)

Figure 1. Welcome page.

![](images/create.png)

Figure 2. Basic creation page (not all functions available).

![](images/filter.png)

Figure 3. Filter.

![](images/descriptive_by_group.png)

Figure 4. Description by group.

![](images/overall_description.png)

Figure 5. Variables descriptive statistics.

![](images/chisquare.png)

Figure 6. Chisquare

![](images/corrplot.png)

Figure 7. Correlation plot

![](images/decision_tree.png)

Figure 8. Decision tree plot (using Rpart and Party libraries)

![](images/heatmap.png)

Figure 9. Heatmap

![](images/ANOVA.png)

Figure 10. ANOVA and boxplots

![](images/PCA.png)

Figure 11. PCA and Cluster analysis

![](images/ngrams.png)

Figure 12. N-grams

![](images/pitch_raw_audio.png)

Figure 13. Pitch or intensity visualization and audio

![](images/linear_prosody.png)

Figure 14. Linear prosodic behaviour among file or corpus

![](images/pitch_contour.png)

Figure 15. Pitch contour with TOBI tagging

## R packages for the classic modules

- **view:** DBI; RSQLite; shinybusy; psych; shiny; RColorBrewer; tidyverse; shinyWidgets; tidytext; readr; heatmaply; RLumShiny; party; gplots; FactoMineR
- **creation:** tidyverse; sqldf; udpipe; xfun; readbulk; readxl

**Note:** UDPipe needs a specific language file to deal with the language used in the data. By default a Spanish file is used, but you can use any other language file covering your needs.

## Notes on some tonal and semantic transformations

Some categorization procedures are still being improved. So, although they are consistent among all the files you can process, you should pay attention to these variables and their corresponding variants:

1. **TOBI** (Tone and Break Indices, Pierrehumbert 1993 and Estebas & Prieto 2008). We calculated some tone categories related to the tonic vowels of the utterances, but this procedure is a little basic compared with other computational solutions available out there. If you are interested in better TOBI transformations for Spanish, you should check the work done by Wendy Elvira in EtiTOBI (Elvira-García et alii 2016): <https://github.com/wendyelviragarcia/eti_ToBI>

2. **SMA/AMH** (Speech melodic analysis / análisis melódico del habla, Cantero & Font 2012). This is a percentual codification of the pitch rises and falls. Specific improvements, adding intensity and duration codifications, can be followed in references like Cantero (2019).

3. **Sentiment analysis**. The words-transformation part of the script will add sentiment analysis based on a simple correlation with an annotated list of words and corresponding sentiments. This is a very basic annotation done with a joining Tidyverse approach; nevertheless, it does not compute strong sentiment assignations as well as it is done in other R packages, like Syuzhet (<https://github.com/mjockers/syuzhet>), or in other Python solutions (Taboada et alii 2011, <https://github.com/sfu-discourse-lab/SO-CAL>).

   **Note:** We used sentiment analysis and the explained Tidyverse approach to partially support the PhD thesis of [Danny Murillo](https://www.researchgate.net/profile/Danny-Murillo-Lanza) (in elaboration), but you could use any other tagging process. The process only needs a list of words or expressions and their meaning, being this a sentiment or another conceptual factor, such as politeness, humor, mitigation or whatever element that could be interesting for a researcher.

## Mods

As scientific interests can differ substantially from one researcher to another, there are different projects where Oralstats is directly involved. The ones below are just some of them, but the point is that they can be incremented as Oralstats is also being improved. All these projects, with the exception of the *Discourse genre and idiolectal analysis from politicians* and the analysis of the youtuber *Pico de Oro/Padre Zorro*, are still being developed.

1. *Sahelanthropus*. This modification was developed to help researchers characterize the speech of specific individuals. More specifically, the main idea is to compare the speech features of one person with other individuals to see in what sense these features are really shared with a speaker community or if they are idiosyncratic of this individual. So, basically, it compares the speech of one individual with the average speech coming from a global corpus.

2. *Ozymandias*. A modification developed to deal with linked linguistic data. The aim is to analyze different discourse units (words, intonational phrases, subacts, speech acts) defined at the Val.Es.Co. speech units framework (Pons [ed.] 2014 and <http://www.valesco.es>).

3. *Discourse genre and idiolectal analysis from politicians.* This is a small sample of 4 different Spanish politicians speaking along multiple oral discourse genres. You can see a demo here: [https://adrin-cabedo.shinyapps.io/oralstatsgenres](https://adrin-cabedo.shinyapps.io/oralstatsgenres/?_ga=2.267120075.498562318.1634906108-483580461.1634906108)

4. *Padre zorro among genres*. This version includes intonational phrases, words and phonemes recorded from a Spanish youtuber called Padre Zorro/Pico de Oro. Data proceed from three different online formats: Twitch monologue, YouTube edited monologue and a YouTube polemic interview. You can check the demo here: <https://adrin-cabedo.shinyapps.io/padrezorro/>

5. *Fonocortesía*. This was a funded project ended in 2013 and supervised by full professor Antonio Hidalgo Navarro. You can access this data dynamically at <http://fonocortesia.es> and also <https://adrin-cabedo.shinyapps.io/fonocortesia/> (old Oralstats version) or <https://adrin-cabedo.shinyapps.io/fonocortesia2/> (new Oralstats version [recommended]).

6. *Aroca* (in collaboration with Andrea Carcelén Guerrero). This is a special version and simplification of the Oralstats core package: adrin-cabedo.shinyapps.io/aroca_viewer/ Nowadays, it includes more than 150 Spanish conversations from the Ameresco corpus, covering 14 Latin-American cities <www.corpusameresco.com>. The specific page for this mod can be found at: <https://github.com/acabedo/aroca>
