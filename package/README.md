# oralstats

Aplicación Shiny (Oralstats v1.8 "LASP") para explorar y visualizar habla
transcrita con análisis prosódico, léxico, GAMM e informes.

## Instalación

```r
# install.packages("remotes")
remotes::install_github("acabedo/oralstats", subdir = "package")
```

## Uso

```r
library(oralstats)
run_app()
```

## Funciones que usan Python (opcional)

El análisis acústico (Parselmouth) y de sentimiento/emoción usa Python. Es
opcional: el resto de la app funciona sin él. Para activarlo una sola vez:

```r
install_oralstats_python("core")   # parselmouth, tgt (ligero)
install_oralstats_python("text")   # + sentimiento/emoción
install_oralstats_python("asr")    # + transcripción (descarga grande)
```
