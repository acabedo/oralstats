#!/usr/bin/env bash
# Prueba de instalación en limpio de OralStats SIN contaminar el entorno real.
# Oculta temporalmente el venv heredado y aísla la caché de renv, reproduce desde
# cero en una copia temporal del repo y arranca la app. Restaura todo al salir.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"       # .../Oralstats (el script vive en tests/)
TMP="$(mktemp -d)"
echo ">> Carpeta temporal: $TMP"

# Aislar la caché de renv para no reutilizar librerías ya compiladas.
export RENV_PATHS_ROOT="$TMP/renv-root"

# 1) Ocultar (reversible) el venv heredado bert-env y el venv del proyecto.
BERT="$HOME/.virtualenvs/bert-env"
PROJ="$HOME/.virtualenvs/oralstats-env"
BERT_BAK=""; PROJ_BAK=""

restore() {
  [ -n "$BERT_BAK" ] && [ -d "$BERT_BAK" ] && mv "$BERT_BAK" "$BERT" && echo ">> bert-env restaurado"
  [ -n "$PROJ_BAK" ] && [ -d "$PROJ_BAK" ] && mv "$PROJ_BAK" "$PROJ" && echo ">> oralstats-env restaurado"
  echo ">> Copia temporal conservada en $TMP (bórrala con: rm -rf \"$TMP\")"
}
trap restore EXIT

[ -d "$BERT" ] && BERT_BAK="$BERT.cleanbak.$$" && mv "$BERT" "$BERT_BAK" && echo ">> bert-env ocultado"
[ -d "$PROJ" ] && PROJ_BAK="$PROJ.cleanbak.$$" && mv "$PROJ" "$PROJ_BAK" && echo ">> oralstats-env ocultado"

# 2) Copiar el repo a la carpeta temporal (sin librerías ni caches).
rsync -a --exclude 'renv/library' --exclude 'renv/python' --exclude '.git' \
      "$REPO_ROOT/" "$TMP/Oralstats/"

# 3) Reproducir desde cero y arrancar.
cd "$TMP/Oralstats"
Rscript -e "install.packages('renv', repos='https://cloud.r-project.org'); renv::restore(prompt = FALSE)"
echo ">> Arrancando OralStats desde cero. Revisa el checklist de smoke-test del spec."
Rscript run.R
