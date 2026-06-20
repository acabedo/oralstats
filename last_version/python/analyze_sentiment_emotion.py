#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
analyze_sentiment_emotion.py
Análisis de sentimiento (texto) y emociones (audio) para grupos entonativos.

Uso:
  python3 analyze_sentiment_emotion.py \
    --ips_file ips_resumen.tsv \
    --wav_dir /ruta/a/wavs/ \
    --output_file resultados_sentimiento.tsv \
    --lang es \
    [--no_audio]

Entrada (ips_resumen.tsv): TSV con columnas obligatorias:
  hablante, grupo_fonico, grupo_entonativo, tmin_ip, tmax_ip, palabras, [archivo_origen]

Salida (resultados_sentimiento.tsv): TSV con columnas añadidas:
  sentimiento, prob_pos, prob_neg, prob_neu,
  emocion_texto, prob_emocion_texto,
  emocion_audio, prob_emocion_audio,
  emociones_audio_detalle (JSON con todas las probabilidades)

Dependencias:
  pip install pysentimiento torch torchaudio funasr soundfile
"""

import argparse
import json
import os
import sys
import warnings
import tempfile

warnings.filterwarnings("ignore")

def get_best_device():
    """Detecta el mejor dispositivo disponible: MPS (Apple Silicon) > CUDA > CPU."""
    try:
        import torch
        if torch.backends.mps.is_available() and torch.backends.mps.is_built():
            return "mps"
        if torch.cuda.is_available():
            return "cuda"
    except Exception:
        pass
    return "cpu"

def main():
    parser = argparse.ArgumentParser(description="Análisis de sentimiento y emociones para grupos entonativos")
    parser.add_argument("--ips_file", required=True, help="Archivo TSV con resumen de IPs")
    parser.add_argument("--wav_dir", default=None, help="Directorio con archivos WAV")
    parser.add_argument("--output_file", required=True, help="Archivo TSV de salida")
    parser.add_argument("--lang", default="es", choices=["es", "ca", "en"], help="Idioma para análisis textual")
    parser.add_argument("--no_audio", action="store_true", help="Omitir análisis de emociones por audio")
    parser.add_argument("--model_audio", default="base", choices=["base", "large", "seed"],
                        help="Tamaño del modelo emotion2vec+")
    args = parser.parse_args()

    import pandas as pd
    import numpy as np

    # =============================================
    # 1. Cargar datos de IPs
    # =============================================
    print("STATUS:Cargando datos de grupos entonativos...", flush=True)
    ips = pd.read_csv(args.ips_file, sep="\t", quotechar='"')

    # Mapear nombres de columnas alternativos (compatibilidad con app_parselmouth.R)
    col_aliases = {
        "inicio": "tmin_ip",
        "fin": "tmax_ip",
        "palabras_grupo": "palabras",
    }
    for old_name, new_name in col_aliases.items():
        if old_name in ips.columns and new_name not in ips.columns:
            ips.rename(columns={old_name: new_name}, inplace=True)

    required_cols = ["hablante", "grupo_fonico", "grupo_entonativo", "tmin_ip", "tmax_ip", "palabras"]
    missing = [c for c in required_cols if c not in ips.columns]
    if missing:
        print(f"ERROR:Faltan columnas requeridas: {', '.join(missing)}", file=sys.stderr)
        print(f"STATUS:Columnas disponibles: {', '.join(ips.columns.tolist())}", flush=True)
        sys.exit(1)

    n_ips = len(ips)
    print(f"STATUS:Encontrados {n_ips} grupos entonativos", flush=True)

    # =============================================
    # 2. Análisis de sentimiento textual (pysentimiento)
    # =============================================
    print("STATUS:Cargando modelo de sentimiento textual...", flush=True)

    # Inicializar columnas
    ips["sentimiento"] = ""
    ips["prob_pos"] = 0.0
    ips["prob_neg"] = 0.0
    ips["prob_neu"] = 0.0
    ips["emocion_texto"] = ""
    ips["prob_emocion_texto"] = 0.0
    ips["emociones_texto_detalle"] = ""

    lang_sent = args.lang
    if lang_sent == "ca":
        lang_sent = "es"
        print("STATUS:Catalán: usando modelo español para análisis textual", flush=True)

    device = get_best_device()
    print(f"STATUS:Dispositivo: {device}", flush=True)

    try:
        from pysentimiento import create_analyzer

        sentiment_analyzer = create_analyzer(task="sentiment", lang=lang_sent, device=device)
        print(f"STATUS:Modelo de sentimiento cargado ({device})", flush=True)

        emotion_text_analyzer = create_analyzer(task="emotion", lang=lang_sent, device=device)
        print("STATUS:Modelo de emociones textuales cargado", flush=True)

        # Procesamiento por lotes para mayor velocidad
        BATCH_SIZE = 32
        textos = [str(row["palabras"]).strip() for _, row in ips.iterrows()]
        valid_idx = [i for i, t in enumerate(textos) if t and t != "nan"]
        valid_texts = [textos[i] for i in valid_idx]

        for batch_start in range(0, len(valid_texts), BATCH_SIZE):
            batch = valid_texts[batch_start:batch_start + BATCH_SIZE]
            batch_idx = valid_idx[batch_start:batch_start + BATCH_SIZE]
            try:
                sent_results = [sentiment_analyzer.predict(t) for t in batch]
                emo_results  = [emotion_text_analyzer.predict(t) for t in batch]
                for j, (i, sr, er) in enumerate(zip(batch_idx, sent_results, emo_results)):
                    ips.at[i, "sentimiento"] = sr.output
                    ips.at[i, "prob_pos"] = round(sr.probas.get("POS", 0.0), 4)
                    ips.at[i, "prob_neg"] = round(sr.probas.get("NEG", 0.0), 4)
                    ips.at[i, "prob_neu"] = round(sr.probas.get("NEU", 0.0), 4)
                    ips.at[i, "emocion_texto"] = er.output
                    ips.at[i, "prob_emocion_texto"] = round(max(er.probas.values()), 4)
                    ips.at[i, "emociones_texto_detalle"] = json.dumps(
                        {k: round(v, 4) for k, v in er.probas.items()}, ensure_ascii=False)
            except Exception as e:
                print(f"WARNING:Error lote {batch_start}: {e}", file=sys.stderr)
            processed_so_far = min(batch_start + BATCH_SIZE, len(valid_texts))
            print(f"PROGRESS_TEXT:{processed_so_far}/{len(valid_texts)}", flush=True)

        # Rellenar vacíos
        for i in range(n_ips):
            if ips.at[i, "sentimiento"] == "":
                ips.at[i, "sentimiento"] = "NEU"
                ips.at[i, "prob_neu"] = 1.0
                ips.at[i, "emocion_texto"] = "others"
                ips.at[i, "prob_emocion_texto"] = 1.0

        print(f"STATUS:Análisis textual completado para {n_ips} IPs", flush=True)

    except ImportError:
        print("WARNING:pysentimiento no instalado. Omitiendo análisis textual.", file=sys.stderr)
        print("WARNING:Instalar con: pip install pysentimiento", file=sys.stderr)

    # =============================================
    # 3. Análisis de emociones por audio (emotion2vec+)
    # =============================================
    ips["emocion_audio"] = ""
    ips["prob_emocion_audio"] = 0.0
    ips["emociones_audio_detalle"] = ""

    has_audio = (
        not args.no_audio
        and args.wav_dir is not None
        and os.path.isdir(args.wav_dir)
    )

    if has_audio:
        print("STATUS:Cargando modelo de emociones por audio (emotion2vec+)...", flush=True)

        try:
            from funasr import AutoModel
            import soundfile as sf

            model_map = {
                "seed": "iic/emotion2vec_plus_seed",
                "base": "iic/emotion2vec_plus_base",
                "large": "iic/emotion2vec_plus_large",
            }
            model_id = model_map[args.model_audio]

            # Seleccionar dispositivo para FunASR
            audio_device = get_best_device()
            # FunASR acepta "cuda", "mps" o "cpu" como device
            audio_model = AutoModel(model=model_id, hub="hf", device=audio_device)
            print(f"STATUS:Modelo emotion2vec+ ({args.model_audio}) cargado en {audio_device}", flush=True)

            # Mapear archivos WAV disponibles
            wav_files = {}
            for f in os.listdir(args.wav_dir):
                if f.lower().endswith((".wav", ".flac", ".mp3", ".ogg")):
                    base = os.path.splitext(f)[0]
                    wav_files[base] = os.path.join(args.wav_dir, f)
                    wav_files[f] = os.path.join(args.wav_dir, f)

            print(f"STATUS:Encontrados {len(wav_files)//2} archivos de audio", flush=True)

            # Determinar qué columna usar para identificar el archivo
            has_archivo = "archivo_origen" in ips.columns

            # Caché de WAVs: cada archivo fuente se lee UNA SOLA VEZ y queda en memoria.
            # Con 1221 GEs de un mismo archivo, evita leer el WAV 1221 veces (ahorro >90%).
            wav_cache = {}  # wav_path → (audio_data_mono_float32, sample_rate)

            # Clases estándar de emotion2vec+ (fallback si el resultado no trae labels)
            emo_labels = ["angry", "disgusted", "fearful", "happy", "neutral",
                          "other", "sad", "surprised", "unknown"]

            processed = 0
            errors = 0

            for i, row in ips.iterrows():
                tmin = float(row["tmin_ip"])
                tmax = float(row["tmax_ip"])
                duracion = tmax - tmin

                # Saltar segmentos muy cortos (<0.1s)
                if duracion < 0.1:
                    ips.at[i, "emocion_audio"] = "too_short"
                    continue

                # Buscar archivo WAV correspondiente
                wav_path = None
                if has_archivo:
                    archivo = str(row["archivo_origen"])
                    base = os.path.splitext(archivo)[0]
                    for key in [archivo, base, base.replace("_texto", "").replace("_text", "")]:
                        if key in wav_files:
                            wav_path = wav_files[key]
                            break

                # Si no hay archivo_origen, usar el primer (o único) WAV
                if wav_path is None and len(wav_files) > 0:
                    # Si hay hablante, intentar match
                    hablante = str(row.get("hablante", ""))
                    for key, path in wav_files.items():
                        if hablante and hablante.lower() in key.lower():
                            wav_path = path
                            break
                    # Fallback: usar primer WAV si solo hay uno
                    if wav_path is None:
                        unique_wavs = list(set(wav_files.values()))
                        if len(unique_wavs) == 1:
                            wav_path = unique_wavs[0]

                if wav_path is None:
                    ips.at[i, "emocion_audio"] = "no_wav"
                    errors += 1
                    continue

                try:
                    # Cargar WAV desde caché (evita re-leer el mismo archivo en cada iteración)
                    if wav_path not in wav_cache:
                        audio_full, sr = sf.read(wav_path, dtype="float32")
                        if len(audio_full.shape) > 1:
                            audio_full = audio_full.mean(axis=1)  # stereo → mono
                        wav_cache[wav_path] = (audio_full, sr)
                    audio_data, sr = wav_cache[wav_path]

                    start_sample = int(tmin * sr)
                    end_sample = int(tmax * sr)
                    start_sample = max(0, start_sample)
                    end_sample = min(len(audio_data), end_sample)

                    segment = audio_data[start_sample:end_sample]

                    if len(segment) < int(0.1 * sr):
                        ips.at[i, "emocion_audio"] = "too_short"
                        continue

                    # Pasar el array numpy directamente al modelo (sin escribir archivo temporal)
                    # FunASR acepta input=array + fs=sr para inferencia en memoria
                    result = audio_model.generate(
                        input=segment,
                        fs=sr,
                        output_dir=None,
                        granularity="utterance",
                        extract_embedding=False
                    )

                    if result and len(result) > 0:
                        rec = result[0]
                        # emotion2vec+ devuelve 'labels' y 'scores'
                        if "labels" in rec and "scores" in rec:
                            labels = rec["labels"]
                            scores = rec["scores"]
                        elif "label" in rec:
                            labels = [rec["label"]]
                            scores = [rec.get("score", 1.0)]
                        else:
                            # Formato alternativo
                            labels = emo_labels
                            scores = rec.get("scores", [0]*9)

                        # emotion2vec devuelve etiquetas bilingües "开心/happy"; quedarse
                        # con la parte inglesa (los caracteres CJK rompen los plots en R)
                        labels = [str(lab).split("/")[-1].strip() for lab in labels]

                        # Encontrar emoción dominante
                        if len(scores) > 0:
                            max_idx = scores.index(max(scores)) if isinstance(scores, list) else 0
                            ips.at[i, "emocion_audio"] = labels[max_idx] if max_idx < len(labels) else "unknown"
                            ips.at[i, "prob_emocion_audio"] = round(float(max(scores)), 4)

                            # Detalle completo
                            detail = {}
                            for li, lab in enumerate(labels):
                                if li < len(scores):
                                    detail[lab] = round(float(scores[li]), 4)
                            ips.at[i, "emociones_audio_detalle"] = json.dumps(detail, ensure_ascii=False)

                    processed += 1

                except Exception as e:
                    ips.at[i, "emocion_audio"] = "ERROR"
                    errors += 1
                    print(f"WARNING:Error audio IP {i}: {e}", file=sys.stderr)

                if (i + 1) % 5 == 0 or i == n_ips - 1:
                    print(f"PROGRESS_AUDIO:{i+1}/{n_ips}", flush=True)

            print(f"STATUS:Análisis de audio completado: {processed} OK, {errors} errores", flush=True)

        except ImportError as e:
            print(f"WARNING:No se pudo cargar emotion2vec: {e}", file=sys.stderr)
            print("WARNING:Instalar con: pip install funasr soundfile", file=sys.stderr)

    elif not args.no_audio:
        print("STATUS:No se proporcionó directorio de WAVs. Omitiendo análisis de audio.", flush=True)

    # =============================================
    # 4. Guardar resultados
    # =============================================
    print("STATUS:Guardando resultados...", flush=True)
    ips.to_csv(args.output_file, sep="\t", index=False)

    # Resumen JSON para R
    resumen = {
        "n_ips": int(n_ips),
        "sentimiento_completado": bool((ips["sentimiento"] != "").any()),
        "emocion_texto_completado": bool((ips["emocion_texto"] != "").any()),
        "emocion_audio_completado": bool((ips["emocion_audio"] != "").any()),
        "distribucion_sentimiento": ips["sentimiento"].value_counts().to_dict() if (ips["sentimiento"] != "").any() else {},
        "distribucion_emocion_texto": ips["emocion_texto"].value_counts().to_dict() if (ips["emocion_texto"] != "").any() else {},
        "distribucion_emocion_audio": ips["emocion_audio"].value_counts().to_dict() if (ips["emocion_audio"] != "").any() else {},
    }
    print(f"RESULT_JSON:{json.dumps(resumen, ensure_ascii=False)}", flush=True)
    print("STATUS:Completado!", flush=True)


if __name__ == "__main__":
    main()
