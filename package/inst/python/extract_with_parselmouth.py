#!/usr/bin/env python3
"""
extract_with_parselmouth.py
Extrae datos de texto (tiers), pitch e intensidad desde pares TextGrid+WAV
usando parselmouth, generando los mismos formatos tabulares que espera la app Shiny.

Uso:
  python3 extract_with_parselmouth.py \
    --input_dir /tmp/shiny_uploads \
    --output_dir /tmp/shiny_extracted \
    --pitch_floor 75 --pitch_ceiling 500 \
    --time_step_pitch 0.01 --time_step_intensity 0.01

Archivos de salida (en output_dir):
  - text_combined.txt   (tier | tmin | tmax | text | archivo_origen)
  - pitch_combined.txt  (tiempo | f0 | archivo_origen)
  - intensity_combined.txt (tiempo | intensidad | archivo_origen)
"""

import argparse
import os
import sys
import glob
import csv

try:
    import parselmouth
    from parselmouth.praat import call
except ImportError:
    print("ERROR: parselmouth no está instalado. Instalar con: pip install praat-parselmouth", file=sys.stderr)
    sys.exit(1)

def parse_textgrid(tg):
    """Extrae todos los intervalos de todos los tiers de un TextGrid."""
    rows = []
    n_tiers = call(tg, "Get number of tiers")
    for itier in range(1, n_tiers + 1):
        tier_name = call(tg, "Get tier name", itier)
        is_interval = call(tg, "Is interval tier", itier)
        if is_interval:
            n_intervals = call(tg, "Get number of intervals", itier)
            for j in range(1, n_intervals + 1):
                label = call(tg, "Get label of interval", itier, j)
                tmin = call(tg, "Get start time of interval", itier, j)
                tmax = call(tg, "Get end time of interval", itier, j)
                rows.append({
                    "tier": tier_name,
                    "tmin": tmin,
                    "tmax": tmax,
                    "text": label
                })
        else:
            # Point tier - convertir a pseudo-intervalos si es necesario
            n_points = call(tg, "Get number of points", itier)
            for j in range(1, n_points + 1):
                t = call(tg, "Get time of point", itier, j)
                label = call(tg, "Get label of point", itier, j)
                rows.append({
                    "tier": tier_name,
                    "tmin": t,
                    "tmax": t,
                    "text": label
                })
    return rows


def extract_pitch_track(sound, time_step, pitch_floor, pitch_ceiling,
                        voicing_threshold=0.15, silence_threshold=0.01):
    """Extrae la pista de pitch como serie temporal (tiempo, f0).

    Usa To Pitch (ac) con los mismos parámetros que analisis_prosodico_v3_1_6.praat:
      voicing_threshold = 0.15  (Praat por defecto: 0.45 — más restrictivo)
      silence_threshold = 0.01  (Praat por defecto: 0.03 — más restrictivo)
    Valores más bajos detectan más regiones como sonoras, reduciendo huecos en F0.
    """
    pitch_obj = call(
        sound, "To Pitch (ac)",
        time_step,          # time step (s)
        pitch_floor,        # pitch floor (Hz)
        15,                 # max number of candidates
        False,              # very accurate
        silence_threshold,  # silence threshold
        voicing_threshold,  # voicing threshold
        0.01,               # octave cost
        0.35,               # octave jump cost
        0.14,               # voiced/unvoiced cost
        pitch_ceiling       # pitch ceiling (Hz)
    )

    rows = []
    n_frames = call(pitch_obj, "Get number of frames")

    for i in range(1, n_frames + 1):
        t = call(pitch_obj, "Get time from frame number", i)
        f0 = call(pitch_obj, "Get value in frame", i, "Hertz")
        # f0 será undefined (nan) si no hay voicing; filtramos esos
        if f0 is not None and f0 == f0 and f0 > 0:  # f0 == f0 descarta NaN
            rows.append({"tiempo": t, "f0": f0})

    return rows


def extract_intensity_track(sound, time_step, pitch_floor):
    """Extrae la pista de intensidad como serie temporal (tiempo, intensidad)."""
    intensity_obj = call(sound, "To Intensity", pitch_floor, time_step, "yes")
    
    rows = []
    n_frames = call(intensity_obj, "Get number of frames")
    
    for i in range(1, n_frames + 1):
        t = call(intensity_obj, "Get time from frame number", i)
        val = call(intensity_obj, "Get value in frame", i)
        if val is not None and val == val:  # descartar NaN
            rows.append({"tiempo": t, "intensidad": val})
    
    return rows


def find_audio_for_textgrid(tg_path, input_dir):
    """Busca el archivo de audio correspondiente a un TextGrid."""
    basename = os.path.splitext(os.path.basename(tg_path))[0]
    for ext in [".wav", ".WAV", ".flac", ".mp3", ".ogg"]:
        audio_path = os.path.join(input_dir, basename + ext)
        if os.path.isfile(audio_path):
            return audio_path
    return None


def main():
    parser = argparse.ArgumentParser(description="Extraer datos acústicos con parselmouth")
    parser.add_argument("--input_dir", required=True, help="Carpeta con TextGrid y audios")
    parser.add_argument("--output_dir", required=True, help="Carpeta de salida")
    parser.add_argument("--pitch_floor", type=float, default=75.0)
    parser.add_argument("--pitch_ceiling", type=float, default=500.0)
    parser.add_argument("--time_step_pitch", type=float, default=0.01)
    parser.add_argument("--time_step_intensity", type=float, default=0.01)
    parser.add_argument("--voicing_threshold", type=float, default=0.15,
                        help="Voicing threshold para To Pitch (ac). "
                             "Praat por defecto: 0.45. El script Praat original usa 0.15.")
    parser.add_argument("--silence_threshold", type=float, default=0.01,
                        help="Silence threshold para To Pitch (ac). "
                             "Praat por defecto: 0.03. El script Praat original usa 0.01.")
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)

    # Buscar TextGrids
    tg_files = sorted(glob.glob(os.path.join(args.input_dir, "*.TextGrid")))
    if not tg_files:
        print("ERROR: No se encontraron archivos .TextGrid en " + args.input_dir, file=sys.stderr)
        sys.exit(1)

    all_text = []
    all_pitch = []
    all_intensity = []
    
    n_files = len(tg_files)
    processed = 0
    skipped = 0

    for tg_path in tg_files:
        basename = os.path.splitext(os.path.basename(tg_path))[0]
        audio_path = find_audio_for_textgrid(tg_path, args.input_dir)
        
        if audio_path is None:
            print(f"WARN: Sin audio para {basename} - omitido", file=sys.stderr)
            skipped += 1
            continue
        
        print(f"Procesando [{processed+1}/{n_files}]: {basename}", file=sys.stderr)
        
        try:
            # Cargar archivos
            sound = parselmouth.Sound(audio_path)
            tg = call("Read from file", tg_path)
            
            # Extraer texto (tiers)
            text_rows = parse_textgrid(tg)
            for row in text_rows:
                row["archivo_origen"] = basename
            all_text.extend(text_rows)
            
            # Extraer pitch
            pitch_rows = extract_pitch_track(
                sound, args.time_step_pitch, args.pitch_floor, args.pitch_ceiling,
                voicing_threshold=args.voicing_threshold,
                silence_threshold=args.silence_threshold
            )
            for row in pitch_rows:
                row["archivo_origen"] = basename
            all_pitch.extend(pitch_rows)
            
            # Extraer intensidad
            int_rows = extract_intensity_track(
                sound, args.time_step_intensity, args.pitch_floor
            )
            for row in int_rows:
                row["archivo_origen"] = basename
            all_intensity.extend(int_rows)
            
            processed += 1
            
        except Exception as e:
            print(f"ERROR procesando {basename}: {e}", file=sys.stderr)
            skipped += 1
            continue

    # Escribir archivos de salida
    text_out = os.path.join(args.output_dir, "text_combined.txt")
    with open(text_out, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["tier", "tmin", "tmax", "text", "archivo_origen"],
                                 delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows(all_text)
    
    pitch_out = os.path.join(args.output_dir, "pitch_combined.txt")
    with open(pitch_out, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["tiempo", "f0", "archivo_origen"],
                                 delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows(all_pitch)
    
    int_out = os.path.join(args.output_dir, "intensity_combined.txt")
    with open(int_out, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["tiempo", "intensidad", "archivo_origen"],
                                 delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows(all_intensity)
    
    # Resumen
    summary = {
        "archivos_procesados": processed,
        "archivos_omitidos": skipped,
        "total_intervalos_texto": len(all_text),
        "total_frames_pitch": len(all_pitch),
        "total_frames_intensidad": len(all_intensity)
    }
    
    # Imprimir resumen como JSON por stdout para que R lo lea
    import json
    print(json.dumps(summary))


if __name__ == "__main__":
    main()
