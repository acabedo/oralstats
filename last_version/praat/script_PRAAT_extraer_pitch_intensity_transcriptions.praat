# Script Integral con Selección de Procesos y Parámetros Avanzados
# Autor: Gemini AI

form Parámetros de Análisis y Selección
    comment ¿Qué datos deseas procesar?
    boolean Procesar_Pitch 1
    boolean Procesar_Intensidad 1
    boolean Procesar_Texto_(TextGrid) 1
    
    comment --- Configuración de Pitch ---
    positive Paso_de_tiempo_Pitch_(s) 0.01
    positive Pitch_minimo_(Hz) 75
    positive Pitch_maximo_(Hz) 500
    real Silence_threshold 0.03
    real Voicing_threshold 0.45
    
    comment --- Configuración de Intensidad ---
    positive Paso_de_tiempo_Int_(s) 0.01
    positive Pitch_minimo_para_intensidad_(Hz) 100
endform

# 1. Selección de directorios
dirInput$ = chooseDirectory$: "Selecciona la carpeta con tus archivos (.wav y .TextGrid)"
dirOutput$ = chooseDirectory$: "Selecciona la carpeta de DESTINO"

if dirInput$ == "" or dirOutput$ == ""
    exitScript: "Operación cancelada."
endif

# 2. Listar archivos de audio
list = Create Strings as file list: "list", dirInput$ + "/*.wav"
nFiles = Get number of strings

# 3. Bucle principal
for i from 1 to nFiles
    selectObject: list
    fileName$ = Get string: i
    baseName$ = fileName$ - ".wav"
    
    if procesar_Pitch or procesar_Intensidad
        sound = Read from file: dirInput$ + "/" + fileName$
    endif
    
    # --- PROCESO: PITCH (Usando Autocorrelación avanzada) ---
    if procesar_Pitch
        selectObject: sound
        # To Pitch (ac): time step, min pitch, candidates, silence thresh, voicing thresh, etc.
        pitch = To Pitch (ac): paso_de_tiempo_Pitch, pitch_minimo, 15, "no", silence_threshold, voicing_threshold, 0.01, 0.35, 0.14, pitch_maximo
        nFramesP = Get number of frames
        outPitch$ = dirOutput$ + "/" + baseName$ + "_pitch.txt"
        writeFileLine: outPitch$, "Tiempo(s)", tab$, "F0(Hz)"
        
        for p from 1 to nFramesP
            valF0 = Get value in frame: p, "Hertz"
            if valF0 <> undefined
                timeP = Get time from frame: p
                appendFileLine: outPitch$, fixed$(timeP, 4), tab$, fixed$(valF0, 2)
            endif
        endfor
        selectObject: pitch
        Remove
    endif
    
    # --- PROCESO: INTENSIDAD ---
    if procesar_Intensidad
        selectObject: sound
        intensity = To Intensity: pitch_minimo_para_intensidad, paso_de_tiempo_Int, "yes"
        nFramesI = Get number of frames
        outInt$ = dirOutput$ + "/" + baseName$ + "_intensidad.txt"
        writeFileLine: outInt$, "Tiempo(s)", tab$, "Intensidad(dB)"
        
        for k from 1 to nFramesI
            valInt = Get value in frame: k
            timeI = Get time from frame: k
            if valInt <> undefined
                appendFileLine: outInt$, fixed$(timeI, 4), tab$, fixed$(valInt, 2)
            endif
        endfor
        selectObject: intensity
        Remove
    endif
    
    # --- PROCESO: TEXTGRID ---
    if procesar_Texto
        tgFile$ = dirInput$ + "/" + baseName$ + ".TextGrid"
        if fileReadable(tgFile$)
            tg = Read from file: tgFile$
            nTiers = Get number of tiers
            outTG$ = dirOutput$ + "/" + baseName$ + "_texto.txt"
            writeFileLine: outTG$, "tier", tab$, "tmin", tab$, "tmax", tab$, "text"
            
            for t from 1 to nTiers
                tierName$ = Get tier name: t
                isInterval = Is interval tier: t
                if isInterval
                    nInt = Get number of intervals: t
                    for j from 1 to nInt
                        label$ = Get label of interval: t, j
                        if label$ <> ""
                            tmin = Get start time of interval: t, j
                            tmax = Get end time of interval: t, j
                            appendFileLine: outTG$, tierName$, tab$, fixed$(tmin, 4), tab$, fixed$(tmax, 4), tab$, label$
                        endif
                    endfor
                else
                    nPoints = Get number of points: t
                    for j from 1 to nPoints
                        label$ = Get label of point: t, j
                        if label$ <> ""
                            tpoint = Get time of point: t, j
                            appendFileLine: outTG$, tierName$, tab$, fixed$(tpoint, 4), tab$, "--", tab$, label$
                        endif
                    endfor
                endif
            endfor
            selectObject: tg
            Remove
        endif
    endif

    if procesar_Pitch or procesar_Intensidad
        selectObject: sound
        Remove
    endif
endfor

selectObject: list
Remove
appendInfoLine: "Proceso finalizado. Parámetros avanzados aplicados."