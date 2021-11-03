# ANNOTATION PATH 

# Expected: all types in the same data frame (they came from the same file). They will be split up on the next part.

# 1. Load libraries and dependencies ----------------------------------------------------------

library(tidyverse)
library(sqldf)
library(udpipe)
library(xfun)
library(readbulk)
library(readxl)
udmodel_spanish <-
  udpipe_load_model(file = "~/Library/Mobile Documents/com~apple~CloudDocs/proyectos/oralstats/core/spanish-gsd-ud-2.4-190531.udpipe")

# 2. Create working folder directory.  -----------------------------------------
# Note = Uncomment the folder you need. 
# mydir = "~/Library/Mobile Documents/com~apple~CloudDocs/proyectos/oralstats/core/test_files/standard_path"
mydir = "~/Library/Mobile Documents/com~apple~CloudDocs/proyectos/oralstats/mods/padrezorro"
setwd = mydir
# export_elan <- read_delim(paste(mydir,"/export_elan.txt",sep=""), "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# 3. Import all pitch and intensity files into two data frames, one for pitch and the other for intensity --------------------------------------
# Note: pitch and intensity files must be on 0.001 time frames. If data is collected by 0.0001 time frames, it will be needed to adjust Time variable on steps below. 
pitch <- read_bulk(directory = mydir, extension = "_pitch.txt", fun = read.delim, header=FALSE)
intensity <- read_bulk(directory = mydir, extension = "_intensity.txt", fun = read.delim, header=TRUE)

# Uncomment only if header is false

pitch <- pitch %>% rename(
  Time = V1,
  Pitch = V2,
  filename = File
)
# pitch$filename <- pitch$File
# pitch$filename <- "orst"
pitch$filename <- gsub(".*\\/","", pitch$filename)
pitch$filename <- gsub("_pitch\\.txt","", pitch$filename)
# pitch$Time <- pitch$V1
# pitch$Pitch <- pitch$V2
# pitch$Time <-round(pitch$Time,2)
# pitch$Pitch <-round(pitch$Pitch,2)
# pitch$V1<- NULL
# pitch$V2<- NULL
# pitch$File<- NULL
pitch$Time <- format(round(pitch$Time,3),nsmall=3)
pitch <- pitch %>% distinct(filename,Time, .keep_all = TRUE)
pitch <- pitch%>%mutate(Time_ms = as.numeric(pitch$Time))%>%mutate(Time_ms = Time_ms*1000)

# Uncomment if filename comes from the folder files.

intensity <- intensity %>% rename(
  Time = Time..s.,
  Intensity = Intensity..dB.,
  filename = File
)
intensity$rowLabel <- NULL

# intensity$filename <- intensity$File
intensity$filename <- gsub(".*\\/","", intensity$filename)
intensity$filename <- gsub("_intensity\\.txt","", intensity$filename)
# intensity$filename <- "orst"
# intensity$Time <- intensity$Time..s.
# intensity$intensity <- intensity$Intensity..dB.

# intensity <- subset(intensity, V2 != "Time (s)")
# options(digits = 5)
# intensity$V2 <- as.character(intensity$V2)
# intensity$V2 <- as.numeric(intensity$V2)
# intensity$V3 <- as.character(intensity$V3)
# intensity$V3 <- as.numeric(intensity$V3) 
# intensity$V1 <- intensity$V2
# intensity$V2 <- intensity$V3
# intensity$filename <- intensity$File
# intensity$filename <- gsub(".*\\/","", intensity$filename)
# intensity$filename <- gsub("intensity\\.txt","", intensity$filename)
# intensity$Time <- intensity$V1
# intensity$intensity <- intensity$V2
# intensity$Time <-round(intensity$Time,2)
# intensity$intensity <-round(intensity$intensity,2)
# intensity$V1<- NULL
# intensity$V2<- NULL
# intensity$V3<- NULL
# intensity$File<- NULL
intensity$Time <- format(round(intensity$Time,3),nsmall=3)
intensity <- intensity %>% distinct(filename,Time, .keep_all = TRUE)
intensity <- intensity%>%mutate(Time_ms = as.numeric(intensity$Time))%>%mutate(Time_ms = Time_ms*1000)

# 4. Import exported and tabbed ELAN files into data frame.-------------------------------------- 
# Notes: Path required. By default it is expected from Excel, but it can be modified and uncomment the txt import.
# Specially we must take care of filename, because the code belowe is thought to work with a unique file calle "test"; 
# if your data have different files, you should strip the part ending with  "mutate(filename = "test")"; 
# in any case, you must be sure that you keep a column called filename with the specific filenames inside.

orst <- read_delim(paste(mydir,"/padre.txt",sep=""),
                     "\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
orst <- orst %>% rename(
  annotation = text
  # spk = X2,
  # tmin = X2,
  # tmax = X3,
  # dur = X5,
  # annotation = X9
)
orst$dur <- orst$tmax - orst$tmin
orst <- orst %>% mutate(spk = ifelse(grepl("fox",tier),"fox","amorosi"), corpus = "corpus")
# %>% select(-X3,-X5,-X7) %>% mutate(filename = "test", corpus ="corpus")
# orst <- subset(orst,X3<66000)

# orst <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Publicaciones/Estudios_Fonetica_2021/files/txts/combined.xlsx",
# sheet = "Hoja1")

# 5. Create units data frames and ids ------------------------------------------------
# Note: this part must be customized conveniently because units can only filtered with a specific substring reference (word, for example, for words tier)

orstip <- subset(orst, !grepl("\\/phon$",tier)&!grepl("word$",tier))
orstip <- orstip%>%group_by(filename)%>%mutate(ip_id= paste("ip_",filename,"_",dplyr::row_number(),sep=""))
orstalof <- subset(orst, grepl("\\/phon",tier))
orstalof <- orstalof%>%group_by(filename)%>%mutate(ph_id= paste("alof_",filename,"_",dplyr::row_number(),sep=""))
orstwords <- subset(orst, grepl("word$",tier))
orstwords <- orstwords%>%group_by(filename)%>%mutate(w_id= paste("word_",filename,"_",dplyr::row_number(),sep=""))%>%ungroup()

# 6. Create prosody data frame --------------------------------------
# Note: the most important here is to reassure that the variable Time and filename are well set on pitch and intensity dataframes
prosodydb <- pitch %>% dplyr::left_join(intensity%>%select(-Time), by = c("filename","Time_ms"))
prosodydb$st <- 12*log2(prosodydb$Pitch)/1

# Specific to Padre Zorro exploration

prosodydb <- prosodydb%>%filter(Time_ms < 720000)
# prosodydb2 <- prosodydb
# prosodydb2$Time <- as.numeric(as.character(prosodydb2$Time))
# prosodydb2$Time <- format(round(prosodydb2$Time,2),nsmall=2)
# prosodydb2 <- prosodydb2 %>% distinct(filename,Time, .keep_all = TRUE)
# prosodydb2$Time <- as.numeric(as.character(prosodydb2$Time))
# prosodydb2$Time_ms <- prosodydb2$Time*1000

# prosodydb$file <- "orst"
# prosodydb$filename <- "orst"

# 7. Save prosody data on a sqlite database ------------------
# Note: as we are talking of several thousands or even milions of data in some cases, we suggest to put them into this SQLite file

library(DBI)
mydb <- dbConnect(RSQLite::SQLite(), "prosody.sqlite")
dbWriteTable(mydb, "prosody", prosodydb, overwrite=TRUE)

# 8. Rename variables on ELAN exported file --------------------------------------------------------
# Note: this is not compulsory, but it must be set accordingly with researcher needs. The most important is to know how are named the speaker, text and filename variables.
orstalof$tmin_ms <- orstalof$tmin*1000
orstalof$tmax_ms <- orstalof$tmax*1000
# orstalofrn <- orstalof %>% rename(
#   spk = speaker,
#   annotation = text,
#   filename = filename
# )

# Customize tier, speaker columns. It will depend on naming; for example, at Ameresco Corpus speaker tiers are called A_phon, B_phon, and so on.

# orstalofrn$file <- gsub(".eaf","",orstalofrn$file)
# orstalofrn <- orstalofrn %>% mutate(id = paste0("segmentacion_", row_number()))
# orstalofrn$spk <- gsub("_phon/phon","",orstalofrn$tier)

# 9. Combine the smallest unit with prosody data frame --------------------------
# Note: if your speech data is transcribed in intonational phrases, words and phonemes, these last ones are the one you must use here.

# orst_ <-  orstalofrn %>% group_by(ph_id) %>% mutate(Time_ms = list(tmin*1000:tmax*1000)) %>% ungroup() %>% unnest(Time_ms)
# prosodydb$Time_ms <- as.numeric(prosodydb$Time)*1000
# prosodydb$file <- prosodydb$filename

# orstphon <-
#   left_join(orst_, prosodydb, by = c("filename", "Time_ms"))

alofwithpros<- sqldf("select x.*,y.Pitch, y.Intensity, y.Time_ms from orstalof x 
left join prosodydb as y on (y.Time_ms between x.tmin_ms and x.tmax_ms)
and (x.filename = y.filename)")
alofwithpros <- alofwithpros%>%arrange(filename,Time_ms)

# 9.1. Summarise phoneme data frame values --------------------------

orstalofrn <-
  alofwithpros%>% 
  group_by(ph_id) %>% summarise(
    tier = max(tier),
    tmin = max(tmin),
    tmax = max(tmax),
    spk = max(spk),
    dur = max(dur),
    phon = max(annotation),
    filename = max(filename),
    firstHz = round(first(Pitch),2),
    midHz = round(median(Pitch[Time_ms>(max(tmin)+(dur/2)+5) |Time_ms<(max(tmax)-(dur/2)+5)], na.rm = TRUE),2),
    lastHz = round(last(Pitch),2),
    firstSt = round(12*log2(first(Pitch)/1),2),
    midSt = round(12*log2(median(Pitch[Time_ms>(max(tmin)+(dur/2)+5) |Time_ms<(max(tmax)-(dur/2)+5)], na.rm = TRUE)/1),2),
    lastSt = round(12*log2(last(Pitch)/1),2),
    PirHz = round(max(Pitch,na.rm=TRUE) - min(Pitch, na.rm = TRUE),2),
    PirSt = round(12*log2(max(Pitch,na.rm=TRUE)/min(Pitch, na.rm = TRUE)),2),
    Pimd = round(median(Pitch, na.rm = TRUE),2),
    PimdSt = round(12*log2(median(Pitch,na.rm = TRUE)/1),2),
    PimnHz = round(mean(Pitch, na.rm = TRUE),2),
    PimnSt = round(12*log2(mean(Pitch,na.rm = TRUE)/1),2),
    Imd = round(median(Intensity, na.rm = TRUE),2),
    Imn = round(mean(Intensity, na.rm = TRUE),2), 
    
  ) %>%ungroup()%>% arrange(filename,tmin) %>%   
  mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% 
  group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), 
  trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
# col_means <- lapply(orstalofrn%>%select(-transition_post, -transition_prev), mean, na.rm = TRUE)
# col_means <- lapply(col_means, round,2)
# orstalofrn <- replace_na(orstalofrn, col_means)
orstalofrn <-
  orstalofrn %>% mutate(
    RdprSt = PirSt - lag(PirSt),
    Idpr = Imn - lag(Imn, 1),
    spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
    spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
    ppr = ifelse(spkpr =="mismo",trpr,NA),
    ppst = ifelse(spkpst =="mismo",trpst,NA),
    ftopr = ifelse(spkpr =="otro",trpr,NA),
    ftopst = ifelse(spkpst =="otro",trpst,NA)
  )

orstalofrn <-
  orstalofrn %>% group_by(filename, spk) %>% mutate(
    # Rng_mean_sp = round(mean(Pitch_range_St,na.rm = TRUE), 2),
    Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
    Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
    # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
    # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
    PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
    PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
    # Pdspkp = round(Pit_diff_sp_Hz / mean(Pitch_mean_Hz,na.rm = TRUE), 2) * 100
    # Pitch_mean_sp = round(mean(Pitch_mean_Hz,na.rm = TRUE), 2)
  ) %>% ungroup()

orstalofrn <-
  orstalofrn %>% group_by(filename)%>%
  mutate(phon_type = ifelse(grepl("[aeiou]",phon),"vowel",
                            ifelse(grepl("[jw]",phon),"glide","consonant")),
         tonic = ifelse(grepl("\\*",phon),"yes","no"))

orstalofrn <-
  orstalofrn %>% group_by(filename, phon_type )%>% mutate(
    Pdprvow = PimnSt - lag(PimnSt),
    Idprvow = Imn - lag(Imn, 1),
    Pdprvow = ifelse(is.na(Pdprvow),0,Pdprvow),
    Idprvow = ifelse(is.na(Idprvow),0,Idprvow),
  )%>%ungroup()

# Include prosodic notation on most little data frame. For example, phonemes are the little units on a relationship among intonation phrases and words. 

# Note: you need operation section XXX from standard path. If you have phonemes, probably you will want to compute the difference among vowels. Most common operations include 
# differences between contiguous vowels, but also between first and last vowel to check global rising or falling pitch on the complete intonational phrase. 
# And even between the last vowel and the first vowel of the next intonational phrase. All of this can even be developed a little more if we include a notation from tonic syllable.

# 9.2. Create relationships. --------
# Note: This part of the script can be customized in order to get the relations between units. Basic notions on SQL are needed. 

orstalof2 <- sqldf(
"select x.*,
y.annotation as word,y.dur as w_dur, y.tmin as w_tmin,y.tmax as w_tmax, 
y.w_id as w_id from orstalofrn x left join orstwords as 
y on (x.tmin*1000 >= (y.tmin*1000 -5)) and (x.tmax*1000 <= (y.tmax*1000+5)) 
and (x.spk = y.spk) and (x.filename = y.filename)")

orstalof3 <- sqldf(
"select x.*,
y.annotation as ip, y.dur as ip_dur,y.tmin as ip_tmin,y.tmax as ip_tmax, 
y.ip_id as ip_id from orstalof2 x left join orstip as 
y on (x.tmin*1000 >= (y.tmin*1000 -5)) and (x.tmax*1000 <= (y.tmax*1000+5))
and (x.spk = y.spk) and (x.filename = y.filename)")

# Calculate correspondence between conversational speech units and phonic units ---

orstcomp <- orstalof3
orstcomp <- orstcomp %>% group_by(filename,ip_id, phon_type)%>% mutate(
  declination = last(PimnSt)-first(PimnSt),
  PidprSt = PimnSt - lag(PimnSt)
)%>%ungroup()
orstcomp <- orstcomp %>%
  group_by(filename,w_id)%>%mutate(orderinword = row_number())%>% 
  group_by(filename, ip_id)%>%mutate(orderinip = row_number()) 
orstcomp <- orstcomp %>% mutate(corresp_w_ip = ip_dur - w_dur)


# 10. Vowels data frame -------------------------------------------------------

orstvowels <- orstcomp %>% ungroup()%>% filter(phon_type=="vowel")%>% select(filename,ph_id,tmin,tmax,ip_id,ip,ip,w_id,phon_type,phon,firstSt,midSt,midHz,lastSt,PimnSt,Imn,dur,tonic)
orstvowels <- orstvowels %>% group_by(filename,ip_id,phon_type)%>% mutate(ord = row_number())%>%ungroup()%>% mutate(aguda = ifelse(ord==1 &lead(ord,1)==1,"unique",
                                                                                                                              ifelse(lead(ord,1)==1 & tonic!="yes","noaguda",
                                                                                                                                       ifelse(lead(ord,1)==1 & tonic=="yes","aguda",                                                                                                                                            ifelse(ord ==1 |ord==2 &lead(ord,1)!=1,"prebody","body")))))
orstvowelslast <- orstvowels %>%select(filename,ip_id,ph_id,phon_type) %>%
  group_by(filename,ip_id,phon_type)%>% 
  mutate(first=first(ph_id),last=last(ph_id))%>%ungroup()%>%
  group_by(last)%>%summarise(ip_id = max(ip_id))
orstvowelstoneme <- orstvowels %>%select(filename,ip_id,ph_id,phon_type,tonic)%>%
  filter(tonic=="yes") %>%group_by(filename,ip_id,phon_type)%>% 
  mutate(toneme=last(ph_id))%>%ungroup()%>%group_by(toneme)%>%summarise(ip_id = max(ip_id))
orstiprange <- orstcomp %>% group_by(ip_id)%>% summarise(rangeSt = quantile(PimnSt,na.rm = TRUE, .75))

orstvowels <- orstvowels %>% left_join(orstiprange,by="ip_id")
orstvowels <- orstvowels %>% left_join(orstvowelslast,by="ip_id")
orstvowels <- orstvowels %>% left_join(orstvowelstoneme,by="ip_id")
orstvowels <- orstvowels %>% mutate(last= ifelse(ph_id==last,"yes","no"))
orstvowels <- orstvowels %>% mutate(toneme= ifelse(ph_id==toneme,"yes","no"))
orstvowels <- orstvowels %>% group_by(ip_id)%>%arrange(tmin) %>%mutate(
  
  
  rangedif = midSt-rangeSt,
  ipdecl = last(midSt)-first(midSt),
  declinationpre = midSt - lag(midSt,1),
  voweldecl = lastSt-firstSt,
  stressed = ifelse(midSt-firstSt>1.5 & between(lastSt-midSt,-6,-1.5) < -1.5,"(LHL)*",
                    ifelse(between(midSt-firstSt,-6,-1.5) & lastSt-midSt >1.5,"(HLH)*",
                           ifelse(lastSt-firstSt> 1.5&between(lag(midSt)-midSt,-10,-1.5),"L+H*",
                                  
                                         ifelse(midSt-lag(midSt)>1.5 & lead(midSt)-midSt<1.5,"L+H*",
                                                ifelse(midSt-lag(midSt)<1.5 &lead(midSt)-midSt>1.5,"L*+H",
                                                       ifelse(lag(voweldecl)>1.5&voweldecl>1.5 & lead(voweldecl)>1.5,"L+>H*",
                                                              ifelse(voweldecl<1.4 &between(declinationpre,-15,0),"H+L*",NA)
                                                       )))))),
  declinationpost=  lead(midSt,1)-midSt,
  TOBI = ifelse(midSt-lag(midSt)<1.5 & lead(midSt)-midSt<1.5&voweldecl>1.5,"L+H*",
                ifelse(midSt-lag(midSt)>1.5&lead(midSt) - lag(midSt)>1.5 ,"L+>H*",
                ifelse(midSt-lag(midSt)<1.5 &lead(midSt)-midSt>1.5,"L*+H",
                       ifelse(rangedif > 6, "L+¡H*",ifelse(rangedif > 1.5&rangedif < 6, "L+H*","L*")
                       )
                ))),
  TOBI = ifelse(!is.na(TOBI),TOBI, ifelse(rangedif > 1.5, "H*","L*")),
stressed = ifelse(!is.na(stressed),stressed, ifelse(rangedif > 1.5, "H*", ifelse(between(rangedif,-15,0),"L*","S"))),
stressed = ifelse(last=="yes" &aguda !="aguda",gsub("*","",stressed),stressed),  
bound = ifelse(toneme=="yes" &aguda !="aguda",paste(lead(TOBI),"%",sep=""),
               ifelse(toneme=="yes"&aguda=="aguda",paste(TOBI,"%",sep=""),"")),
AMH = ((midHz - lag(midHz))/lag(midHz))*100,
AMH2= cumsum(replace_na(AMH, 0)),
AMH3= 100+AMH2)

orstvowels <- orstvowels %>%group_by(filename)%>% mutate (
  
  vowelresetpitchip = ifelse(lead(ip_id) != ip_id, lead(midSt)-midSt,NA),
  vowelresetpausip = ifelse(lead(ip_id) != ip_id, lead(tmin)-tmax,NA),
  
  vowelresetw = ifelse(lead(w_id) != w_id, lead(midSt)-midSt,NA),
  vowelresetpausw = ifelse(lead(w_id) != w_id, lead(tmin)-tmax,NA)
  
  
)

  # TOBIpre = ifelse(declinationpre < -1.5 & tonic=="yes", "L",
  #                  ifelse(declinationpre>1.5& tonic=="yes","H",NA)),
  # TOBIpost = ifelse(declinationpost < -1.5& tonic=="yes", "H",
  #                  ifelse(declinationpost>1.5& tonic=="yes","L",NA)),
  # TOBIdespl = ifelse(voweldecl>1.5 & declinationpost>1.5 &tonic =="yes","L+>H*",NA),
  # TOBIdecl = ifelse(voweldecl>1.5, "H*",
  #                   ifelse(voweldecl<-1.5,"L*",NA)),
  # BOUNDARY = ifelse(aguda=="noaguda",(paste(TOBIpre,TOBIdecl,TOBIpost,"%",sep = "+")),
  #                   ifelse(aguda=="aguda",ifelse(midSt-lastSt>1.5,"H%","L%"),NA)),
  # TOBI = ifelse(is.na(voweldecl),NA,
  #               ifelse(declinationpre<1.5 &declinationpre>-1.5 & declinationpost<1.5& declinationpost>-1.5,
  #                      TOBIdecl,
  #               (paste(TOBIpre,TOBIdecl,TOBIpost,BOUNDARY,sep = "+")))),
  # TOBI = gsub("NA\\+|\\+NA","",TOBI),
  # TOBI = ifelse(voweldecl>1.5 & declinationpost>1.5 &tonic =="yes",TOBIdespl,TOBI)
  # TOBI = ifelse((lastSt - firstSt) > 1.5 & (lead(lastSt,1)- lastSt) < 1.5, "L+H*",
  #               ifelse((lead(lastSt,1) - lag(firstSt,1))>1.5,"L*+H",
  #               ifelse(lastSt - firstSt > 1.5 &(lead(lastSt,1) - lag(firstSt,1))>1.5, "L+>H*",
  #                      ifelse(midSt >rangeSt,"H*","L*")))),
  
  # TOBI = ifelse(tonic=="yes",paste(TOBI,"*",sep=""),TOBI)
  
# )
orstvowelstonic <- orstvowels%>% filter(tonic == "yes")
orstvowels <- orstvowels %>% mutate(tobilegend = ifelse(tonic == "yes", TOBI,""))

# 11. Words developed data frame-------------------------------------------------------

orstwords$tmin_ms <- orstwords$tmin*1000
orstwords$tmax_ms <- orstwords$tmax*1000
# orstwordsrn <- orstwords %>% rename(
#   spk = speaker,
#   annotation = text,
#   filename = filename
# )

# orstwordsrn <- orstwords %>% rename(
#   tier = X1,
#   spk = X2,
#   tmin = X3,
#   tmax = X4,
#   duration = X5,
#   annotation = X6
# )

# orst_w <-
#   orstwordsrn %>% group_by(w_id) %>% mutate(Time_ms = list(tmin:tmax)) %>% ungroup() %>% unnest(Time_ms)


# Combine words data frame with prosody

word_declin <- orstcomp%>%ungroup()%>% select(-tmin,-tmax,-dur)%>% filter(phon_type=="vowel") %>%group_by(filename,w_id,phon_type)%>% summarise(declination = last(PimnSt)-first(PimnSt))%>%ungroup()


prueba<- sqldf(
  "select x.*,y.Pitch, y.Intensity, y.Time_ms from orstwords x left join prosodydb as y on (y.Time_ms >= (x.tmin_ms)) and (y.Time_ms <= (x.tmax_ms)) and (x.filename = y.filename)")

orstwordsrn <-prueba%>% 
  group_by(w_id) %>% summarise(
    tier = max(tier),
    spk = max(spk),
    tmin = max(tmin),
    tmax = max(tmax),
    dur = max(dur),
    corpus = max(corpus),
    token = max(annotation),
    file = max(filename),
    PirHz = round(max(Pitch,na.rm=TRUE) - min(Pitch, na.rm = TRUE),2),
    PirSt = round(12*log2(max(Pitch,na.rm=TRUE)/min(Pitch, na.rm = TRUE)),2),
    Pimd = round(median(Pitch, na.rm = TRUE),2),
    PimnHz = round(mean(Pitch, na.rm = TRUE),2),
    PimnSt = round(12*log2(mean(Pitch,na.rm = TRUE)/1),2),
    Imd = round(median(Intensity, na.rm = TRUE),2),
    Imn = round(mean(Intensity, na.rm = TRUE),2),
    
  ) %>%ungroup()%>% arrange(file,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(file)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
# col_means <- lapply(orstalofrn%>%select(-transition_post, -transition_prev), mean, na.rm = TRUE)
# col_means <- lapply(col_means, round,2)
# orstalofrn <- replace_na(orstalofrn, col_means)
orstwordsrn <-
  orstwordsrn %>% mutate(
    RdprSt = PirSt - lag(PirSt),
    Idpr = Imn - lag(Imn, 1),
    spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
    spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
    ppr = ifelse(spkpr =="mismo",trpr,NA),
    ppst = ifelse(spkpst =="mismo",trpst,NA)
  )
orstwordsrn <-
  orstwordsrn %>% group_by(file, spk) %>% mutate(
    # Rng_mean_sp = round(mean(Pitch_range_St,na.rm = TRUE), 2),
    Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
    Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
    # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
    # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
    PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
    PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
    # Pdspkp = round(Pit_diff_sp_Hz / mean(Pitch_mean_Hz,na.rm = TRUE), 2) * 100
    # Pitch_mean_sp = round(mean(Pitch_mean_Hz,na.rm = TRUE), 2)
  ) %>% ungroup()

orstwordsrn<- sqldf(
  "select x.*,y.ip_id from orstwordsrn x left join orstip as y on (x.tmin*1000 >= (y.tmin*1000)) and (x.tmax*1000 <= (y.tmax*1000)) and (x.file = y.filename)and (x.spk = y.spk)")

words_count <- orstcomp%>%ungroup()%>% select(-tmin,-tmax,-dur)%>% group_by(w_id)%>% summarise(qphonemes =n(), fph = first(phon), lstph = last(phon))
words <- orstwordsrn %>% left_join(words_count, by="w_id")

# 12. Words tagging -----------------------------------------------------------

orstwords_tag <- orstwords
orstwords_tag$notag <- orstwords_tag$annotation
orstwords_tag <-
  udpipe_annotate(
    udmodel_spanish,
    x = orstwords_tag$notag,
    tokenizer = "vertical",
    tagger = "default",
    parser = "none",
    doc_id = orstwords_tag$w_id
  )
orstwords_tag <- as.data.frame(orstwords_tag) %>% select(doc_id,lemma,upos)
orstwords_tag$w_id <- orstwords_tag$doc_id

# Merging ending words data frame

orstwordsmer <- words%>%left_join(orstwords_tag,by="w_id")
orstwordsmer <- distinct(orstwordsmer%>%ungroup(),w_id,.keep_all = TRUE)

# Optional: download pos tagged words to review and reupload

write.csv(orstwords,"words.csv")
orstwords <- read.csv(file = "words.csv")

# Uncomment if you want to add sentiment analysis with a simple joining

sentiment <- read_csv(file="lexico_nrc.csv")
sentiment$token <- sentiment$palabra
orstwordsmer <- orstwordsmer %>% left_join(sentiment, by="token") 
orstwordsmer <- distinct(orstwordsmer%>%ungroup(),w_id, .keep_all = TRUE)
orstwordsmer <- orstwordsmer%>% select(-word,-doc_id)%>%rename(sentiment=sentimiento, qphonemes=phonemes)
orstwordsmer <- orstwordsmer %>% left_join(word_declin, by="w_id") 
orstwordsmer <- orstwordsmer %>% left_join(ip%>%select(ip_id,annotation), by="ip_id") 
orstwordsmer <- orstwordsmer %>% left_join(orstvowels%>%select(w_id,stressed,toneme,bound,tonic)%>%filter(tonic%in%"yes"), by="w_id")
orstwordsmer <- orstwordsmer %>% mutate(s_rate = (qphonemes/dur))
orstwordsmer <- orstwordsmer%>%rename(TOBI=stressed)
# Uncomment if you want to add pos tag to alophones.

orstcomp <- orstcomp%>%left_join(orstwords_tag,by="w_id")

# 13. Count tags by unit --------------------------------------------------------

orsttagcountip <- orstwordsmer %>% group_by(ip_id)%>% summarise(
  qadv = sum(upos == "ADV"),
  qnoun = sum(upos == "NOUN"),
  qverb = sum(upos == "VERB"),
  qdet = sum(upos == "DET"),
  qadj = sum(upos == "ADJ"),
  qpron = sum(upos == "PRON"),
  qconj = sum(upos %in% "ADV"),
  qangry = sum(sentimiento %in% "enfado"),
  qsadness = sum(sentimiento %in% "tristeza"),
  qfear = sum(sentimiento %in% "miedo"),
  qhappy = sum(sentimiento %in% "alegría"),
  qpos = sum(sentimiento %in% "positivo"),
  qneg = sum(sentimiento %in% "negativo"),
  qnasty = sum(sentimiento %in% "asco")
)



# 14. Intonational phrase developed data frame-------------------------------------------------------

orstip$tmin_ms <- orstip$tmin*1000
orstip$tmax_ms <- orstip$tmax*1000
# orstiprn <- orstip %>% rename(
#   spk = speaker,
#   annotation = text,
#   filename = filename
# )
# ip_num <- orstcomp%>%ungroup()%>% select(-tmin,-tmax,-duration)%>% group_by(ge_id)%>% summarise_if(is.numeric, mean, na.rm = TRUE)
ip_count <- orstcomp%>%ungroup()%>% select(-tmin,-tmax,-dur)%>% group_by(filename,ip_id)%>% summarise(qwords = n_distinct(w_id),qphonemes =n(),first(word),last(word),first(upos),last(upos))
ip_declin <- orstcomp%>%ungroup()%>% select(-tmin,-tmax,-dur)%>% filter(phon_type=="vowel") %>%group_by(filename,ip_id,phon_type)%>% summarise(declination = last(PimnSt)-first(PimnSt))

# orstip$tmin_ms <- orstip$tmin*1000
# orstip$tmax_ms <- orstip$tmax*1000
# orstiprn <- orstip %>% rename(
#   spk = speaker,
#   annotation = text,
#   filename = filename
# )

# Combine ip data frame with prosody 

ippros<- sqldf(
  "select x.*,y.Pitch, y.Intensity, y.Time_ms from orstip x left join prosodydb as y on (y.Time_ms >= (x.tmin_ms)) and (y.Time_ms <= (x.tmax_ms)) and (x.filename = y.filename)")

# orst_ge <-
#   orstiprn %>% group_by(ip_id) %>% mutate(Time_ms = list(tmin:tmax)) %>% ungroup() %>% unnest(Time_ms)

orstiprn <-
  ippros%>% 
  group_by(ip_id) %>% summarise(
    tier = max(tier),
    spk = max(spk),
    tmin = max(tmin),
    tmax = max(tmax),
    dur = max(dur),
    corpus = max(corpus),
    annotation = max(annotation),
    file = max(filename),
    PirHz = round(max(Pitch,na.rm=TRUE) - min(Pitch, na.rm = TRUE),2),
    PirSt = round(12*log2(max(Pitch,na.rm=TRUE)/min(Pitch, na.rm = TRUE)),2),
    Pimd = round(median(Pitch, na.rm = TRUE),2),
    PimnHz = round(mean(Pitch, na.rm = TRUE),2),
    PimnSt = round(12*log2(mean(Pitch,na.rm = TRUE)/1),2),
    Imd = round(median(Intensity, na.rm = TRUE),2),
    Imn = round(mean(Intensity, na.rm = TRUE),2),
    
  ) %>%ungroup()%>% arrange(file,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(file)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
# col_means <- lapply(orstalofrn%>%select(-transition_post, -transition_prev), mean, na.rm = TRUE)
# col_means <- lapply(col_means, round,2)
# orstalofrn <- replace_na(orstalofrn, col_means)
orstiprn <-
  orstiprn %>% mutate(
    RdprSt = PirSt - lag(PirSt),
    Idpr = Imn - lag(Imn, 1),
    spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
    spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
    ppr = ifelse(spkpr =="mismo",trpr,NA),
    ppst = ifelse(spkpst =="mismo",trpst,NA)
  )
orstiprn <-
  orstiprn %>% group_by(file, spk) %>% mutate(
    # Rng_mean_sp = round(mean(Pitch_range_St,na.rm = TRUE), 2),
    Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
    Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
    # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
    # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
    PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
    PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
    # Pdspkp = round(Pit_diff_sp_Hz / mean(Pitch_mean_Hz,na.rm = TRUE), 2) * 100
    # Pitch_mean_sp = round(mean(Pitch_mean_Hz,na.rm = TRUE), 2)
  ) %>% ungroup()

# words_count <- orstcomp%>%ungroup()%>% select(-tmin,-tmax,-dur)%>% group_by(w_id)%>% summarise(phonemes =n(), fph = first(phon), lstph = last(phon))
# ip_count$ip_id <- ip_count$ge_id
ip <- orstiprn %>% left_join(ip_count, by="ip_id")
ip <- ip %>% mutate(s_rate = qwords/dur)
ip <- ip %>% left_join(ip_declin, by="ip_id")
ip <- ip %>% left_join(orsttagcountip, by="ip_id")
ip <- ip %>% left_join(orstvowels%>%select(ip_id,stressed,toneme,bound,ipdecl)%>%filter(toneme%in%"yes"), by="ip_id")
ip <- ip%>%rename(TOBI=stressed)
# ip$spk <- as.factor(ip$spk)
# ip$file <- as.factor(ip$file.x)
# ip$stressed <- as.factor(ip$stressed)
# ip$bound <- as.factor(ip$bound)

# 15. Metadata ----------------------------------------------------------------
# Note: you can add metadata to every single data frame developed above

# 16. Export data frames to RDS ------------------------------------------------------
# Note: you can add here the data frames created above 
saveRDS(orstcomp%>%ungroup(),"phon.rds")
saveRDS(orstwordsmer%>%ungroup()%>%as_tibble(),"words.rds")
saveRDS(ip%>%select(-filename, -filename.x,-filename.y,-phon_type,-toneme),"ip.rds")
saveRDS(orstvowels,"vowels.rds")

# 17. Export data frames to CSV ------------------------------------------------------

write.csv(orstcomp,"phon.csv")
write.csv(orstwords,"words.csv")
write.csv(ip,"ip.csv")

# 18. Variables acronyms -----

# spk
# firstHz 
# midHz
# lastHz 
# firstSt
# midSt
# lastSt
# PirHz
# PirSt
# Pimd
# PimnHz
# PimnSt
# Imd
# Imn
# trpr
# trpst
# RdprSt
# Idpr
# spkpr
# spkpst
# ppr
# ppst
# ftopr
# ftopst
# Rdspk
# Idspk
# PdspkHz
# PdspkSt
# phon_type
# tonic
# Pdprvow
# Idprvow
# Pdprvow
# Idprvow
# PidprSt
# TOBI


                                                                         
