#-# LIBRERIE #-#----------------------------------------------------------------
library(jsonlite)
library(magrittr)
library(plotly)
library(dplyr)
library(tidyquant)
library(leaflet)
library(viridis)

#-# DATA CHECK #-#--------------------------------------------------------------
oggi_data <- Sys.Date()
ieri_data <- Sys.Date() - 1
altro_ieri_data <- Sys.Date() - 2

now <- as.POSIXlt(Sys.time())
ora_oggi <- now$hour

if (ora_oggi < 18){
  dati_aggiornati <-  ieri_data
} else {
  dati_aggiornati <-  oggi_data
}

if (dati_aggiornati == ieri_data){
  dati_ieri <- altro_ieri_data
} else {
  dati_ieri <- ieri_data
}

#-# DATASET #-#-----------------------------------------------------------------

#-# dati ANDAMENTO NAZIONALE #-#------------------------------------------------
url_andamento_nazionale <- 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-andamento-nazionale.json'
dati_andamento_nazionale <- jsonlite::fromJSON(url_andamento_nazionale)
dati_andamento_nazionale <- as.data.frame(dati_andamento_nazionale)

#pulizia e operazione sui dati
dati_andamento_nazionale$data <- gsub("T18:00:00", "",dati_andamento_nazionale$data)
dati_andamento_nazionale$data <- gsub("T17:00:00", "",dati_andamento_nazionale$data)
dati_andamento_nazionale$data <- as.Date(dati_andamento_nazionale$data)
dta_nazionale <- dati_andamento_nazionale[,c(1,4,5,6,7,9,10,11,14,15,16,18,21,22,23,24)] 

#-# dati ANDAMENTO REGIONALE #-#------------------------------------------------
url_andamento_regionale <- 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv'
dati_andamento_regionale <- read.csv(url_andamento_regionale)
dati_andamento_regionale <- as.data.frame(dati_andamento_regionale)

#pulizia e operazione sui dati
dati_andamento_regionale$data <- gsub("T18:00:00", "",dati_andamento_regionale$data)
dati_andamento_regionale$data <- gsub("T17:00:00", "",dati_andamento_regionale$data)
dati_andamento_regionale$data <- as.Date(dati_andamento_regionale$data)
dta_regionale <- dati_andamento_regionale[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,18,19)]

#-# dati MAPPA #-#--------------------------------------------------------------
states <- geojsonio::geojson_read("Desktop/CA/code_r/PROGETTO/CovidDataHunters/limits_IT_regions .geojson", what = "sp")

#-# SUBSET DATI OSPEDALIZAZIONI #-#---------------------------------------------
ospedale <- dta_nazionale[,c(1,2,3,7)]

ospedale_reg <- dta_regionale[,c(1,2,3,6,7,8,13)]

#-# SUBSET TAMPONI NAZIONALI#-#-------------------------------------------------
tamponi <- dta_nazionale[c(1,10,13,14,15,16)]

tamponi[is.na(tamponi)] <- 0 #sostituzione degli NA con il valore 0

#-# OPERAZIONI SU SUBSET TAMPONI #-#--------------------------------------------

#TAMPONI DBD (day by day)
tamponi_dbd <- vector()
to <- as.numeric(nrow(tamponi))
add <-  0
for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamponi[tamponi$data==operazione,]
  gamma <- tamponi[tamponi$data==operazione+1,]
  x <- gamma$tamponi
  y <- sigma$tamponi
  risultato <- x-y
  tamponi_dbd <- append(tamponi_dbd,risultato)
}

tamponi_dbd <- append(tamponi_dbd,4324)

tamponi_dbd <- as.array(tamponi_dbd)
tamponi_dbd <- as.data.frame(tamponi_dbd)
to_tamponi_dbd <- as.numeric(nrow(tamponi_dbd))
tamponi_dbd <-  tamponi_dbd[c(to_tamponi_dbd:1),]
tamponi_dbd <- as.data.frame(tamponi_dbd)

tamponi['tamponi_dbd'] <- tamponi_dbd

#TAMPONI MOLECOLARI POSITIBI BDB
totale_positivi_test_molecolare_dbd <- vector()
to <- as.numeric(nrow(tamponi))
add <-  0
for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamponi[tamponi$data==operazione,]
  gamma <- tamponi[tamponi$data==operazione+1,]
  x <- gamma$totale_positivi_test_molecolare
  y <- sigma$totale_positivi_test_molecolare
  risultato <- x-y
  totale_positivi_test_molecolare_dbd <- append(totale_positivi_test_molecolare_dbd,risultato)
}

totale_positivi_test_molecolare_dbd <- append(totale_positivi_test_molecolare_dbd,0)

totale_positivi_test_molecolare_dbd <- as.array(totale_positivi_test_molecolare_dbd)
totale_positivi_test_molecolare_dbd <- as.data.frame(totale_positivi_test_molecolare_dbd)
to_totale_positivi_test_molecolare_dbd <- as.numeric(nrow(totale_positivi_test_molecolare_dbd))
totale_positivi_test_molecolare_dbd <-  totale_positivi_test_molecolare_dbd[c(to_totale_positivi_test_molecolare_dbd:1),]
totale_positivi_test_molecolare_dbd <- as.data.frame(totale_positivi_test_molecolare_dbd)

tamponi['totale_positivi_test_molecolare_dbd'] <- totale_positivi_test_molecolare_dbd

#TAMPONI ANTIGENICI POSITIVI BDB
totale_positivi_test_antigenico_rapido_dbd <- vector()
to <- as.numeric(nrow(tamponi))
add <-  0
for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamponi[tamponi$data==operazione,]
  gamma <- tamponi[tamponi$data==operazione+1,]
  x <- gamma$totale_positivi_test_antigenico_rapido
  y <- sigma$totale_positivi_test_antigenico_rapido
  risultato <- x-y
  totale_positivi_test_antigenico_rapido_dbd <- append(totale_positivi_test_antigenico_rapido_dbd,risultato)
}

totale_positivi_test_antigenico_rapido_dbd <- append(totale_positivi_test_antigenico_rapido_dbd,0)

totale_positivi_test_antigenico_rapido_dbd <- as.array(totale_positivi_test_antigenico_rapido_dbd)
totale_positivi_test_antigenico_rapido_dbd <- as.data.frame(totale_positivi_test_antigenico_rapido_dbd)
to_totale_positivi_test_antigenico_rapido_dbd <- as.numeric(nrow(totale_positivi_test_antigenico_rapido_dbd))
totale_positivi_test_antigenico_rapido_dbd <-  totale_positivi_test_antigenico_rapido_dbd[c(to_totale_positivi_test_antigenico_rapido_dbd:1),]
totale_positivi_test_antigenico_rapido_dbd <- as.data.frame(totale_positivi_test_antigenico_rapido_dbd)

tamponi['totale_positivi_test_antigenico_rapido_dbd'] <- totale_positivi_test_antigenico_rapido_dbd

#TAMPONI MOLECOLARI BDB
tamponi_test_molecolare_dbd <- vector()
to <- as.numeric(nrow(tamponi))
add <-  0
for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamponi[tamponi$data==operazione,]
  gamma <- tamponi[tamponi$data==operazione+1,]
  x <- gamma$tamponi_test_molecolare
  y <- sigma$tamponi_test_molecolare
  risultato <- x-y
  tamponi_test_molecolare_dbd <- append(tamponi_test_molecolare_dbd,risultato)
}

tamponi_test_molecolare_dbd <- append(tamponi_test_molecolare_dbd,0)

tamponi_test_molecolare_dbd <- as.array(tamponi_test_molecolare_dbd)
tamponi_test_molecolare_dbd <- as.data.frame(tamponi_test_molecolare_dbd)
to_tamponi_test_molecolare_dbd <- as.numeric(nrow(tamponi_test_molecolare_dbd))
tamponi_test_molecolare_dbd <-  tamponi_test_molecolare_dbd[c(to_tamponi_test_molecolare_dbd:1),]
tamponi_test_molecolare_dbd <- as.data.frame(tamponi_test_molecolare_dbd)

tamponi['tamponi_test_molecolare_dbd'] <- tamponi_test_molecolare_dbd

#TAMPONI ANTIGENICI RAPIDI BDB
tamponi_test_antigenico_rapido_dbd <- vector()
to <- as.numeric(nrow(tamponi))
add <-  0
for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamponi[tamponi$data==operazione,]
  gamma <- tamponi[tamponi$data==operazione+1,]
  x <- gamma$tamponi_test_antigenico_rapido
  y <- sigma$tamponi_test_antigenico_rapido
  risultato <- x-y
  tamponi_test_antigenico_rapido_dbd <- append(tamponi_test_antigenico_rapido_dbd,risultato)
}

tamponi_test_antigenico_rapido_dbd <- append(tamponi_test_antigenico_rapido_dbd,0)

tamponi_test_antigenico_rapido_dbd <- as.array(tamponi_test_antigenico_rapido_dbd)
tamponi_test_antigenico_rapido_dbd <- as.data.frame(tamponi_test_antigenico_rapido_dbd)
to_tamponi_test_antigenico_rapido_dbd <- as.numeric(nrow(tamponi_test_antigenico_rapido_dbd))
tamponi_test_antigenico_rapido_dbd <-  tamponi_test_antigenico_rapido_dbd[c(to_tamponi_test_antigenico_rapido_dbd:1),]
tamponi_test_antigenico_rapido_dbd <- as.data.frame(tamponi_test_antigenico_rapido_dbd)

tamponi['tamponi_test_antigenico_rapido_dbd'] <- tamponi_test_antigenico_rapido_dbd

#OPERAZIONI COMPLETATE SUBSET TAMPONI 
tamponi <- tamponi[c(1,7,8,9,10,11)]

#effettuiamo una media mobile a 7 giorni
tamponi <- tamponi %>% 
  mutate(M7_tamponi_dbd = rollmean(tamponi_dbd, 7, na.pad = T)) %>%
  mutate(M7_totale_positivi_test_molecolare_dbd = rollmean(totale_positivi_test_molecolare_dbd, 7, na.pad = T)) %>% 
  mutate(M7_totale_positivi_test_antigenico_rapido_dbd = rollmean(totale_positivi_test_antigenico_rapido_dbd, 7, na.pad = T)) %>% 
  mutate(M7_tamponi_test_molecolare_dbd = rollmean(tamponi_test_molecolare_dbd, 7, na.pad = T)) %>% 
  mutate(M7_tamponi_test_antigenico_rapido_dbd = rollmean(tamponi_test_antigenico_rapido_dbd, 7, na.pad = T))  

#-# SUBSET TAMPONI REGIONALI#-#-------------------------------------------------
tamponi_reg <- dta_regionale[c(1,2,3,16)]

tamp_RPR <- as.data.frame(unique(tamponi_reg$data))
colnames(tamp_RPR)[1] <- 'data'

#-# OPERAZIONI SU SUBSET TAMPONI #-#--------------------------------------------

#-------------------------------------------------
RegionSplitterColumns <-  function(nome_regione){
  #funzione per selezionare i tamponi per singole regioni creando il dataset
  regione_singola <- tamponi_reg %>% 
    filter(denominazione_regione == nome_regione)
  
  x <- as.data.frame(regione_singola$tamponi)
  
  tamp_RPR[nome_regione] <- x
  
  return(tamp_RPR)
}
#-------------------------------------------------

#Abruzzo
tamp_RPR <- RegionSplitterColumns('Abruzzo')
#Basilicata
tamp_RPR <- RegionSplitterColumns('Basilicata')
#Calabria
tamp_RPR <- RegionSplitterColumns('Calabria')
#Campania
tamp_RPR <- RegionSplitterColumns('Campania')
#Emilia-Romagna
tamp_RPR <- RegionSplitterColumns('Emilia-Romagna')
#Friuli Venezia Giulia
tamp_RPR <- RegionSplitterColumns('Friuli Venezia Giulia')
#Lazio
tamp_RPR <- RegionSplitterColumns('Lazio')
#Liguria
tamp_RPR <- RegionSplitterColumns('Liguria')
#Lombardia
tamp_RPR <- RegionSplitterColumns('Lombardia')
#Marche
tamp_RPR <- RegionSplitterColumns('Marche')
#Molise
tamp_RPR <- RegionSplitterColumns('Molise')
#P.A. Bolzano
tamp_RPR <- RegionSplitterColumns('P.A. Bolzano')
#P.A. Trento
tamp_RPR <- RegionSplitterColumns('P.A. Trento')
#Piemonte
tamp_RPR <- RegionSplitterColumns('Piemonte')
#Puglia
tamp_RPR <- RegionSplitterColumns('Puglia')
#Sardegna
tamp_RPR <- RegionSplitterColumns('Sardegna')
#Sicilia
tamp_RPR <- RegionSplitterColumns('Sicilia')
#Toscana
tamp_RPR <- RegionSplitterColumns('Toscana')
#Umbria
tamp_RPR <- RegionSplitterColumns('Umbria')
#Valle d'Aosta
tamp_RPR <- RegionSplitterColumns("Valle d'Aosta")
#Veneto
tamp_RPR <- RegionSplitterColumns('Veneto')

#--------------------------------------------
#Abruzzo
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Abruzzo
  y <- sigma$Abruzzo
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Abruzzo_dbd'] <- v

#Basilicata
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Basilicata
  y <- sigma$Basilicata
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Basilicata_dbd'] <- v

#Calabria
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Calabria
  y <- sigma$Calabria
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Calabria_dbd'] <- v

#Campania
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Campania
  y <- sigma$Campania
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Campania_dbd'] <- v

#Emilia-Romagna
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$'Emilia-Romagna'
  y <- sigma$'Emilia-Romagna'
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Emilia_Romagna_dbd'] <- v

#Friuli Venezia Giulia
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$'Friuli Venezia Giulia'
  y <- sigma$'Friuli Venezia Giulia'
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Friuli_Venezia_Giulia_dbd'] <- v

#Lazio
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Lazio
  y <- sigma$Lazio
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Lazio_dbd'] <- v

#Liguria
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Liguria
  y <- sigma$Liguria
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Liguria_dbd'] <- v

#Lombardia
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Lombardia
  y <- sigma$Lombardia
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Lombardia_dbd'] <- v

#Marche
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Marche
  y <- sigma$Marche
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Marche_dbd'] <- v

#Molise
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Molise
  y <- sigma$Molise
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Molise_dbd'] <- v

#P.A. Bolzano
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$'P.A. Bolzano'
  y <- sigma$'P.A. Bolzano'
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Bolzano_dbd'] <- v

#P.A. Trento
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$'P.A. Trento'
  y <- sigma$'P.A. Trento'
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Trento_dbd'] <- v

#Piemonte
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Piemonte
  y <- sigma$Piemonte
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Piemonte_dbd'] <- v

#Puglia
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Puglia
  y <- sigma$Puglia
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Puglia_dbd'] <- v

#Sardegna
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Sardegna
  y <- sigma$Sardegna
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Sardegna_dbd'] <- v

#Sicilia
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Sicilia
  y <- sigma$Sicilia
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Sicilia_dbd'] <- v

#Toscana
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Toscana
  y <- sigma$Toscana
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Toscana_dbd'] <- v

#Umbria
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Umbria
  y <- sigma$Umbria
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Umbria_dbd'] <- v

#Valle d'Aosta
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$"Valle d'Aosta"
  y <- sigma$"Valle d'Aosta"
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Valle_dAosta_dbd'] <- v

#Veneto
v <- vector()
to <- as.numeric(nrow(tamp_RPR))
add <-  0

for (val in 1: to){
  add <- add + 1
  operazione <- dati_aggiornati - add
  sigma <- tamp_RPR[tamp_RPR$data==operazione,]
  gamma <- tamp_RPR[tamp_RPR$data==operazione+1,]
  x <- gamma$Veneto
  y <- sigma$Veneto
  risultato <- x-y
  v <- append(v,risultato)
}
v <- append(v,0)

v <- as.array(v)
v <- as.data.frame(v)
to_v <- as.numeric(nrow(v))
v <-  v[c(to_v:1),]
v <- as.data.frame(v)

tamp_RPR['Veneto_dbd'] <- v

#OPERAZIONI COMPLETATE SUBSET TAMPONI 
tamp_RPR <- tamp_RPR[c(1,23:43)]

#effettuiamo una media mobile a 7 giorni
tamp_RPR <- tamp_RPR %>% 
  mutate(Abruzzo = rollmean(Abruzzo_dbd, 7, na.pad = T)) %>%
  mutate(Basilicata = rollmean(Basilicata_dbd, 7, na.pad = T)) %>% 
  mutate(Calabria = rollmean(Calabria_dbd, 7, na.pad = T)) %>% 
  mutate(Campania = rollmean(Campania_dbd, 7, na.pad = T)) %>% 
  mutate('Emilia Romagna' = rollmean(Emilia_Romagna_dbd, 7, na.pad = T)) %>%
  mutate('Friuli Venezia Giulia' = rollmean(Friuli_Venezia_Giulia_dbd, 7, na.pad = T)) %>%
  mutate(Lazio = rollmean(Lazio_dbd, 7, na.pad = T)) %>% 
  mutate(Liguria = rollmean(Liguria_dbd, 7, na.pad = T)) %>% 
  mutate(Lombardia = rollmean(Lombardia_dbd, 7, na.pad = T)) %>% 
  mutate(Marche = rollmean(Marche_dbd, 7, na.pad = T)) %>%
  mutate(Molise = rollmean(Molise_dbd, 7, na.pad = T)) %>%
  mutate('P.A. Bolzano' = rollmean(Bolzano_dbd, 7, na.pad = T)) %>% 
  mutate('P.A. Trento' = rollmean(Trento_dbd, 7, na.pad = T)) %>% 
  mutate(Piemonte = rollmean(Piemonte_dbd, 7, na.pad = T)) %>% 
  mutate(Puglia = rollmean(Puglia_dbd, 7, na.pad = T)) %>%
  mutate(Sardegna = rollmean(Sardegna_dbd, 7, na.pad = T)) %>%
  mutate(Sicilia = rollmean(Sicilia_dbd, 7, na.pad = T)) %>% 
  mutate(Toscana = rollmean(Toscana_dbd, 7, na.pad = T)) %>% 
  mutate(Umbria = rollmean(Umbria_dbd, 7, na.pad = T)) %>% 
  mutate("Valle d'Aosta" = rollmean(Valle_dAosta_dbd, 7, na.pad = T)) %>%
  mutate(Veneto = rollmean(Veneto_dbd, 7, na.pad = T))
  





