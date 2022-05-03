#-# LIBRERIE #-#----------------------------------------------------------------
library(jsonlite)
library(magrittr)
library(plotly)
library(dplyr)
library(tidyquant)

#-# VALUE BOX #-#---------------------------------------------------------------
dta_aggioranti <- dta_nazionale %>% 
  filter(data == dati_aggiornati)

dta_ieri <- dta_nazionale %>% 
  filter(data == dati_ieri)

tamponi_aggioranti <- tamponi %>% 
  filter(data == dati_aggiornati)

nuovi_positivi_oggi <- dta_aggioranti$nuovi_positivi
deceduti_oggi <- dta_aggioranti$deceduti - dta_ieri$deceduti
tamponi_oggi <- dta_aggioranti$tamponi - dta_ieri$tamponi
casi_tesstati_oggi <- dta_aggioranti$casi_testati - dta_ieri$casi_testati
positivi_oggi <- dta_aggioranti$totale_positivi
isolamento_oggi <- dta_aggioranti$isolamento_domiciliare

tasso_positivita <- round((nuovi_positivi_oggi/tamponi_oggi) * 100, 2)

tamp_mole_oggi <- tamponi_aggioranti$tamponi_test_molecolare_dbd
tamp_ant_oggi <- tamponi_aggioranti$tamponi_test_antigenico_rapido_dbd
tamp_moleP_oggi <- tamponi_aggioranti$totale_positivi_test_molecolare_dbd
tamp_antP_oggi <- tamponi_aggioranti$tamponi_test_antigenico_rapido_dbd


#completare i value box per le regioni
dta_regionale_aggioranti <- dta_regionale %>% 
  filter(data == dati_aggiornati)

dta_regionale_ieri <- dta_regionale %>% 
  filter(data == dati_ieri)

nuovi_positivi_regionali_oggi <- dta_regionale_aggioranti[,c(1,3,12)]

nuovi_positivi_regionali_oggi %>% 
  filter(denominazione_regione == 'Lazio')

dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$Regione,]$nuovi_positivi



  

#-# PIE GRAPH #-#---------------------------------------------------------------
ospedale_data <- ospedale %>% 
  filter(data == dati_aggiornati)

ospedale_data_ieri <- ospedale %>% 
  filter(data == dati_ieri)

dimessi_guariti_oggi <- ospedale_data$dimessi_guariti - ospedale_data_ieri$dimessi_guariti
terapia_intensiva_pie <- as.numeric(unique(ospedale_data$terapia_intensiva))
totale_ospedalizzati_pie <- as.numeric(unique(ospedale_data$totale_ospedalizzati))

labels_pie = c('Terapia Intensiva','Totale Ospedalizzati')
values_pie = c(terapia_intensiva_pie, totale_ospedalizzati_pie)

#-# GRAFICO STORICO CON BARRA TEMPORALE #-#-------------------------------------
dta_serie_storica_naz <- dta_nazionale[c(1,5,6,4)]

dta_serie_storica_reg <- dta_regionale[c(1,2,3,10,12,9)]

#-# GRAFICO STORICO PER TIPOLOGIE DI TAMPONI #-#--------------------------------
tto <- as.numeric(nrow(tamponi))
tamponi_singoli <- tamponi[c(331:tto),]

#-# TABELLA REGIONALE TAMPONI #-#-----------------------------------------------
tab_reg_tamponi <- tamp_RPR[,c(1:22)]

tab_reg_tamponi <- tab_reg_tamponi %>% 
  rename(
    'Abruzzo'= "Abruzzo_dbd",            
    'Basilicata'= "Basilicata_dbd" ,           
    'Calabria'= "Calabria_dbd",             
    'Campania'= "Campania_dbd",             
    'Emilia-Romagna'= "Emilia_Romagna_dbd",       
    'Friuli Venezia Giulia'= "Friuli_Venezia_Giulia_dbd",
    'Lazio'= "Lazio_dbd",                
    'Liguria'= "Liguria_dbd",              
    'Lombardia'= "Lombardia_dbd",           
    'Marche'= "Marche_dbd",               
    'Molise'= "Molise_dbd",               
    'P.A. Bolzano'= "Bolzano_dbd",              
    'P.A. Trento'= "Trento_dbd",               
    'Piemonte'= "Piemonte_dbd",             
    'Puglia'= "Puglia_dbd",               
    'Sardegna'= "Sardegna_dbd",             
    'Sicilia'= "Sicilia_dbd",              
    'Toscana'= "Toscana_dbd",              
    'Umbria'= "Umbria_dbd",               
    "Valle d'Aosta"= "Valle_dAosta_dbd",         
    'Veneto'= "Veneto_dbd"        
  )

#-# MAPPA REGIONI #-#-----------------------------------------------------------
dta_mappa <- dta_regionale_aggioranti[,c(1,2,3,7,9,10)]


totale_positivi_mappa <- with(dta_mappa, 
                              sum(totale_positivi[codice_regione == 21|codice_regione == 22]))
terapia_intensiva_mappa <- with(dta_mappa, 
                                sum(terapia_intensiva[codice_regione == 21|codice_regione == 22]))
isolamento_domiciliare_mappa <- with(dta_mappa, 
                                     sum(isolamento_domiciliare[codice_regione == 21|codice_regione == 22]))

dta_mappa[nrow(dta_mappa) + 1,] <- c('2022-05-02', 4, 'Trentino-Alto Adige/Südtirol',terapia_intensiva_mappa,
                                     isolamento_domiciliare_mappa,totale_positivi_mappa)

dta_mappa <- dta_mappa[-c(12,13), ]

dta_mappa$data <- as.Date(dta_mappa$data)
dta_mappa$codice_regione <- as.numeric(dta_mappa$codice_regione)
dta_mappa$terapia_intensiva <- as.numeric(dta_mappa$terapia_intensiva)
dta_mappa$isolamento_domiciliare <- as.numeric(dta_mappa$isolamento_domiciliare)
dta_mappa$totale_positivi <- as.numeric(dta_mappa$totale_positivi)

dta_mappa <- dta_mappa[order(dta_mappa$codice_regione),]


