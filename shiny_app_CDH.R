library(shiny)
library(shinydashboard)

#-# ui #-#----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  title = "CovidDataHunters",
  
  # HEADER ---------------------------------------------------------------------
  
  dashboardHeader(title = "CovidDataHunters",titleWidth = 250),
  
  # SIDEBAR --------------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dati Nazionali", tabName = "tab_nazionale",
               icon = icon("globe")),
      menuItem("Dati Regionali", tabName = "tab_regioni",
               icon = icon("map")),
      menuItem("Ospedali", tabName = "tab_ospedali",
               icon = icon("hospital"))
    )
  ),
  
  # BODY -----------------------------------------------------------------------
  
  dashboardBody(
    ### CUSTOM FONT : title --------------------------------------------------
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '
    ))),
    
    tabItems(
      ### MENU : dati_nazionali ------------------------------------------------
      tabItem(
        tabName = "tab_nazionale",
        
        fluidRow(
          box(
            width = 12, background = "aqua",
            "I DATI PRESENTI SONO AGGIORNATI AL GIORNO:",dati_aggiornati
          )
        ),
        
        fluidRow(
          box(title = 'INFORMAZIONI NAZIONALI GENERALI',
              width = 12, background = "blue"
          )
        ),
        
        fluidRow(
          valueBox( nuovi_positivi_oggi,"Nuovi Positivi",
                    color = "orange",
                    icon = icon("plus")),
          
          valueBox( deceduti_oggi,"Deceduti",
                    color = "red",
                    icon = icon("skull")),
          
          valueBox( tamponi_oggi,"Tamponi",
                    color = "olive",
                    icon = icon("flask")),
          
          valueBox(isolamento_oggi,"Isolamento Domiciliare",
                   color = 'light-blue',
                   icon = icon("hourglass-half")),
          
          valueBox(positivi_oggi,"Persone attualmete Positive", 
                   color = 'orange',
                   icon = icon("virus")),
          
          valueBox(paste0(tasso_positivita, "%"),"Tasso di positività",
                   color = 'yellow',
                   icon = icon("head-side-cough"))
          
        ),
        
        fluidRow(
          box(title = "Seleziona una Variablie per la Serie Storica", 
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 4.5,
              selectInput("Variabile", "Variabile:",
                          choices=colnames(dta_serie_storica_naz[,2:4]))
          ),
          
          box(title = "Serie Storica", status = "primary", 
              width = 8.5,
              plotlyOutput("grafico_storico")
          )
        ),
        
        fluidRow(
          box(title = 'INFORMAZIONI RIGUARDO TAMPONI',
              width = 12, background = "blue"
          )
        ),
        
        fluidRow(
          box(title = "Serie Storica di tutti i Tamponi", 
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 8,
              plotlyOutput("grafico_storico_tamponi")
          ),
          
          valueBox(tamp_mole_oggi,"Tamponi Molecolari",
                   color = 'olive',
                   icon = icon("flask")),
          
          valueBox(tamp_moleP_oggi,"Tamponi Molecolari Positivi",
                   color = 'orange',
                   icon = icon("virus")),
          
          valueBox(tamp_ant_oggi,"Tamponi Antigenici Rapidi",
                   color = 'olive',
                   icon = icon("vial")),
          
          valueBox(tamp_antP_oggi,"Tamponi Antigenici Rapidi Positivi",
                   color = 'orange',
                   icon = icon("virus"))
        ),
        
        fluidRow(
          box(
            width = 12, background = "aqua",
            "I DATI CHE VENGONO VISUALIZZATI NEL GRAFICO: Serie Storica delle Tipologie di Tampone, SONO OTTENUTI ESEGUENDO UNA MEDIA MOBILE A 7 GIORNI"
          )
        ),
        
        fluidRow(
          box(title = "Serie Storica delle Tipologie di Tampone", status = "primary", 
              width = 8.5,
              plotlyOutput("grafico_storico_tamponiMA"))
        )
      ),#fine tab_nazionale
      
      ### MENU : dati_regionali -------------------------------------------------
      tabItem(
        tabName = "tab_regioni",
        
        fluidRow(
          box(
            width = 12, background = "aqua",
            "I DATI PRESENTI SONO AGGIORNATI AL GIORNO:",dati_aggiornati
          )
        ),
        
        fluidRow(
          box(title = 'MAPPA NAZIONALE DEI POSITIVI PER REGIONE',
              width = 12, background = "blue"
          )
        ),
        
        fluidRow(
          box(status = "primary", 
              width = 8.5,
              leafletOutput("mappa")
          )
        ),
        
        fluidRow(
          box(title = "Seleziona la Regione di cui vuoi visualizzare di dati", 
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 4.5,
              selectInput("Regione", "Regione:",
                          choices=unique(dta_regionale['denominazione_regione']))
          )
        ),
        
        fluidRow(
          box(title = 'INFORMAZIONI GENERALI RIGUARDO LA REGIONE SCELTA',
              width = 12, background = "blue"
          )
        ),
        
        fluidRow(
          infoBoxOutput("nuovi_positivi"),
          infoBoxOutput("deceduti"),
          infoBoxOutput("tamponi"),
          infoBoxOutput("isolamento_domiciliare"),
          infoBoxOutput("positivi"),
          infoBoxOutput("tasso_positivita")
        ),
        
        fluidRow(
          box(solidHeader = TRUE,collapsible = TRUE,
              title = "Seleziona una Variablie per la Serie Storica", 
              status = "primary",width = 4.5,
              selectInput("VariabileR", "Variabile:",
                          choices=colnames(dta_serie_storica_reg[,4:6]))
          ),
          
          box(title = "Serie Storica", status = "primary", 
              width = 8.5,
              plotlyOutput("grafico_storico_regionale")
          )
        ),
        
        fluidRow(
          box(title = 'INFORMAZIONI RIGUARDO TAMPONI RIGUARDO LA REGIONE SCELTA',
              width = 12, background = "blue"
          )
        ),
        
        fluidRow(
          box(
            width = 12, background = "aqua",
            "I DATI CHE VENGONO VISUALIZZATI NEL GRAFICO: 
            Serie Storica di tutti i Tamponi della Regione scelta, 
            SONO OTTENUTI ESEGUENDO UNA MEDIA MOBILE A 7 GIORNI"
          ),
        ),
        
        fluidRow(
          box(title = "Serie Storica di tutti i Tamponi della Regione scelta", 
              status = "primary",width = 8,
              plotlyOutput("grafico_storico_tamponi_reg")
          ),
          
          box(width = 4,
              DT::dataTableOutput('tabella_tamponi_regione')
          )
        ),
        
        fluidRow(
          box(title = 'COMPARAZIONE DEI TAMPONI EFFETUATI TRA TUTTE LE REGIONI',
              width = 12, background = "blue"
          )
        ),
        
        fluidRow(
          box(
            width = 12, background = "aqua",
            "I DATI CHE VENGONO VISUALIZZATI NEL GRAFICO: Comparazione Grafici Tamponi Regionali, SONO OTTENUTI ESEGUENDO UNA MEDIA MOBILE A 7 GIORNI"
          )
        ),
        
        fluidRow(
          box(title = "Comparazione Grafici Tamponi Regionali", 
              status = "primary",width = 8.5,
              plotlyOutput("storico_tamponi_comparato")
          )
        )
      ),#fine tab_regioni
      
      ### MENU : ospedali ------------------------------------------------------
      tabItem(
        tabName = "tab_ospedali",
        
        fluidRow(
          box(
            width = 12, background = "aqua",
            "I DATI PRESENTI SONO AGGIORNATI AL GIORNO:",dati_aggiornati
          )
        ),
        
        fluidRow(
          box(title = 'DATI OSPEDALI A LIVELLO NAZIONALE',
              width = 12, background = "blue"
          )
        ),
        
        fluidRow(
          box(title = "PieChart degli ospedalizzati a livello Nazionale", status = "primary"
              , height = 360,width = 8,
              plotlyOutput("torta_ospedalizati")
          ),
          
          valueBox(terapia_intensiva_pie,"Terapia Intensiva",
                   color = "red",
                   icon = icon("bed")),
          
          valueBox(totale_ospedalizzati_pie,"Ospedalizzati",
                   color = "yellow",
                   icon = icon("briefcase-medical")),
          
          valueBox(dimessi_guariti_oggi,"Dimessi Guariti",
                   color = "green",
                   icon = icon("user-check"))
        ),
        
        fluidRow(
          box(title = "Serie Storica: Ospedalizazioni e Terapie Intensive a livello Nazionale", 
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 8.5,
              plotlyOutput("grafico_storico_ospedale")
          )
        ),
        
        fluidRow(
          box(title = 'DATI OSPEDALI A LIVELLO REGIONALE',
              width = 12, background = "blue"
          )
        ),
        
        fluidRow(
          box(title = "Seleziona la Regione di cui vuoi visualizzare di dati", 
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 4.5,
              selectInput("RegioneO", "Regione:",
                          choices=unique(dta_regionale['denominazione_regione']))
          )
        ),
        
        fluidRow(
          infoBoxOutput("terapia_intensiva"),
          infoBoxOutput("ospedalizati"),
          infoBoxOutput("dimessi_guariti")
        ),
        
        fluidRow(
          box(title = "PieChart degli ospedalizzati a livello Regionale", status = "primary"
              , height = 480,width = 6,solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("torta_ospedalizati_regionale")
          ),
          
          box(title = "Serie Storica: Ospedalizazioni e Terapie Intensive a livello Regionale", 
              status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 6,
              plotlyOutput("grafico_storico_ospedale_regionale")
          )
        )
      )#fine tab_ospedali
    )#fine TABITEMS
  )
)
  

#-# server #-#------------------------------------------------------------------
server <- function(input, output) {
  
  output$grafico_storico <- renderPlotly({
    storico_generale <- plot_ly(dta_serie_storica_naz, type = 'scatter', mode = 'lines')%>%
      add_lines(x = ~data, y = ~get(input$Variabile))%>%
      layout(showlegend = F,colorway = '#d93030',
             xaxis = list(rangeslider = list(visible = T),
                          rangeselector=list(
                            buttons=list(
                              list(count=1, label="1 mese", step="month", stepmode="backward"),
                              list(count=6, label="6 mesi", step="month", stepmode="backward"),
                              list(count=1, label="1 anno", step="year", stepmode="backward"),
                              list(count=1, label="dal 1 gennaio", step="year", stepmode="todate"),
                              list(label='tutto', step="all")
                            ))))
    storico_generale <- storico_generale %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6')

  })
  
  output$torta_ospedalizati <- renderPlotly({
    ospedale_pie <- plot_ly(type='pie', labels=labels_pie, values=values_pie, 
                            textinfo='percent',
                            insidetextorientation='radial',
                            marker= list(colors = c("#d93030", "#ffa40c")),
                            height = 300)
  })
  
  output$grafico_storico_ospedale <- renderPlotly({
    grafico_storico_ospedale <- 
      plot_ly(ospedale, x = ~data, y = ~terapia_intensiva, 
              name = 'Terapia Intensiva', type = 'scatter', mode = 'lines',
              line = list(color = '#d93030', width = 4)) 
    grafico_storico_ospedale <- grafico_storico_ospedale %>% add_trace(y = ~totale_ospedalizzati, 
                                                  name = 'Ospedalizati', 
                                                  line = list(color = '#ffa40c', width = 4)) 
    grafico_storico_ospedale <- grafico_storico_ospedale %>% layout(xaxis = list(title = "Mesi"),
                                               yaxis = list (title = ""))
  })
  
  output$grafico_storico_tamponi <- renderPlotly({
    grafico_storico_tamponi <- plot_ly(tamponi, x = ~data, 
                                       y = tamponi$tamponi_dbd, 
                                       name = 'Tamponi', type = 'scatter', mode = 'lines',
                                       line = list(color = '#d93030')) 
    
    grafico_storico_tamponi <- grafico_storico_tamponi %>% layout(xaxis = list(title = "Mesi"),
                                                                  yaxis = list (title = ""))
  })
  
  output$grafico_storico_tamponiMA <- renderPlotly({
    grafico_storico_tamponiMA <- plot_ly(tamponi_singoli, x = ~data, y = ~M7_tamponi_test_molecolare_dbd, 
                                         name = 'Test Molecolari', type = 'scatter', 
                                         mode = 'lines',
                                         line = list(color = '#fd7f00')) 
    grafico_storico_tamponiMA <- grafico_storico_tamponiMA %>% add_trace(y = ~M7_totale_positivi_test_molecolare_dbd, 
                                                                         name = 'Test Molacolari Positivi', 
                                                                         line = list(color = '#e43900')) 
    grafico_storico_tamponiMA <- grafico_storico_tamponiMA %>% add_trace(y = ~M7_tamponi_test_antigenico_rapido_dbd, 
                                                                         name = 'Test Antigenici Rapidi', 
                                                                         line = list(color = '#2db100', mode = 'lines')) 
    grafico_storico_tamponiMA <- grafico_storico_tamponiMA %>% add_trace(y = ~M7_totale_positivi_test_antigenico_rapido_dbd, 
                                                                         name = 'Test Antigenici Rapid Positivi', 
                                                                         line = list(color = '#00abe4', mode = 'lines')) 
    
    grafico_storico_tamponiMA <- grafico_storico_tamponiMA %>% layout(xaxis = list(title = "Months"),
                                                                      yaxis = list (title = ""))
    
  })
  
  output$grafico_storico_regionale <- renderPlotly({
    grafico_storico_regionale <- plot_ly(dta_serie_storica_reg, type = 'scatter', mode = 'lines')%>%
      filter(denominazione_regione %in% input$Regione) %>%
      group_by(denominazione_regione) %>%
      add_lines(x = ~data, y = ~get(input$VariabileR))%>%
      layout(showlegend = F,colorway = '#d93030',
             xaxis = list(rangeslider = list(visible = T),
                          rangeselector=list(
                            buttons=list(
                              list(count=1, label="1 mese", step="month", stepmode="backward"),
                              list(count=6, label="6 mesi", step="month", stepmode="backward"),
                              list(count=1, label="1 anno", step="year", stepmode="backward"),
                              list(count=1, label="dal 1 gennaio", step="year", stepmode="todate"),
                              list(label='tutto', step="all")
                            ))))
    grafico_storico_regionale <- grafico_storico_regionale %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6')
    
  })
  
  output$grafico_storico_tamponi_reg <- renderPlotly({
    grafico_storico_tamponi_reg <- plot_ly(tamp_RPR, x = ~data, 
                                       y = ~get(input$Regione), 
                                       name = 'Tamponi', type = 'scatter', mode = 'lines',
                                       line = list(color = '#d93030'))
    
    grafico_storico_tamponi_reg <- grafico_storico_tamponi_reg %>% layout(xaxis = list(title = "Mesi"),
                                                                  yaxis = list (title = ""))
  })
  
  output$nuovi_positivi <- renderInfoBox({
    infoBox(
      "Nuovi Positivi", 
      dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$Regione,]$nuovi_positivi
      , icon = icon("plus"),
      color = "orange", fill = TRUE
    )
  })
  
  output$deceduti <- renderInfoBox({
    infoBox(
      "Deceduti", 
      dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$Regione,]$deceduti-
        dta_regionale_ieri[dta_regionale_ieri$denominazione_regione==input$Regione,]$deceduti
      , icon = icon("skull"),
      color = "red", fill = TRUE
    )
  })
  
  output$tamponi <- renderInfoBox({
    infoBox(
      "Tamponi", 
      dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$Regione,]$tamponi -
        dta_regionale_ieri[dta_regionale_ieri$denominazione_regione==input$Regione,]$tamponi 
      , icon = icon("flask"),
      color = "olive", fill = TRUE
    )
  })
  
  output$isolamento_domiciliare <- renderInfoBox({
    infoBox(
      "Isolamento Domiciliare", 
      dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$Regione,]$isolamento_domiciliare
      ,icon = icon("hourglass-half"),
      color = "light-blue", fill = TRUE
    )
  })
  
  output$positivi <- renderInfoBox({
    infoBox(
      "Persone attualmente Positive", 
      dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$Regione,]$totale_positivi
      , icon = icon("virus"),
      color = "orange", fill = TRUE
    )
  })
  
  output$tasso_positivita <- renderInfoBox({
    infoBox(
      "Tasso di positivià", 
      
      round((dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$Regione,]$nuovi_positivi/
             (dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$Regione,]$tamponi -
                dta_regionale_ieri[dta_regionale_ieri$denominazione_regione==input$Regione,]$tamponi ))*100 ,2)
      
      
      , icon = icon("head-side-cough"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$tabella_tamponi_regione <- DT::renderDataTable({
    DT::datatable(tab_reg_tamponi[,c('data',input$Regione)], options = list(pageLength = 6))
  })
  
  output$storico_tamponi_comparato <- renderPlotly({
    storico_tamponi_comparato <- plot_ly(tamp_RPR, x = ~data, y = ~Abruzzo, 
                                         name = 'Abruzzo', type = 'scatter', 
                                         mode = 'lines') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Basilicata, 
                                                                         name = 'Basilicata') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Calabria, 
                                                                         name = 'Calabria') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Campania, 
                                                                         name = 'Campania')
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~'Emilia-Romagna', 
                                                                         name = 'Emilia-Romagna') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~'Friuli Venezia Giulia', 
                                                                         name = 'Friuli Venezia Giulia') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Lazio, 
                                                                         name = 'Lazio')
    
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Liguria, 
                                                                         name = 'Liguria') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Lombardia, 
                                                                         name = 'Lombardia') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Marche, 
                                                                         name = 'Marche')
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Molise, 
                                                                         name = 'Molise') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~'P.A. Bolzano', 
                                                                         name = 'P.A. Bolzano') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~'P.A. Trento', 
                                                                         name = 'P.A. Trento')
    
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Piemonte, 
                                                                         name = 'Piemonte') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Puglia, 
                                                                         name = 'Puglia') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Sardegna, 
                                                                         name = 'Sardegna')
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Sicilia, 
                                                                         name = 'Sicilia') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Toscana, 
                                                                         name = 'Toscana') 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Umbria, 
                                                                         name = 'Umbria')
    
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~"Valle d'Aosta", 
                                                                         name = "Valle d'Aosta") 
    storico_tamponi_comparato <- storico_tamponi_comparato %>% add_trace(y = ~Veneto, 
                                                                         name = 'Veneto')
    
    
    storico_tamponi_comparato <- storico_tamponi_comparato %>% layout(xaxis = list(title = "Months"),
                                                                      yaxis = list (title = ""))
    
  })
  
  output$terapia_intensiva <- renderInfoBox({
    infoBox(
      "Terapia Intensiva", 
      dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$RegioneO,]$terapia_intensiva
      , icon = icon("bed"),
      color = "red", fill = TRUE
    )
  })
  
  output$ospedalizati <- renderInfoBox({
    infoBox(
      "Ospedalizati", 
      dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$RegioneO,]$ricoverati_con_sintomi
      , icon = icon("briefcase-medical"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$dimessi_guariti <- renderInfoBox({
    infoBox(
      "Dimessi Guariti", 
      dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$RegioneO,]$dimessi_guariti-
        dta_regionale_ieri[dta_regionale_ieri$denominazione_regione==input$RegioneO,]$dimessi_guariti 
      , icon = icon("user-check"),
      color = "green", fill = TRUE
    )
  })
  
  output$torta_ospedalizati_regionale <- renderPlotly({
    ospedale_pie <- plot_ly(type='pie', labels=labels_pie, values=c(dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$RegioneO,]$terapia_intensiva,
                                                                    dta_regionale_aggioranti[dta_regionale_aggioranti$denominazione_regione==input$RegioneO,]$ricoverati_con_sintomi), 
                            textinfo='percent',
                            insidetextorientation='radial',
                            marker= list(colors = c("#d93030", "#ffa40c")),
                            height = 425)
  })
  
  output$grafico_storico_ospedale_regionale <- renderPlotly({
  
    
    
    plot_ly(dta_regionale, type = 'scatter', mode = 'lines')%>%
      filter(denominazione_regione %in% input$Regione) %>%
      group_by(denominazione_regione) %>%
      add_trace(x = ~data, y = ~terapia_intensiva,name = 'Terapia Intensiva', type = 'scatter', mode = 'lines',
                 line = list(color = '#d93030', width = 4)) %>%
      add_trace(x = ~data, y = ~ricoverati_con_sintomi,name = 'Ospedalizati', 
                 line = list(color = '#ffa40c', width = 4))%>% 
      layout(xaxis = list(title = "Mesi"))
  })
  
  output$mappa <- renderLeaflet({
    
    s <- colorNumeric("magma",domain =dta_mappa$totale_positivi ,n = 3, reverse = T)
    state_popup <- paste0("<strong>Regione: </strong>", 
                          states$reg_name, 
                          "<br><strong>Positivi: </strong>", 
                          dta_mappa$totale_positivi)
    
    
    leaflet(dta_mappa)%>%
      addTiles()%>%
      addPolygons(data = states,
                  fillColor = s(dta_mappa$totale_positivi),
                  highlight = highlightOptions(weight = 0.5,
                                               fillOpacity = 10,
                                               bringToFront = F),
                  
                  label= states$reg_name,
                  color = "greed",
                  popup = state_popup)%>%
      
      addLegend("bottomright", pal = s, values = dta_mappa$totale_positivi,
                title = "Positivi",
                labFormat = labelFormat(prefix = "casi"),
                bins = 3,
                opacity = 1
      )
  })
  
  
}

shinyApp(ui, server)




