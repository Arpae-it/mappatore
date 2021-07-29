library(shiny)
library(leaflet)
library(leaflet.extras)

library(sp)

# Define UI for data download app ----
options(shiny.maxRequestSize=60*1024^2)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Mappe"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width=2,
      
      # Input: Choose dataset ----
     
      
      
      sliderInput("TOSM","Trasparenza OSM",min=0,max=1,step=0.1,value=0.30),
      fileInput("file","Carica un kml"),
      
      actionButton("button", "Cancella tutti i kml"),
      numericInput("xcoo","X da wgs84 a utm32",0),
      numericInput("ycoo","Y da wgs84 a utm32",0)
     
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width=10,
      
      leafletOutput("gvis",height=800),
      textOutput("coordinate")
      
      
    )
    
  )
)

# Define server logic to display and download selected file ----

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
server <- function(input, output,session) {
  
  observeEvent(input$button, {
    leafletProxy("gvis", session) %>%
      removeMarker("gvis","gino")
    
    
  })
  observe({
    click = input$gvis_click
    print(click$lat)
    print(click$lng)
    updateNumericInput(session,"xcoo",value=click$lng)
    updateNumericInput(session,"ycoo",value=click$lat)})
  
  observe({
    if (is.null(input$file)) return()
    file.remove(dir(  
      "www", 
      pattern = "*.*", 
      full.names = TRUE
    ))
    file.copy(input$file$datapath, "www/carico.kml")
    kml<-read_file("www/carico.kml")
    leafletProxy("gvis", session) %>%
      addKML(kml,layerId="gino")
    })
  # Table of selected dataset ----
  output$coordinate<-renderText({
    validate(
      need (input$xcoo,"Qui puoi convertire coordinate inserendo x e y"),
      need (input$ycoo,"Qui puoi convertire coordinate inserendo x e y")
    )
    coordino<-LongLatToUTM(input$xcoo,input$ycoo,32)
    
    paste0("x=",unlist(coordino)[2],"-y=",unlist(coordino)[3])
    }
  )
  output$gvis <- renderLeaflet({
    
    
    
     p<- leaflet()  %>%
    #  leafletOptions(maxZoom=20)%>%
        flyToBounds("gvis",lng1=12, lat1=44,lng2=13,lat2=45)%>%
        addTiles(options=tileOptions(opacity=input$TOSM),group="OSM")%>%
      addProviderTiles("Esri.WorldImagery",group="ESRI") %>%
              addWMSTiles(
          'http://servizigis.regione.emilia-romagna.it/wms/agea2011_rgb?',
          layers = 'Agea2011_RGB', group='Agea2011_RGB',
          options = WMSTileOptions(format = "image/png", transparent = TRUE) 
        ) %>%
        addWMSTiles(
          'http://servizigis.regione.emilia-romagna.it/wms/agea2011_ir',
          layers = 'Agea2011_IR', group='Agea2011_IR',
          options = WMSTileOptions(format = "image/png", transparent = TRUE) 
        ) %>%  
        addWMSTiles(
          'http://servizigis.regione.emilia-romagna.it/wms/agea2008_rgb?',
          layers = 'Agea2008_RGB', group='Agea2008_RGB',
          options = WMSTileOptions(format = "image/png", transparent = TRUE) 
        ) %>% 
        addWMSTiles(
          'http://servizigis.regione.emilia-romagna.it/wms/agea2008_ir?',
          layers = 'Agea2008_IR', group='Agea2008_IR',
          options = WMSTileOptions(format = "image/png", transparent = TRUE) 
        ) %>% 
        addWMSTiles(
          'http://servizigis.regione.emilia-romagna.it/wms/carta_storica_regionale_1853',
          layers = 'carta_storica_regionale_1853', group='carta_storica_regionale_1853',
          options = WMSTileOptions(format = "image/png", transparent = TRUE) 
        ) %>% 
        addWMSTiles(
          'http://servizigis.regione.emilia-romagna.it/wms/dbtr_ctrmultiscala',
          layers = 'DBTR_CtrMultiscala', group='DBTR_CtrMultiscala',
          options = WMSTileOptions(format = "image/png", transparent = TRUE) 
        ) %>% 
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/sfumo_altimetrico5_bosco?',
        layers = 'Sfumo_Altimetrico5_Bosco', group='Sfumo_Altimetrico5_Bosco',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/rete_escursionistica',
        layers = 'Percorso_escursionistico', group='Percorso_escursionistico',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/uso_del_suolo?',
        layers = '2008_uso_suolo_ed2011', group='2008_uso_suolo_ed2011',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/uso_del_suolo?',
        layers = '2003_uso_suolo_ed2011', group='2003_uso_suolo_ed2011',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/uso_del_suolo?',
        layers = '1994_uso_suolo_ed2015', group='1994_uso_suolo_ed2015',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
     
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/uso_del_suolo?',
        layers = '1976_uso_suolo_ed2011', group='1976_uso_suolo_ed2011',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/uso_del_suolo?',
        layers = '1853_uso_suolo_storico_poligoni', group='1853_uso_suolo_storico_poligoni',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
        
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/ambiti_amministrativi?',
        layers = 'Regione_Ed_2018', group='Regione_Ed_2018',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/ambiti_amministrativi?',
        layers = 'Provincia_Ed_2018', group='Provincia_Ed_2018',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
      addWMSTiles(
        'http://servizigis.regione.emilia-romagna.it/wms/ambiti_amministrativi?',
        layers = 'Comune_Ed_2018_250k', group='Comune_Ed_2018_25k',
        options = WMSTileOptions(format = "image/png", transparent = TRUE,opacity=30) 
      ) %>%
               addSearchOSM() %>% addControlGPS(options=gpsOptions(position = "topleft", activate = TRUE, 
                                                                  autoCenter = TRUE, maxZoom = 17, 
                                                                  setView = TRUE)) %>%
       
        addReverseSearchOSM(showSearchLocation = TRUE, showBounds = FALSE,showFeature = TRUE, fitBounds = TRUE, displayText = TRUE,group = "OSMA")%>%
        addMeasure(primaryLengthUnit="meters",secondaryLengthUnit="meters",primaryAreaUnit="sqmeters",secondaryAreaUnit="hectares")%>%
      addDrawToolbar(targetGroup = "draw",editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
                addStyleEditor() %>%   
      addLayersControl(baseGroups=c('ESRI','OSM','Agea2011_RGB','Agea2011_IR','Agea2008_RGB','Agea2008_IR',"Sfumo_Altimetrico5_Bosco","2008_uso_suolo_ed2011","2003_uso_suolo_ed2011","1994_uso_suolo_ed2015","1976_uso_suolo_ed2011","1853_uso_suolo_storico_poligoni","carta_storica_regionale_1853","Percorso_escursionistico","DBTR_CtrMultiscala","ORTO 1988","ORTO 1994"),
                       overlayGroups=c("draw","DBTR_CtrMultiscala","OSM","Percorso_escursionistico","Comune_Ed_2018_25k","Provincia_Ed_2018","Regione_Ed_2018","OSMA","carta_storica_regionale_1853"))
        
    p
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
