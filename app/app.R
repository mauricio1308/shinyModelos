library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rsconnect)
library(markovchain)
library(plotly)
library(scales)
source("funciones.R")
# Define UI for application that draws a histogram
ui <- dashboardPage(skin="blue",
      dashboardHeader(title = "Álbum del mundial Catar 2022",
                                    titleWidth = 250),
      dashboardSidebar(width = 250,
                                     hr(),
                                     sidebarMenu(id="tabs",
                                                 menuItem("Fase II",  icon = icon("futbol"),
                                                          #menuSubItem("Tienda de recuerdos", tabName = "tienda", icon = icon("shopify")),
                                                          menuSubItem("Album", tabName = "album", icon = icon("globe"))
                                                 ),
                                                 menuItem("Fase III", tabName = "faseIII", icon=icon("trophy"))
                                     ),
                                     hr()
                    ),
                    dashboardBody(
                      
                      tabItems(
                        # Contenido pestaÃ±a Fase I
                        tabItem(tabName = "faseIII",
                                h2("Disponible próximamente.")
                        ),
                    # Contenido pestaña album
                    # Contenido pestaña propagacion vectores (FaseII)
                    tabItem(tabName = "album",
                            fluidRow(
                              box(title = "Controles", status="primary", collapsible = TRUE,
                  numericInput(inputId = "prob1",label="Probabilidad de desanimarse entre 
                                               [0,162) laminas ",value=0.01,step=0.01,max=1,min=0),
                  numericInput(inputId = "prob2",label="Probabilidad de desanimarse entre 
                                               [162,324) laminas ",value=0.015,step=0.01,max=1,min=0),
                  numericInput(inputId = "prob3",label="Probabilidad de desanimarse entre 
                                               [324,486) laminas ",value=0.035,step=0.01,max=1,min=0),
                  numericInput(inputId = "prob4",label="Probabilidad de desanimarse entre 
                                               [486,638) laminas ",value=0.02,step=0.01,max=1,min=0),
                  sliderInput(inputId = "monasIniciales",label="Ingrese el # de monas iniciales",
                                              min=5,max=637,value = 5,step=1),
                  actionButton("do","Guardar parámetros")
                              ),
                  box(
                    title = "Gráfica", status = "primary", collapsible = TRUE,
                    plotlyOutput("Grafica"),width=6
                  ),
                  box(title="Resultados",status="primary",collapsible=TRUE,
                      
                      infoBox("Costo asociado",value=textOutput("Costo"),fill=TRUE,width=12),
                      infoBox("Probabilidad de terminar el álbum",
                              value=textOutput("probTerminar"),fill=TRUE,width=12))
                 
                )))))
                

                            

# Define server logic required to draw a histogram
server <- function(input, output,session) {

    datos <- eventReactive(input$do,{
    data <- cadenita(input$prob1,input$prob2,input$prob3,input$prob4,input$monasIniciales)
  })
  output$Grafica <- renderPlotly({
    datos <- datos()[[1]]
    plot_ly(data=datos,x=~rangos,y=~probabilidad,type="bar",text=~probabilidad) %>%
      layout(title="Probabilidad de desanimarse según el rango",
             xaxis=list(title="Rango"),yaxis=list(title="Probabilidad (%)")
             )
  })
  output$Costo <- renderText({
    datos <- dollar(as.numeric(datos()[[2]]))
  })
  output$probTerminar <- renderText({
    if(datos()[[3]]<0.05){
      datos <- round(as.numeric(datos()[[3]]),10)
    }
    else{
      datos <- round(as.numeric(datos()[[3]]),3)
    }
    
    
    
  })
}


# Run the application 
#profvis::profvis(runApp(shinyApp(ui, server)))
shinyApp(ui = ui, server = server)
