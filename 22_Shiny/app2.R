library(shiny)
source("R/Stats.R")

ui <- fluidPage(
  titlePanel("Simulación de Muestra Simple"),
  sidebarLayout(
    sidebarPanel(
      numericInput("seed", "Semilla", 123, min = 1, max = 1000),
      numericInput("n_poblacion", "Tamaño población", 1000, min = 100, max = 10000),
      numericInput("p_real", "Probabilidad", 0.5, min = 0.1, max = 0.9, step = 0.1),
      numericInput("n_muestra", "Tamaño muestra", 100, min = 10, max = 500),
      actionButton("generar", "Generar Muestra")
    ),
    mainPanel(
      h4("Resultados de la Muestra"),
      verbatimTextOutput("resultado"),
      tableOutput("tabla")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$generar, {
    # Generar población y muestra
    poblacion <- generar_poblacion(input$seed, input$n_poblacion, input$p_real)
    muestra <- tomar_muestra(poblacion, input$n_muestra)
    
    # Calcular proporción poblacional
    p_poblacion <- calcular_proporcion(poblacion)
    
    # Calcular proporción muestral
    p_muestral <- calcular_proporcion(muestra)
    # Mostrar resultados
    output$resultado <- renderPrint({
      cat("Probabilidad:", input$p_real, "\n")
      cat("Proporción poblacional:", round(p_poblacion, 4), "\n")
      cat("Proporción muestral:", round(p_muestral, 4), "\n")
      cat("Diferencia:", round(p_muestral - input$p_real, 4), "\n")
      cat("Tamaño muestra:", input$n_muestra, "\n")
    })
    output$tabla <- renderTable({
      data.frame(
        Categoria = c("0", "1", "Total"),
        Conteo = c(
          sum(muestra == 0),
          sum(muestra == 1), 
          length(muestra)
        )
      )
    })
  })
}

shinyApp(ui, server)