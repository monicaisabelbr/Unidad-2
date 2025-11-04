library(shiny)
source("R/Stats.R", local = TRUE)

ui <- fluidPage(
  titlePanel("Simulación con Semillas Separadas"),
  sidebarLayout(
    sidebarPanel(
      h4("Semilla para Población"),
      selectInput("tipo_semilla_pob", "Tipo semilla población:",
                  choices = c("Manual" = "manual", "Automática" = "auto"),
                  selected = "manual"),
      conditionalPanel(
        condition = "input.tipo_semilla_pob == 'manual'",
        numericInput("seed_poblacion", "Semilla población", 123, min = 1, max = 100000)
      ),
      
      h4("Semilla para Muestra"),
      selectInput("tipo_semilla_mue", "Tipo semilla muestra:",
                  choices = c("Manual" = "manual", "Automática" = "auto"),
                  selected = "manual"),
      conditionalPanel(
        condition = "input.tipo_semilla_mue == 'manual'",
        numericInput("seed_muestra", "Semilla muestra", 456, min = 1, max = 100000)
      ),
      
      hr(),
      numericInput("n_poblacion", "Tamaño población", 1000, min = 100, max = 10000),
      numericInput("p_real", "Probabilidad", 0.5, min = 0.1, max = 0.9, step = 0.1),
      numericInput("n_muestra", "Tamaño muestra", 100, min = 10, max = 500),
      
      actionButton("generar_pob", "Generar Solo Población"),
      actionButton("generar_mue", "Generar Muestra de Población Actual"),
      actionButton("generar_todo", "Generar Población y Muestra")
    ),
    mainPanel(
      h4("Resultados"),
      verbatimTextOutput("info_semillas"),
      verbatimTextOutput("resultado"),
      tableOutput("tabla"),
      verbatimTextOutput("estadisticas")
    )
  )
)

server <- function(input, output) {
  
  # Valores reactivos para almacenar
  poblacion_actual <- reactiveVal(NULL)
  semilla_pob_usada <- reactiveVal(123)
  semilla_mue_usada <- reactiveVal(456)
  
  # 1. GENERAR SOLO POBLACIÓN
  observeEvent(input$generar_pob, {
    # Determinar semilla para población
    if(input$tipo_semilla_pob == "manual") {
      semilla_pob <- input$seed_poblacion
    } else {
      semilla_pob <- sample(1:100000, 1)
    }
    
    semilla_pob_usada(semilla_pob)
    set.seed(semilla_pob)
    
    # Generar población
    poblacion <- generar_poblacion(input$n_poblacion, input$p_real)
    poblacion_actual(poblacion)
    
    output$info_semillas <- renderPrint({
      cat("=== SEMILLAS USADAS ===\n")
      cat("Población:", semilla_pob, "(Generada ahora)\n")
      cat("Muestra:", semilla_mue_usada(), "(Última usada)\n")
    })
    
    output$resultado <- renderPrint({
      p_poblacion <- calcular_proporcion(poblacion)
      cat("=== POBLACIÓN GENERADA ===\n")
      cat("Semilla población:", semilla_pob, "\n")
      cat("Tamaño población:", input$n_poblacion, "\n")
      cat("Proporción poblacional:", round(p_poblacion, 4), "\n")
      cat("Probabilidad teórica:", input$p_real, "\n")
      cat("Diferencia:", round(p_poblacion - input$p_real, 4), "\n")
    })
    
    output$tabla <- renderTable(NULL)  # Limpiar tabla de muestra
    output$estadisticas <- renderPrint(NULL)
  })
  
  # 2. GENERAR MUESTRA DE POBLACIÓN ACTUAL
  observeEvent(input$generar_mue, {
    if(is.null(poblacion_actual())) {
      showNotification("Primero genera una población!", type = "warning")
      return()
    }
    
    # Determinar semilla para muestra
    if(input$tipo_semilla_mue == "manual") {
      semilla_mue <- input$seed_muestra
    } else {
      semilla_mue <- sample(1:100000, 1)
    }
    
    semilla_mue_usada(semilla_mue)
    set.seed(semilla_mue)
    
    # Generar muestra
    muestra <- tomar_muestra(poblacion_actual(), input$n_muestra)
    p_poblacion <- calcular_proporcion(poblacion_actual())
    p_muestral <- calcular_proporcion(muestra)
    
    output$info_semillas <- renderPrint({
      cat("=== SEMILLAS USADAS ===\n")
      cat("Población:", semilla_pob_usada(), "(Previa)\n")
      cat("Muestra:", semilla_mue, "(Generada ahora)\n")
    })
    
    output$resultado <- renderPrint({
      cat("=== MUESTRA GENERADA ===\n")
      cat("Semilla muestra:", semilla_mue, "\n")
      cat("Proporción poblacional:", round(p_poblacion, 4), "\n")
      cat("Proporción muestral:", round(p_muestral, 4), "\n")
      cat("Diferencia:", round(p_muestral - p_poblacion, 4), "\n")
      cat("Tamaño muestra:", input$n_muestra, "\n")
    })
    
    output$tabla <- renderTable({
      data.frame(
        Categoria = c("0", "1", "Total"),
        Conteo = c(sum(muestra == 0), sum(muestra == 1), length(muestra)),
        Proporcion = c(
          round(sum(muestra == 0)/length(muestra), 4),
          round(sum(muestra == 1)/length(muestra), 4),
          1.0000
        )
      )
    })
    
    output$estadisticas <- renderPrint({
      error <- abs(p_muestral - p_poblacion)
      error_relativo <- error / p_poblacion * 100
      cat("=== ESTADÍSTICAS ===\n")
      cat("Error absoluto:", round(error, 4), "\n")
      cat("Error relativo:", round(error_relativo, 2), "%\n")
      cat("Precisión:", round(1 - error, 4), "\n")
    })
  })
  
  # 3. GENERAR POBLACIÓN Y MUESTRA JUNTAS
  observeEvent(input$generar_todo, {
    # Semilla población
    if(input$tipo_semilla_pob == "manual") {
      semilla_pob <- input$seed_poblacion
    } else {
      semilla_pob <- sample(1:100000, 1)
    }
    
    # Semilla muestra
    if(input$tipo_semilla_mue == "manual") {
      semilla_mue <- input$seed_muestra
    } else {
      semilla_mue <- sample(1:100000, 1)
    }
    
    semilla_pob_usada(semilla_pob)
    semilla_mue_usada(semilla_mue)
    
    # Generar población
    set.seed(semilla_pob)
    poblacion <- generar_poblacion(input$n_poblacion, input$p_real)
    poblacion_actual(poblacion)
    
    # Generar muestra
    set.seed(semilla_mue)
    muestra <- tomar_muestra(poblacion, input$n_muestra)
    
    p_poblacion <- calcular_proporcion(poblacion)
    p_muestral <- calcular_proporcion(muestra)
    
    output$info_semillas <- renderPrint({
      cat("=== SEMILLAS USADAS ===\n")
      cat("Población:", semilla_pob, "\n")
      cat("Muestra:", semilla_mue, "\n")
    })
    
    output$resultado <- renderPrint({
      cat("=== POBLACIÓN Y MUESTRA ===\n")
      cat("Proporción poblacional:", round(p_poblacion, 4), "\n")
      cat("Proporción muestral:", round(p_muestral, 4), "\n")
      cat("Diferencia:", round(p_muestral - p_poblacion, 4), "\n")
    })
    
    output$tabla <- renderTable({
      data.frame(
        Categoria = c("0", "1", "Total"),
        Conteo = c(sum(muestra == 0), sum(muestra == 1), length(muestra))
      )
    })
    
    output$estadisticas <- renderPrint({
      error <- abs(p_muestral - p_poblacion)
      error_relativo <- error / p_poblacion * 100
      cat("=== ESTADÍSTICAS ===\n")
      cat("Error absoluto:", round(error, 4), "\n")
      cat("Error relativo:", round(error_relativo, 2), "%\n")
    })
  })
}

shinyApp(ui, server)