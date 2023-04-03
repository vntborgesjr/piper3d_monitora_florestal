#
# Essa aplicação Shiny gera três gráficos diferentes para visualização 
# da dsitribuição das distâncias perpendiculares
# para rodar a plicação precione o botão acima 'Run App'.
#
# carregar funções
source("R/carregar_dados_completos.R") 
source("R/grafico_exploratorio.R") 

# carregar dados para o R
dados_completos <- carregar_dados1(
  dados = "data-raw/dados-brutos.xlsx" 
  ) 

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  
  # Application title
  shiny::titlePanel("Distância perpendicular por unidade de conseração e por espécie"),
  
  # Sidebar with a slider input for number of bins 
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # caixa de seleção das UCs
      shiny::selectInput(
        inputId = "ucs",
        label = "UC's", 
        choices = dados_completos$uc_name # ordenar por estado e região
      ),
      # caixa de seleção das espeçies
      shiny::selectInput(
        inputId = "especie",
        label = "Espécie", 
        choices = dados_completos$sp
      )
    ),
    
    # Mostrar grafico de coluna
    shiny::mainPanel(
      shiny::plotOutput(
        outputId = "coluna"
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$coluna <- shiny::renderPlot({
    # imprime mensagem
    print("Gerando gráficos...")
    
    # gera reatividade 
    # gerar os dados para desenhar os gráficos
    dados_filtrados <- dados_completos |> 
      dplyr::filter(uc_name == input$ucs,
                    sp == input$especie,
                    !is.na(distance))
    
    # gerar gráficos
    fig <- grafico_exploratorio1(dados_filtrados)
    
    # retornar graficos
    fig
    
  })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)