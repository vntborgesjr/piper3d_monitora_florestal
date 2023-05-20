#
# Essa aplicação Shiny gera três gráficos diferentes para visualização 
# da dsitribuição das distâncias perpendiculares
# para rodar a plicação precione o botão acima 'Run App'.
#
# carregar funções
source("R/minhas_funcoes.R") 

# carregar dados para o R
dados_selecionados <- carregar_dados_selecionados() 

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  
  # Application title
  shiny::titlePanel("Distância perpendicular por unidade de conseração e por espécie"),
  
  # Sidebar with a slider input for number of bins 
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # caixa de seleção da região
      
      
      # caixa de seleção do estado
      
      
      # caixa de seleção das UCs
      shiny::selectInput(
        inputId = "ucs",
        selected = "Resex Tapajós-Arapiuns",
        label = "UC's", 
        choices = unique(dados_selecionados$uc_name) 
      ),
      # caixa de seleção das espeçies
      shiny::selectInput(
        inputId = "especie",
        selected = "Dasyprocta croconota",
        label = "Espécie", 
        choices = unique(dados_selecionados$sp_name)
      )
    ),
    
    # Mostrar grafico de coluna
    shiny::mainPanel(
      plotly::plotlyOutput(
        outputId = "coluna"
      ),
      DT::DTOutput(
        outputId = "tabela"
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # gera reatividade 
  # gerar os dados para desenhar os gráficos
  dados_filtrados <- reactive({
    dados_selecionados |> 
      dplyr::filter(uc_name == input$ucs,
                    sp_name == input$especie,
                    !is.na(distance))
  })
  
  output$coluna <- plotly::renderPlotly({
    # imprime mensagem
    print("Gerando gráficos...")
    
    # gerar gráficos
    fig <- plotar_distribuicao_distancia_interativo(dados_filtrados())
    
    # retornar graficos
    fig
    
  })
  
  # imprimir mensagem
  print("Gerando tabela...")
  
  # adicionar tabela interativa
  output$tabela = DT::renderDataTable({
    dados_selecionados |> 
      dplyr::filter(uc_name == input$ucs,
                    sp_name == input$especie,
                    !is.na(distance)) |> 
      DT::datatable(
        filter = list(
          position = "top"
        )
      )
  })
  
}

# Run the application 
shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE, port = 5261))