#
# Essa aplicação Shiny gera três gráficos diferentes para visualização 
# da dsitribuição das distâncias perpendiculares
# para rodar a plicação precione o botão acima 'Run App'.
#
# carregar funções
source("R/carregar_dados_selecionados.R")
source("R/contar_obs_UC.R")
source("R/grafico_n_obs_UC_shiny.R") 

#n_obs_uc <- contar_obs_UC()

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  
  # Application title
  shiny::titlePanel("Número de total observações por UC"),
  
  # Sidebar with a slider input for number of bins 
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # caixa de seleção da região
      
      
      # caixa de seleção do estado
      
      
      # slider para seleção do número de observações
      shiny::sliderInput(
        label = shiny::h3("Nuḿero de observações:"),
        inputId = "obs_sp", 
        min = 1, 
        max = 2047,
        value = c(501, 1000),
        step = 1
        ),
  ),
  # Mostrar grafico de coluna
  shiny::mainPanel(
    plotly::plotlyOutput(
      outputId = "plot_n_obs_uc"
    ),
    DT::DTOutput(
      outputId = "tabela"
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # carregar dados para o R
  dados_selecionados <- carregar_dados_selecionados()
  
  # gera reatividade na amplitude dae valores selecionados
  filtro <- shiny::reactive({ 
    input$obs_sp 
  })
  
  # gerar os dados para desenhar os gráficos
  n_obs_uc_filtrado <- reactive({
    dados_selecionados |>  
      contar_obs_UC() |> 
      dplyr::filter( 
        n %in% filtro()[1]:filtro()[2] 
      )
  })
  
  output$plot_n_obs_uc <- plotly::renderPlotly({
    # imprime mensagem
    print("Gerando gráficos...")
    
    # gerar gráficos
    fig <- n_obs_uc_filtrado() |> 
      grafico_n_obs_UC_shiny()
    
    # retornar graficos
    fig
    
  })
  
  # imprimir mensagem
  print("Gerando tabela...")
  
  # adicionar tabela interativa
  output$tabela = DT::renderDataTable({
    n_obs_uc_filtrado() |> 
      DT::datatable(
        filter = list(
          position = "top"
        )
      )
  })
  
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
    