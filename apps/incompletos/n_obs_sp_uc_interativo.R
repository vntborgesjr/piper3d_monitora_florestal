#
# Transformar n_obs_sp_interativo.R adicionando n_obs_uc_interativo.R
# A ui deve ser mais estreita e contetr dois paineis de seleção,
# um para espécie e outro para Ucs. Os graficos serão menores, um sobre o outro
# e cada painel conterá duas abas, uma para os gráficos e outra para a tabela.
####################################################
############### FAZER QUANDO DER!!!! ###############
####################################################
#n_obs_sp_uc_interativo.R
# carregar funções
source("R/carregar_dados_selecionados.R")
source("R/contar_n_obs_sp.R")
source("R/grafico_n_obs_sp_shiny.R") 

#n_obs_uc <- contar_n_obs_sp()

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  
  # Application title
  shiny::titlePanel("Número de total observações por espécie"),
  
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
        max = 3497,
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
  n_obs_sp_filtrado <- reactive({
    dados_selecionados |>  
      contar_n_obs_sp() |> 
      dplyr::filter( 
        n %in% filtro()[1]:filtro()[2] 
      )
  })
  
  output$plot_n_obs_uc <- plotly::renderPlotly({
    # imprime mensagem
    print("Gerando gráficos...")
    
    # gerar gráficos
    fig <- n_obs_sp_filtrado() |> 
      grafico_n_obs_sp_shiny()
    
    # retornar graficos
    fig
    
  })
  
  # imprimir mensagem
  print("Gerando tabela...")
  
  # adicionar tabela interativa
  output$tabela = DT::renderDataTable({
    n_obs_sp_filtrado() |> 
      DT::datatable(
        filter = list(
          position = "top"
        )
      )
  })
  
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
