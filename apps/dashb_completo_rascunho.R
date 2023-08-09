library(distanceMonitoraflorestal)
library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
#source("R/minhas_funcoes.R") 

dados <- monitora_aves_masto_florestal

ui <- dashboardPage(
  dashboardHeader(title = "Densidades Populacionais"),
  dashboardSidebar( # gera a barra lateral
    sidebarMenu( # define o menu da barra lateral
      menuItem( # define um item do menu
        text = "Densidade Populacional",
        tabName = "sobre",
        icon = icon("info")
      ),
      menuItem(
        text = "Exploração dos dados",
        menuSubItem(# defini  um subitem na barra adicional
          text = "Informações gerais", 
          tabName = "exploraca_dados"
        ),
        menuSubItem(# defini  um subitem na barra adicional
          text = "Etapa 1", 
          tabName = "exploraca_dados"
        ),
        menuSubItem(# defini  um subitem na barra adicional
          text = "Etapa 2", 
          tabName = "exploraca_dados"
        ),
        tabName = "exploraca_dados",
        icon = icon("eye")
      ),
      menuItem(
        text = "Selecionar espécie",
        menuSubItem(
          text = "Fluxo 1",
          tabName = "regiao_sao_paulo"
        ),
        menuSubItem(
          text = "Fluxo 2",
          tabName = "regiao_grande_sp"
        ),
        menuSubItem(
          text = "Fluxo 3",
          tabName = "regiao_santos"
        )
      )
    )
  ),
  dashboardBody(
    tabItems( # define um subitem na barra lateral
      tabItem(
        tabName = "sobre",
        titlePanel("Monitora Componente Florestal"),
        includeMarkdown("texto_sobre.md")
      ),
      tabItem(
        tabName = "exploraca_dados",
        titlePanel("Exploração dos dados"),
        p("Texto sobre os graficos"),
        fluidRow(
          valueBoxOutput(outputId = "num_regioes", width = 4),
          valueBoxOutput(outputId = "num_cidades", width = 4),
          valueBoxOutput(outputId = "num_delegacias", width = 4)
        ),
        fluidRow(
          tabBox(
            title = "Destaques",
            width = 12,
            tabPanel(
              title = "Distâncias perpendiculares",
              shiny::selectInput(
                inputId = "ucs",
                selected = "Resex Tapajós-Arapiuns",
                label = "UC's", 
                choices = unique(dados$nome_uc) 
              ),
              # caixa de seleção das espeçies
              shiny::selectInput(
                inputId = "especie",
                selected = "Dasyprocta croconota",
                label = "Espécie", 
                choices = unique(dados$nome_sp)
              ),
              
              # caixa de selecao da largura das barras
              shiny::sliderInput(
                inputId = "largura_barra",
                min = 1,
                max = 10, 
                value = 1, 
                label = "Largura das barras"
              ),
            plotly::plotlyOutput(
                outputId = "coluna"
              ),
              DT::DTOutput(
                outputId = "tabela"
              )
            ),
            tabPanel(
              title = "Indivíduos por espécie"
            ),
            tabPanel(
              title = "Espécies por UC"
            )
          )
        )
      ),
      tabItem(
        tabName = "regiao_sao_paulo",
        titlePanel("Distâncias exatas com repetições"),
        fluidRow(
          box(
            title = "Filtros",
            width = 12,
            fluidRow(
              column(
                width = 4
                # ,
                # selectInput(
                #   inputId = "delegacia_sp",
                #   label = "Selecione uma delegacia",
                #   choices = dados |>
                #     filter(regiao_nome == "Capital") |>
                #     distinct(delegacia_nome) |>
                #     pull()
                # )
              ),
              column(
                width = 4,
                # selectInput(
                #   inputId = "ocorrencia_sp",
                #   label = "Selecione uma ocorrência",
                #   choices = dados |>
                #     select(estupro:vit_latrocinio) |>
                #     names()
                # )
              )
            )
          )
        ),
        plotOutput("serie_sp")
      ),
      tabItem(
        tabName = "regiao_grande_sp",
        titlePanel("Distâncias exatas sem repetições"),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                width = 4
                # ,
                # selectInput(
                #   inputId = "municipio_grande_sp",
                #   label = "Selecione um município",
                #   choices = dados |>
                #     filter(regiao_nome == "Grande São Paulo") |>
                #     distinct(municipio_nome) |>
                #     pull()
                # )
              ),
              column(
                width = 4,
                uiOutput("filtro_delegacia_grande_sp")
              ),
              column(
                width = 4
                # ,
                # selectInput(
                #   inputId = "ocorrencia_grande_sp",
                #   label = "Selecione uma ocorrência",
                #   choices = dados |>
                #     select(estupro:vit_latrocinio) |>
                #     names()
                # )
              )
            )
          )
        ),
        )
    )
  )
)

server <- function(input, output, session) {
  
  # output$num_regioes <- renderValueBox({
  #   num_regioes <- n_distinct(dados$regiao_nome)
  #   valueBox(
  #     value = num_regioes,
  #     subtitle = "Número de regiões",
  #     icon = icon("map"),
  #     color = "indigo"
  #   )
  # })
  # 
  # output$num_cidades <- renderValueBox({
  #   num_cidades <- n_distinct(dados$municipio_nome)
  #   valueBox(
  #     value = num_cidades,
  #     subtitle = "Número de cidades",
  #     icon = icon("city"),
  #     color = "purple"
  #   )
  # })
  # 
  # 
  # output$num_delegacias <- renderValueBox({
  #   num_delegacias <- n_distinct(dados$delegacia_nome)
  #   valueBox(
  #     value = num_delegacias,
  #     subtitle = "Número de delegacias",
  #     icon = icon("shield-alt"),
  #     color = "fuchsia",
  #     href = "http://www.dados.sp.gov.br/"
  #   )
  # })
  # 
  # output$grafico_roubo_carros <- renderPlot({
  #   dados |>
  #     filter(ano >= 2018) |>
  #     group_by(mes, ano, regiao_nome) |>
  #     summarise(
  #       total_roubo_carro = sum(roubo_veiculo)
  #     ) |>
  #     mutate(
  #       data = lubridate::make_date(
  #         year = ano,
  #         month = mes,
  #         day = 1
  #       )
  #     ) |>
  #     ggplot(aes(x = data, y = total_roubo_carro)) +
  #     geom_col(color = "black", fill = "royalblue") +
  #     facet_wrap(vars(regiao_nome), nrow = 3)
  # })
  # 
  # 
  # 
  # output$serie_sp <- renderPlot({
  #   dados |>
  #     filter(
  #       municipio_nome == "São Paulo",
  #       delegacia_nome == input$delegacia_sp
  #     ) |>
  #     mutate(
  #       data = lubridate::make_date(
  #         year = ano,
  #         month = mes,
  #         day = 1
  #       )
  #     ) |>
  #     ggplot(aes(x = data, y = .data[[input$ocorrencia_sp]])) +
  #     geom_col(color = "black", fill = "royalblue")
  # })
  # 
  # output$filtro_delegacia_grande_sp <- renderUI({
  #   selectInput(
  #     inputId = "delegacia_grande_sp",
  #     label = "Selecione uma delegacia",
  #     choices = dados |>
  #       filter(municipio_nome == input$municipio_grande_sp) |>
  #       distinct(delegacia_nome) |>
  #       pull()
  #   )
  # })
  # 
  # output$serie_grande_sp <- renderPlot({
  #   dados |>
  #     filter(
  #       municipio_nome == input$municipio_grande_sp,
  #       delegacia_nome == input$delegacia_grande_sp
  #     ) |>
  #     mutate(
  #       data = lubridate::make_date(
  #         year = ano,
  #         month = mes,
  #         day = 1
  #       )
  #     ) |>
  #     ggplot(aes(x = data, y = .data[[input$ocorrencia_grande_sp]])) +
  #     geom_col(color = "black", fill = "royalblue")
  # })
  # 

  # gera reatividade 
  # gerar os dados para desenhar os gráficos
  dados_filtrados <- reactive({
    dados |> 
      dplyr::filter(nome_uc == input$ucs,
                    nome_sp == input$especie,
                    !is.na(distancia)) |> 
      transformar_dados_formato_Distance()
  })
  
  dados_selecionados <- reactive({
    dados |>
      dplyr::filter(nome_uc == input$ucs,
                    nome_sp == input$especie,
                    !is.na(distancia))
  })
  
  largura_barra <- reactive({
    input$largura_barra
  })
  
  output$coluna <- plotly::renderPlotly({
    # imprime mensagem
    print("Gerando gráficos...")
    
    # gerar gráficos
    fig <- plotar_distribuicao_distancia_interativo(
      dados_filtrados(),
      largura_caixa = largura_barra()
    )
    
    # retornar graficos
    fig
    
  })
  
  # imprimir mensagem
  print("Gerando tabela...")
  
  # adicionar tabela interativa
  output$tabela = DT::renderDataTable({
    dados_selecionados() |> 
      dplyr::filter(nome_uc == input$ucs,
                    nome_sp == input$especie,
                    !is.na(distancia)) |> 
      DT::datatable(
        filter = list(
          position = "top"
        )
      )
  })
  
  
}

shinyApp(ui, server, options = list(launch.browser = FALSE, port = 3989))
