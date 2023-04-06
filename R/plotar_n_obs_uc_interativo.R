# Descrição

plotar_n_obs_uc_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_selecionados.rds'
      )
    )
) {
  # carregar pacote necessário
  #library(patchwork)
  
  # desenha o gráfico com mais de 1000 observações
  mais_mil_obs <- dados  |>  
    dplyr::count(uc_name_abv) |> 
    dplyr::filter(n > 1000) |> 
    mutate(
      uc_name_abv = forcats::fct_reorder(
        uc_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    ggplot2::labs(y = "Número de observações") +
    ggplot2::theme_minimal(14)
  
  # transforma em grafico interativo
  mais_mil_obs <- plotly::ggplotly(mais_mil_obs)
  
  # desenha o gráfico com 501 a 1000 observações
  quinehtos_mil_obs <- dados  |>  
    dplyr::count(uc_name_abv) |> 
    dplyr::filter(n %in% 501:1000) |> 
    mutate(
      uc_name_abv = forcats::fct_reorder(
        uc_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    ggplot2::labs(y = "Número de observações") +
    ggplot2::theme_minimal(14)
  
  # transforma em gráfico interativo
  quinehtos_mil_obs <- plotly::ggplotly(quinehtos_mil_obs)
  
  # desenha o gráfico com 101 a 500 observações
  cem_quintas_obs <- dados  |>  
    dplyr::count(uc_name_abv) |> 
    dplyr::filter(n %in% 501:1000) |> 
    mutate(
      uc_name_abv = forcats::fct_reorder(
        uc_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    ggplot2::labs(y = "Número de observações") +
    ggplot2::theme_minimal(14)
  
  # tansforma em gráfico interativo
  cem_quintas_obs <- plotly::ggplotly(cem_quintas_obs)
  
  # desenha o gráfico com menos de 100 observações
  uma_cem_obs <- dados  |>  
    dplyr::count(uc_name_abv) |> 
    dplyr::filter(n %in% 501:1000) |> 
    mutate(
      uc_name_abv = forcats::fct_reorder(
        uc_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    ggplot2::labs(x = "Nomes das Uc's abreviados",
                  y = "Número de observações") +
    ggplot2::theme_minimal(14)
  
  # tansforma em gráfico interativo
  uma_cem_obs <- plotly::ggplotly(uma_cem_obs)
  
  # organizar os gráficos
 # fig <- ggpubr::ggarrange(
  #  hist,
   # box, 
    #pontos,
    #nrow = 3
  #)
  fig <-  plotly::subplot(
    mais_mil_obs, 
    quinehtos_mil_obs,
    cem_quintas_obs,
    uma_cem_obs,
    nrows = 4,
    titleX = TRUE,
    titleY = TRUE,
    shareX = TRUE,
    shareY = TRUE
  )
  
  # retronar os gráficos
  return(fig)
}

# Exemplo

# carregar dados
#cutia_tap_arap <- readr::read_rds("data/dados_filtrados.rds")

# gerar gráficos
#grafico_exploratorio_interativo(cutia_tap_arap)


# salvar em disco
#ggplot2::ggsave(
# "fig.tiff", 
#width = 15, 
#height = 15,
#units = "cm"
#)

