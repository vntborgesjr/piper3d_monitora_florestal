# desenha um gráfico de barras interativo com o número de observações por espécie e por UC
plotar_n_obs_sp_uc_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc.rds"
      )
    )
    ) {
  # carregar pacote necessário
  #library(patchwork)
  
  # desenha o gráfico com mais de 1000 observações
  mais_mil_obs <- dados |>  
    dplyr::filter(n %in% 1001:2497) |> 
    dplyr::mutate(
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = sp_name_abv, # substituir por sp_abv
                 y = n,
                 fill = uc_name_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # transforma em grafico interativo
  mais_mil_obs <- plotly::ggplotly(mais_mil_obs)
  
  # desenha o gráfico com 501 a 1000 observações
  quinhentos_mil_obs <- dados  |>  
    dplyr::filter(n %in% 501:1000) |> 
    dplyr::mutate(
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = sp_name_abv, # substituir por sp_abv
                 y = n,
                 fill = uc_name_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # transforma em gráfico interativo
  quinhentos_mil_obs <- plotly::ggplotly(quinhentos_mil_obs)
  
  # desenha o gráfico com 101 a 500 observações
  cem_quintas_obs <- dados  |>  
    dplyr::filter(n %in% 101:500) |> 
    dplyr::mutate(
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = sp_name_abv, # substituir por sp_abv
                 y = n,
                 fill = uc_name_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # tansforma em gráfico interativo
  cem_quintas_obs <- plotly::ggplotly(cem_quintas_obs)
  
  # desenha o gráfico com menos de 100 observações
  uma_cem_obs <- dados  |>  
    dplyr::filter(n < 100) |> 
    dplyr::mutate(
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = sp_name_abv, # substituir por sp_abv
                 y = n,
                 fill = uc_name_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
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
    quinhentos_mil_obs,
    cem_quintas_obs,
    uma_cem_obs,
    nrows = 4,
    titleX = FALSE,
    titleY = TRUE,
    shareX = FALSE,
    shareY = TRUE
  )
  
  # retronar os gráficos
  return(fig)
}
