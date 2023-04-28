# desenha um gráfico de barras intereativo com o número de observações por UC
plotar_n_obs_uc_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data/n_obs_uc.rds'
      )
    )
) {
  # desenha o gráfico com mais de 1000 observações
  fig <- 
    plotly::ggplotly(dados |>
                       dplyr::mutate(
                         n_obs = dplyr::case_when(
                           n %in% 1:100 ~ "Até 100 observações",
                           n %in% 101:500 ~ "Entre 101 e 500 observações",
                           n %in% 501:1000 ~ "Entre 501 e 1000 observações",
                           n > 1000 ~ "Mais de 1000 observações"
                         ),
                         uc_name_abv = forcats::fct_reorder(
                           uc_name_abv,
                           dplyr::desc(n)
                         ),
                         n_obs = forcats::fct_reorder(
                           n_obs,
                           dplyr::desc(n)
                         )
                       ) |> 
                       ggplot2::ggplot() +
                       ggplot2::aes(x = uc_name_abv,
                                    y = n,
                                    label = uc_name) +
                       ggplot2::geom_col(fill = "chartreuse4") +
                       ggplot2::labs(x = "Unidades de Conservação",
                                     y = "Número de observações") +
                       ggplot2::facet_wrap(
                         facets = vars(
                           n_obs
                         ), 
                         nrow = 4, 
                         scales = "free"
                       ) +
                       ggplot2::theme_minimal(14)
    )
  
  # retronar os gráficos
  return(fig)
}

