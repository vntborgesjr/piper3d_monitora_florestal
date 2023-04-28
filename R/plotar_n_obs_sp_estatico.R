# Descrição
# desenha um gráfico de barras com o número de observações por espécie
plotar_n_obs_sp_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp.rds"
      )
    )
) {
  fig <- dados |> 
    dplyr::mutate(
      n_obs = dplyr::case_when(
        n > 1000 ~ "Mais de 1000 observações",
        n %in% 501:1000 ~ "Entre 501 e 1000 observações",
        n %in% 101:500 ~ "Entre 101 e 500 observações",
        n %in% 1:100 ~ "Até 100 observações"
      ),
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        n
      ),
      n_obs = forcats::fct_reorder(
        n_obs,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(y = sp_name_abv,
                 x = n,
                 label = n) +
    ggplot2::geom_col(
      fill = "chartreuse4",
      #width = .9
    ) +
    ggplot2::geom_label(
      size = 8
    ) +
    ggplot2::labs(y = "Espécies",
                  x = "Número de observações") +
    ggplot2::facet_wrap(
      facets = vars(
        n_obs
      ), 
      nrow = 4, 
      scales = "free"
    ) +
    ggplot2::theme_minimal(14) + 
    theme(
      title = element_text(size = 40), 
      axis.text = element_text(size = 30), 
      # Remove labels from the vertical axis
      #axis.text.y = element_blank(),
      strip.text = element_text(size = 40),
    )

  
  # retronar os gráficos
  return(fig)
}
