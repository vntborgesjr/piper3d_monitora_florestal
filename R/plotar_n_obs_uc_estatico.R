# desenha um gráfico de barras esetático com o número de observações por UC
plotar_n_obs_uc_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data/n_obs_uc.rds'
      )
    )
) {
  # desenha os gráficos com mais de 1000, de 501 a 1000, de 101 a 500 e até 100 observações
  fig <- dados |> 
    dplyr::mutate(
      n_obs = dplyr::case_when(
        n > 1000 ~ "Mais de 1000 observações",
        n %in% 501:1000 ~ "Entre 501 e 1000 observações",
        n %in% 101:500 ~ "Entre 101 e 500 observações",
        n %in% 1:100 ~ "Até 100 observações"
      ),
      uc_name = forcats::fct_reorder(
        uc_name,
        n
      ),
      n_obs = forcats::fct_reorder(
        n_obs,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(y = uc_name,
                 x = n,
                 label = n) +
    ggplot2::geom_col(
      fill = "chartreuse4",
      #width = .9
    ) +
    ggplot2::geom_label(
      size = 8
    ) +
    ggplot2::geom_text(
      aes(0, y = uc_name, label = uc_name),
      hjust = 0,
      nudge_x = 0.3,
      colour = "black",
      family = "Econ Sans Cnd",
      size = 7
    ) +
   ggplot2::labs(y = "Unidades de Conservação",
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
      axis.text.y = element_blank(),
      strip.text = element_text(size = 40),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # retronar os gráficos
  return(fig)
}

