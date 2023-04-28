# Descrição

plotar_n_obs_sp_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/n_obs_sp.rds"
      )
    )
) {
  # gerar figura
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
    ggplot2::geom_text(
      aes(0, y = sp_name_abv, label = sp_name),
      hjust = 0,
      nudge_x = 0.3,
      colour = "white",
      family = "Econ Sans Cnd",
      size = 7
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
      axis.text.y = element_blank(),
      strip.text = element_text(size = 40),
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
#contar_total_sp() |> 
# grafico_n_sp_UC_interativo()

