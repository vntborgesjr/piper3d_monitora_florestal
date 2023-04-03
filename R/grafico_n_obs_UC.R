# Descrição

grafico_n_obs_UC <- function(
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
  mais_mil_obs <- dados_selecionados  |>  
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
    ggplot2::geom_label() +
    ggplot2::labs(title = "UC's com mais de 1000 observações") +
    ggplot2::theme_minimal(14)
  
  # desenha o gráfico com 501 a 1000 observações
  quinehtos_mil_obs <- dados_selecionados  |>  
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
    ggplot2::geom_label() +
    ggplot2::labs(title = "UC's com 500 a 1000 observações") +
    ggplot2::theme_minimal(14)
  
  # desenha o gráfico com 101 a 500 observações
  cem_quintas_obs <- dados_selecionados  |>  
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
    ggplot2::geom_label() +
    ggplot2::labs(title = "UC's com 100 a 500 observações") +
    ggplot2::theme_minimal(14)
  
  # desenha o gráfico com menos de 100 observações
  uma_cem_obs <- dados_selecionados  |>  
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
    ggplot2::geom_label() +
    ggplot2::labs(title = "UC's com menos de 1000 observações") +
    ggplot2::theme_minimal(14)
  
  # organizar os gráficos
  fig <- ggpubr::ggarrange(
    mais_mil_obs, 
    quinehtos_mil_obs,
    cem_quintas_obs,
    uma_cem_obs,
    ncols = 2,
    nrows = 2,
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
grafico_n_obs_UC()

