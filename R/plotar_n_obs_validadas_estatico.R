# gera gráfico com número de observações validadas para cada nível taxonômico
plotar_n_obs_validadas_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/tabela_n_obs_validadas.rds"
      )
    )
) {
  #
  grafico_n_sp_validada_estatico <- dados  |>  
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validation,
      y = n,
      label = n
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    geom_label(
      aes(
        label = n
      )
    ) +
    ggplot2::labs(
      title = "Número de obs. validadas para \ncada nível taxonômico",
      x = "Nível taxonômico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)
  
  # retornar gráfico estático
  return(grafico_n_sp_validada_estatico)
}

