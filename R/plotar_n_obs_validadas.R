# Descrição
# gera gráfico com número de observações validadas para cada nível taxonômico

plotar_n_obs_validadas <- function(
    dados = readr::read_rds(
      stringr::str_replace_all(
        getwd(),
        pattern = "doc",
        replacement = "data/tabela_n_obs_validadas.rds"
      )
    )
) {
  #
  grafico_n_sp_validada <- dados  |>  
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validation,
      y = n,
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    ggplot2::labs(
      title = "Número de obs. validadas para \ncada nível taxonômico",
      x = "Nível taxonômico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)
  
  # gerar gráfico interativo
  grafico_n_sp_validada <- plotly::ggplotly(
    grafico_n_sp_validada
  )
  
  return(grafico_n_sp_validada)
}

# Exemplo
