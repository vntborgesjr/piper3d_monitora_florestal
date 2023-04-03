# Descrição
# gera gráfico com número de observações validadas para cada nível taxonômico

grafico_n_sp_validadas <- function(
    dados = readr::read_rds(
      stringr::str_replace_all(
        getwd(),
        pattern = "doc",
        replacement = "data/dados_completos.rds"
      )
    )
) {
  #
  grafico_n_sp_validada <- dados_completos  |>  
    dplyr::count(
      validation
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validation,
      y = n,
      label = n
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    ggplot2::geom_label() +
    ggplot2::labs(
      title = "Número de obs. validadas para \ncada nível taxonômico",
      x = "Nível taxonômico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)
  
  return(grafico_n_sp_validada)
}

# Exemplo

dados = readr::read_rds(
  stringr::str_replace_all(
    string = getwd(),
    pattern = "doc",
    replacement = "data/dados_completos.rds"
  )
)
