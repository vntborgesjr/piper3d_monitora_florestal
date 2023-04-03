# Descrição
# gera um gráfico com o número de observações realizadas por cada UC

grafico_n_obs_UC_shiny <- function(
    dados
    ) {
  # carregar pacote necessário
  #library(patchwork)
  
  # desenha o gráfico com mais de 1000 observações
  grafico_n_obs_uc <- dados  |>  
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das UC's abreviados",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # transforma em grafico interativo
  fig <- plotly::ggplotly(grafico_n_obs_uc)
  
  # retronar os gráficos
  return(fig)
}

# Exemplo

# gerar gráfico
#grafico_n_obs_UC_shiny(
#  dados = readr::read_rds("data/dados_selecionados.rds")
#)


# salvar em disco
#ggplot2::ggsave(
# "fig.tiff", 
#width = 15, 
#height = 15,
#units = "cm"
#)

