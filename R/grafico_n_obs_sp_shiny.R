# Descrição
# gera um gráfico com o número de observações realizadas por cada UC
# a tabela de dados contem ao número total de observações válidas para cada 
# espécies

grafico_n_obs_sp_shiny <- function(
    dados
    ) {
  # carregar pacote necessário
  #library(patchwork)
  
  # desenha o gráfico com mais de 1000 observações
  grafico_n_obs_sp <- dados  |>  
    mutate(
      sp = forcats::fct_reorder(
        sp,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = sp, # substituir por sp_abv
                 y = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # transforma em grafico interativo
  fig <- plotly::ggplotly(grafico_n_obs_sp)
  
  # retronar os gráficos
  return(fig)
}

# Exemplo

# gerar gráfico
#
#dados_selecionados |> 
#  contar_total_sp() |> 
#  grafico_n_obs_sp_shiny()


# salvar em disco
#ggplot2::ggsave(
# "fig.tiff", 
#width = 15, 
#height = 15,
#units = "cm"
#)

