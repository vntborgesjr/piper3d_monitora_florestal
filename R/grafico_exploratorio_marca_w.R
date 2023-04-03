# Descrição
# a função recebe um conjunto de dados no formato Distance do R e retorna 
# três gráficos descrevendo a variação das distâncias observadas. A função 
# retorna um gráfico de caixa, um gráfico de pontos e um histograma.

grafico_exploratorio_marca_w <- function(
  dados
) {
  # carregar pacotes
  library(patchwork)
  
  # desenha o gráfico de caixas
  box <- dados |> 
    dplyr::arrange(dplyr::desc(distance)) |> 
    ggplot2::ggplot() +
    ggplot2::aes(y = distance) +
    ggplot2::geom_boxplot(col = "black",
                          fill = "chartreuse4") +
    ggplot2::geom_hline(yintercept = 20,
                        color = "red",
                        linetype = 2) +
    ggplot2::geom_text(ggplot2::aes(label = "w = 20",
                                    y = 23.5,
                                    x = .6),
                       color = "red") +
    ggplot2::scale_x_continuous(
      breaks = NULL,
      limits = c(-.8, .8)) +
    ggplot2::labs(
      y = "Distância",
    ) +
    ggplot2::theme_minimal()
  
  # desenha o grafico de pontos
  pontos <- dados |> 
    dplyr::arrange(dplyr::desc(distance)) |> 
    ggplot2::ggplot() +
    ggplot2::aes(y = dplyr::desc(seq_along(distance)), 
                 x = distance) +
    ggplot2::geom_point(
      color = "chartreuse4"
    )  +
    ggplot2::geom_vline(xintercept = 31,
                        color = "red",
                        linetype = 2) +
    ggplot2::geom_text(ggplot2::aes(label = "w = 31",
                                    x = 38,
                                    y = 80),
                       color = "red") +
    ggplot2::labs(x = "Distância",
                  y = "") +
    ggplot2::theme_minimal() + 
    ggplot2::theme(axis.text.y = ggplot2::element_blank())
  
  # desenha o hitograma
  hist <- dados |> 
    dplyr::arrange(dplyr::desc(distance)) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_histogram(binwidth = 2.5,
                            fill = "chartreuse4",
                            col = "white",
                            center = 1.25) +
    ggplot2::geom_vline(xintercept = 20,
                        color = "red",
                        linetype = 2) +
    ggplot2::geom_text(ggplot2::aes(label = "w = 20",
                                    y = 390,
                                    x = 24),
                       color = "red") +
    ggplot2::labs(x = "Distância",
                  y = "Frequência") +
    ggplot2::theme_minimal()
  
  # organiza os gráficos
  fig <- (box + pontos) / hist
  
  # retorna os gráficos
  return(fig)
}

# Exemplo

# carregar dados
#dados_filtrados <- readr::read_rds("Monitora/R/dados_filtrados.rds")

# gerar gráficos
#fig -> grafico_exploratorio_marca_w(dados_filtrados)

#fig

# salva em disco
#ggplot2::ggsave(
 # "fig.tiff", 
  #width = 15, 
  #height = 15,
  #units = "cm"
#)
