# retorna três gráficos estáticos descrevendo a variação das distâncias observadas. A função 
# retorna um gráfico de caixa, um gráfico de pontos e um histograma.
plotar_distribuicao_distancia_estatico <- function(
    dados
) {
  # desenha o gráfico de caixa
  box <- dados |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_boxplot(col = "black",
                          fill = "chartreuse4") +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      limits = c(-.8, .8)) +
    ggplot2::labs(
      x = "",
      y = " \n \n",
    ) +
    ggplot2::theme_minimal()
  
  # desenha o gráfico de pontos
  pontos <- dados |> 
    dplyr::arrange(dplyr::desc(distance)) |> 
    ggplot2::ggplot() +
    ggplot2::aes(y = dplyr::desc(seq_along(distance)), 
                 x = distance) +
    ggplot2::geom_point(
      color = "chartreuse4"
    )  +
    ggplot2::labs(x = "Distância",
                  y = " \n \n") +
    ggplot2::theme_minimal() + 
    ggplot2::theme(axis.text.y = ggplot2::element_blank())
  
  # dessenha o hitograma
  hist <- dados |> 
    dplyr::arrange(dplyr::desc(distance)) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_histogram(binwidth = 2.5,
                            fill = "chartreuse4",
                            col = "white",
                            center = 1.25) +
    ggplot2::labs(x = "",
                  y = "Frequência") +
    ggplot2::theme_minimal()
  
  # organizar os gráficos
  fig <- ggpubr::ggarrange(
    hist,
    box, 
    pontos,
    nrow = 3
  )
  
  # retronar os gráficos
  return(fig)
}

