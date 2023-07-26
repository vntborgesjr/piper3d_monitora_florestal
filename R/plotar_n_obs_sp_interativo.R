# Documentacao da funcao plotar_n_obs_sp_interativo() --------------------
#' Gera graficos de barras interativos com o numero de observacoes por especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras interativos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_sp_interativo()}
plotar_n_obs_sp_interativo <- function(
    dados
) {
  fig <- plotly::ggplotly(
    # gerar figura
    fig <- dados |>
      dplyr::mutate(
        n_obs = dplyr::case_when(
          n > 1000 ~ "Mais de 1000 observacoes",
          n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
          n %in% 101:500 ~ "Entre 101 e 500 observacoes",
          n %in% 1:100 ~ "Ate 100 observacoes"
        ),
        nome_sp = forcats::fct_reorder(
          nome_sp,
          n
        ),
        n_obs = forcats::fct_reorder(
          n_obs,
          dplyr::desc(n)
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::aes(y = nome_sp,
                   x = n,
                   label = nome_sp_abv) +
      ggplot2::geom_col(
        fill = "chartreuse4",
        #width = .9
      ) +
      ggplot2::labs(y = "Especies",
                    x = "Numero de observacoes") +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(
          n_obs
        ),
        nrow = 4,
        scales = "free"
      ) +
      ggplot2::theme_minimal(14) +
      ggplot2::theme(
        # Remove labels from the vertical axis
        axis.text.y = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 10),
      )
  )

  # retronar os graficos
  return(fig)
}

utils::globalVariables(
  c(
    "n_obs"
  )
)
