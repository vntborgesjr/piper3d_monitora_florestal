# Descrição
# gera tabela dinâmica dos dados completos

gerar_tabdin_dados_distanceR_completo_cov <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_transformados_dist_r_cov.rds'
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados 
  dados_transformados_dist_r_cov <- dados |> 
    slice(
      n_linhas
    ) |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica dos dados_transformados_dist_r_completo
  return(dados_transformados_dist_r_cov)
}