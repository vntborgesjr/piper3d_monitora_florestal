# gera tabela dinâmica dos dados completos
gerar_tabdin_dados_distanceR_completo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data/dados_transformados_dist_r_completo.rds'
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados completos
  dados_transformados_dist_r_completo <- dados |> 
    slice(
      n_linhas
    ) |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica dos dados_transformados_dist_r_completo
  return(dados_transformados_dist_r_completo)
}