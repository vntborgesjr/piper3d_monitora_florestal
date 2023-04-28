# gera tabela dinâmica dos dados completos
gerar_tabdin_dados_completos <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data/dados_completos.rds'
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados completos
  dados_completos <- dados |> 
    slice(n_linhas) |> 
    DT::datatable(filter = list(position = "top"))
  
  # retornar a tabela dinamica dos dados completos
  return(dados_completos)
}