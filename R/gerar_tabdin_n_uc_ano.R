# gera tabela dinâmica do número de UCs amostradas por ano
gerar_tabdin_n_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data/n_uc_ano.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de observações por sp
  tabdin_n_uc_ano <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de observações por sp
  return(tabdin_n_uc_ano)
}