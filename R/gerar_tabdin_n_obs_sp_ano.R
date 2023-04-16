# Descrição
# gera tabela dinâmica do número de observações por espécies por ano

gerar_tabdin_n_obs_sp_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_obs_sp_ano.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de gerar_tabdin_n_obs_sp_ano
  tabdin_n_obs_sp_ano <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de gerar_tabdin_n_obs_sp_ano
  return(tabdin_n_obs_sp_ano)
}