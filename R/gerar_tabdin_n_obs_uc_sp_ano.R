# Descrição
# gera tabela dinâmica do número de observações por UC, por espécies, por estação e por ano

gerar_tabdin_n_obs_uc_sp_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_obs_uc_sp_estacao_ano.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de observações por UC, por espécies, por estação e por ano
  tabdin_n_obs_uc_sp_estacao_ano <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de observações por UC, por espécies, por estação e por ano
  return(tabdin_n_obs_uc_sp_estacao_ano)
}