# Descrição
# gera tabela dinâmica do número de anos em que cada UC foi amostrada

gerar_tabdin_n_ano_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_ano_uc.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de anos em que cada UC foi amostrada
  tabdin_n_ano_uc <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de anos em que cada UC foi amostrada
  return(tabdin_n_ano_uc)
}