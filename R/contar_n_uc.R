# Descrição
# fornece o número total de UCs na base de dados.

contar_n_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/dados_completos.rds"
      )
    )
) {
  #
  n_ucs <- dados |> 
    distinct(uc_name) |> 
    nrow()
  
  return(n_ucs)
}

# Exemplo
