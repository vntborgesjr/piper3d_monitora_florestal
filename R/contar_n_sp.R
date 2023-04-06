# Descrição
# fornece o número total de UCs na base de dados.

contar_n_sp <- function(
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
  n_sp <- dados |> 
    distinct(sp) |> 
    nrow()
  
  return(n_sp)
}

# Exemplo