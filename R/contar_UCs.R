# Descrição
# fornece o número total de UCs na base de dados.

contar_UCs <- function(
    dados
) {
  #
  n_ucs <- dados_completos |> 
    distinct(uc_name) |> 
    nrow()
  
  return(n_ucs)
}

# Exemplo