# Descrição
# fornece o número total de UCs na base de dados.

contar_sp <- function(
    dados
) {
  #
  n_sp <- dados_completos |> 
    distinct(sp) |> 
    nrow()
  
  return(n_sp)
}

# Exemplo