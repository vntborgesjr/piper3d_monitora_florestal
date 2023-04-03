# Descrição
# fornece o número total de UCs na base de dados.

contar_total_sp <- function(
    dados = dados_selecionados
) {
  #
  total_sp <- dados |> 
    # gerar nome das sp abreviados
    # mutate(sp_abv = ) |> 
    dplyr::count(
      sp#,
      #sp_abv
    )
  
  return(total_sp)
}

# Exemplo

#contar_total_sp() 
