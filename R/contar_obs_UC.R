# Descrição
# gera uma tabela com o número de observações válidas para cada UC.

contar_obs_UC <- function(
    dados = dados_selecionados
) {
  #
  contagem_sp_UC <- dados  |>  
    dplyr::count(
      uc_name,
      uc_name_abv
    )
  
  return(contagem_sp_UC)
}

# Exemplo


