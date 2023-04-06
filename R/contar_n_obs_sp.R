# Descrição
# fornece o número total de observações por espécie.

contar_n_obs_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o número de observaçẽos por espécie
  n_obs_sp <- dados |> 
    # gerar nome das sp abreviados
    # mutate(sp_abv = ) |> 
    dplyr::count(
      sp,
      #sp_abv
    )
  
  # grava a tabela n_obs_sp.rds no diretório data/
  readr::write_rds(
    n_obs_sp,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/n_obs_sp.rds"
    )
  )
  
  # retorna o número de observaçẽos por espécie
  return(n_obs_sp)
}

# Exemplo

#contar_total_sp() 
