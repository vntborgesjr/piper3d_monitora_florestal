# Descrição
# fornece o número total de observações por espécie.

contar_n_ano_uc <- function(
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
  n_ano_uc <- dados |>  
    dplyr::filter(
      validation == "Espécie"
    ) |> 
    dplyr::count(
      year, 
      uc_name, 
      uc_name_abv
    ) |>  
    dplyr::group_by(
      uc_name, 
      uc_name_abv
    ) |>  
    dplyr::count(
      year
    ) |>  
    dplyr::summarise(
      n_ucs = sum(n)
    )
  
  # grava a tabela n_ano_uc.rds no diretório data/
  readr::write_rds(
    n_ano_uc,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/n_ano_uc.rds"
    )
  )
  
  # retorna o número de UCs por ano
  return(n_ano_uc)
}

# Exemplo

#contar_n_ano_uc() 
