# Descrição
# fornece o número total de anos em que cada UC foi amostrada.

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
  # gera o número de anos em que cada UC foi amostrada
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
      n_anos = sum(n)
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
