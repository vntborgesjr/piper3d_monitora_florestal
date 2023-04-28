# fornece quantas UC's foram amostradas por ano
contar_n_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o número de observaçẽos por espécie
  n_uc_ano <- dados |>  
    dplyr::filter(
      validation == "Espécie"
    ) |> 
    dplyr::count(
      year, 
      uc_name
    ) |>  
    dplyr::group_by(
      year
    ) |>  
    dplyr::count(
      uc_name
    ) |>  
    dplyr::summarise(
      n_ucs = sum(n)
    )
  
  # grava a tabela n_uc_ano.rds no diretório data/
  readr::write_rds(
    n_uc_ano,
    file = paste0(
      here::here(),
      "/data/n_uc_ano.rds"
    )
  )
  
  # retorna o número de UCs por ano
  return(n_uc_ano)
}

