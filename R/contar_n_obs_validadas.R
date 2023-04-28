# Descrição
# fornece o número total de observações validadas para cada nível taxonômico
contar_n_obs_validadas <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data/dados_completos.rds'
      )
    )
) {
  # gerar tabela com o número de observações validadas para cada nível 
  # taxonomico
  tabela_n_obs_validadas <- dados |> 
    count(validation) 
  
  # gravar tabela_n_obs_validadas.rds no diretório data
  readr::write_rds(
    tabela_n_obs_validadas,
    file = paste0(
      here::here(),
      "/data/tabela_n_obs_validadas.rds"
    )
  )
  
  # extrair os números de observações validadas 
  n_obs_validadas <- tabela_n_obs_validadas |> 
    pull(var = n)
  
  # retornar os números de observações validadas 
  return(n_obs_validadas)
}

# Exemplo
#contar_n_obs_validadas()