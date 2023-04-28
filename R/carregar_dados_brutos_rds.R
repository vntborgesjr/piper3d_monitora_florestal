# Descrição
# carrega os dados brutos em formato .rds diretamente da pasta raw-data
# funciona apenas no ambiente do arquivo nome_do_arquivo.rmd

carregar_dados_brutos_rds <- function(
  dados = readr::read_rds(
    file = paste0(here::here(),
      "/data-raw/dados_brutos.rds"
    )
    )
  ) {
  # grava uma versão dados_brutos.rds no diretório data-raw
  readr::write_rds(
    dados,
    file = paste0(
      here::here(),
      "/data-raw/dados_brutos.rds"
    )
  )
  
    # retorna os dados butos
  return(dados)
}

# Exemplo
#carregar_dados_brutos_rds()
