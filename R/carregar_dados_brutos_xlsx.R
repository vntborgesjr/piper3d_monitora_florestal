# Descrição
# carrega os dados brutos em formato .xlsx diretamente da pasta raw-data
# funciona apenas no ambiente do arquivo nome_do_arquivo.rmd

carregar_dados_brutos_xlsx <- function(
  dados = readxl::read_excel(
    path = paste0(
      here::here(),
      "/data-raw/monitora_masto_aves_2023_04_04.xlsx"
    ),
    sheet = "dados brutos"
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
#carregar_dados_brutos_xlsx()
