# Descrição
# carrega os dados brutos em formato .xlsx diretamente da pasta raw-data
# funciona apenas no ambiente do arquivo nome_do_arquivo.rmd

carregar_dados_brutos_xlsx <- function(
  dados = readxl::read_excel(
    path = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"
    ),
    sheet = "dados brutos"
    )
  ) {
    # retorna os dados butos
  return(dados)
}

# Exemplo
#carregar_dados_brutos_xlsx()
