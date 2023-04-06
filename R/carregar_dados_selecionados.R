# Descrição
# a função carregar_dados_selecionados() carrega os dados_completos para
# gerar a tabel dados_selecionados
carregar_dados_selecionados <- function(
    dados = readr::read_rds(
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/dados_completos.rds"
    )
  )
    ) {
  # gerar o data.frame desejado
  dados_selecionados <- dados |> 
    filter(validation == "Espécie")
  
  # grava uma versão dados_completos.rds no diretório data
  readr::write_rds(
    dados_selecionados,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/dados_selecionados.rds"
    )
  )
  
  # retornar o data.frame
  return(dados_selecionados)
}

# exemplo de uso da função
# lembre-se de especificar o diretório correto onde o arquivo se encontra
# na sua máquina
#dados <- carregar_dados_selecionados()
#View(dados)

