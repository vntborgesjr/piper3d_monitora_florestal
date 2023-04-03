# Descrição
# A função recebe o diretorio de trabalho atual e altera para o diretório 
# correto

corrigir_diretorio <- function(
  diretorio_atual = getwd(),   
  padrao_alterar = "doc",
  corrige = "/R/"
) {
  # substituir o pdarão atual pelo correto
  novo_diretorio <- paste0(
    stringr::str_remove(
      diretorio_atual, 
      padrao_alterar
    ),
    corrige
  )
  
  # retorno o novo diretorio 
  return(novo_diretorio)
}

# Exemplo
# Padrão correto a ser acrescentado
#certo <- "/R/grafico_exploratorio3.R"

#corrigir_diretorio(
#  corrige = certo
#)
