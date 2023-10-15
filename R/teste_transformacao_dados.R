
# teste usando as funções do projeto atual -------------------------------------------

source("R/carregar_dados_brutos_xlsx.R")
source("R/gerar_dados_completos.R")
source("R/filtrar_dados.R")
source("R/transformar_dados_formato_Distance.R")

# carregar dados brutos
dados <- carregar_dados_brutos_xlsx()

# gerar dados completos - demora...
dados_completos <- gerar_dados_completos(dados)

# dados filtrados com dias sem observações incluídos!
dados_filtrados <- filtrar_dados(
  dados = dados_completos,
  nome_uc %in% c("esec_da_terra_do_meio" ,"parna_da_serra_do_pardo"),
  nome_sp == c("sapajus_apella"),
  validacao_obs = "especie"
)

dados_filtrados |> 
  View()

# teste transformar formato distance sem repetição
# incluidas colunas repeated_visits e Effort_day
# funcionando!
dados_formato_distance_repeticao <- transformar_dados_formato_Distance(
  dados = dados_filtrados,
  Region.Label,
  Sample.Label,
  amostras_repetidas = TRUE
)

dados_formato_distance_repeticao |>
  View()

# teste transformar formato distance sem repetição
# precisa de ajuste
dados_formato_distance <- transformar_dados_formato_Distance(
  dados = dados_filtrados,
  Region.Label,
  Sample.Label,
  season,
  year,
  amostras_repetidas = FALSE
)

dados_formato_distance |>
  View()

# teste usando o pacote -------------------------------------------
# funciona dentro do pacote, mas não fuciona aqui...
# remover funções com mesmo nome
rm(
  carregar_dados_brutos_xlsx,
  filtrar_dados,
  gerar_dados_completos,
  transformar_dados_formato_Distance
)

# reinstalar o pacote
devtools::install_github("vntborgesjr/distanceMonitoraflorestal", force = TRUE)

# carregar o pacote
library(distanceMonitoraflorestal)
View(monitora_aves_masto_florestal)


# dados filtrados com dias sem observações incluídos!
dados_filtrados <- filtrar_dados(
  dados = monitora_aves_masto_florestal,
  "nome_uc" %in% c("esec_da_terra_do_meio" ,"parna_da_serra_do_pardo"),
  nome_sp == c("sapajus_apella"),
  validacao_obs = "especie"
)

dados_filtrados |> 
  View()

# teste transformar formato distance sem repetição
# incluidas colunas repeated_visits e Effort_day
# funcionando!
dados_formato_distance_repeticao <- transformar_dados_formato_Distance(
  dados = dados_filtrados,
  Region.Label,
  Sample.Label,
  amostras_repetidas = TRUE
)

dados_formato_distance_repeticao |>
  View()

# teste transformar formato distance sem repetição
# precisa de ajuste
dados_formato_distance <- transformar_dados_formato_Distance(
  dados = dados_filtrados,
  Region.Label,
  Sample.Label,
  season,
  year,
  amostras_repetidas = FALSE
)
