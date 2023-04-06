
# script para carragar as funções da pasta R ------------------------------

# carregar as funções da pasta R
# carregar função corrigir_diretorio.R
source(
  paste0(
    stringr::str_remove(
      getwd(), 
      'doc'
    ),
    "/R/corrigir_diretorio.R"
  )
) 

# carregar função carregar_dados_brutos_xlsx.R
source(
  corrigir_diretorio(
    corrige = "/R/carregar_dados_brutos_xlsx.R"
  )
)

# carregar função gerar_tabdin_dados_brutos
source(
  corrigir_diretorio(
    corrige = "/R/gerar_tabdin_dados_brutos.R"
  )
)

# carregar a função carregar_dados_completos
source(
  corrigir_diretorio(
    corrige = '/R/carregar_dados_completos.R'
  )
)

# carregar função gerar_tabdin_dados_completos
source(
  corrigir_diretorio(
    corrige = "/R/gerar_tabdin_dados_completos.R"
  )
)

# carregar a função carregar_dados_filtrados
source(
  corrigir_diretorio(
    corrige = '/R/carregar_dados_filtrados.R'
  )
)

# carregar a função gerar_tabdin_dados_filtrados
source(
  corrigir_diretorio(
    corrige = '/R/gerar_tabdin_dados_filtrados.R'
  )
)

# carregar função transformar_para_distanceR
source(
  corrigir_diretorio(
    corrige = "/R/transformar_para_distanceR.R"
  )
)

# carregar a função gerar_tabdin_dados_distanceR_completo
source(
  corrigir_diretorio(
    corrige = '/R/gerar_tabdin_dados_distanceR_completo.R'
  )
)

# carregar a função gerar_tabdin_dados_distanceR_completo
source(
  corrigir_diretorio(
    corrige = '/R/transformar_para_distanceR_covariaveis.R'
  )
)

# carregar a função gerar_tabdin_dados_distanceR_completo
source(
  corrigir_diretorio(
    corrige = '/R/gerar_tabdin_dados_distanceR_completo_cov.R'
  )
)

# carregar a função contar_n_obs_validadas
source(
  corrigir_diretorio(
    corrige = '/R/contar_n_obs_validadas.R'
  )
)

# carregar a função contar_n_obs_validadas
source(
  corrigir_diretorio(
    corrige = '/R/plotar_n_obs_validadas.R'
  )
)

# carregar a função contar_n_obs_validadas
source(
  corrigir_diretorio(
    corrige = '/R/carregar_dados_selecionados.R'
  )
)

# carregar a função contar_n_obs_validadas
source(
  corrigir_diretorio(
    corrige = '/R/gerar_tabdin_dados_selecionados.R'
  )
)

# carregar a função 
source(
  corrigir_diretorio(
    corrige = "/R/contar_n_uc.R"
  )
)

# carregar a função 
source(
  corrigir_diretorio(
    corrige = "/R/contar_n_obs_uc.R"
  )
)

# carregar a função contar_n_sp.R
source(
  corrigir_diretorio(
    corrige = "/R/contar_n_sp.R"
  )
)

# carregar função plotar_n_obs_uc_interativo
source(
  corrigir_diretorio(
    corrige = "/R/plotar_n_obs_uc_interativo.R"
  )
)

# carregar função plotar_n_obs_uc_interativo
source(
  corrigir_diretorio(
    corrige = "/R/gerar_tabdin_n_obs_uc.R"
  )
)

# carregar função contar_n_obs_sp
source(
  corrigir_diretorio(
    corrige = "/R/contar_n_obs_sp.R"
  )
)

# carregar função gerar_tabdin_n_obs_sp
source(
  corrigir_diretorio(
    corrige = "/R/gerar_tabdin_n_obs_sp.R"
  )
)

# carregar função plotar_n_sp_uc_interativo
source(
  corrigir_diretorio(
    corrige = "/R/plotar_n_sp_uc_interativo.R"
  )
)

# carregar função contar_n_obs_sp_uc
source(
  corrigir_diretorio(
    corrige = "/R/contar_n_obs_sp_uc.R"
  )
)

# carregar função contar_n_uc_ano
source(
  corrigir_diretorio(
    corrige = "/R/contar_n_uc_ano.R"
  )
)

# carregar função contar_n_ano_uc
source(
  corrigir_diretorio(
    corrige = "/R/contar_n_ano_uc.R"
  )
)


#dados = readr::read_rds(
#  file = paste0(
#    stringr::str_remove(
#      getwd(), 
#      "doc"
#    ),
#    "/data/dados_completos.rds"
#  )
#)