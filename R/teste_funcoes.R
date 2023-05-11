#############################################################
##################### Teste das funcoes #####################
#############################################################

# carregar minhas_funcoes.R ---------------------------------------------------------

source("R/minhas_funcoes.R")

# testar funcoes de carregamento ------------------------------------------

carregar_dados_brutos_rds() # ok
carregar_dados_brutos_xlsx() # ok
carregar_dados_completos() # ok
carregar_dados_filtrados() # ok
carregar_dados_selecionados() # ok

# testar funcoes de contagem ----------------------------------------------

contar_n_ano_uc() # ok
contar_n_obs_sp() # ok
contar_n_obs_sp_ano() # ok
contar_n_obs_sp_uc() # ok
contar_n_obs_sp_uc_ano() # ok
contar_n_obs_sp_uc_estacao_ano() # ok
contar_n_obs_uc() # ok
contar_n_obs_uc_ano() # ok
contar_n_obs_validadas() # ok
contar_n_sp() # ok
contar_n_uc() # ok
contar_n_uc_ano() # ok

# testar funcoes que geream tabelas dinamicas -----------------------------

gerar_tabdin_dados_brutos() # ok
gerar_tabdin_dados_completos() # ok
gerar_tabdin_dados_filtrados() # ok
gerar_tabdin_dados_selecionados() # ok
gerar_tabdin_dados_selecionados_distanceR() # ok
gerar_tabdin_dados_selecionados_distanceR_cov() # ok
gerar_tabdin_n_ano_uc() # ok
gerar_tabdin_n_obs_sp() # ok
gerar_tabdin_n_obs_sp_ano() # ok
gerar_tabdin_n_obs_sp_uc() # ok
gerar_tabdin_n_obs_sp_uc_ano() # ok
gerar_tabdin_n_obs_sp_uc_estacao_ano() # ok
gerar_tabdin_n_obs_uc() # ok
gerar_tabdin_n_obs_uc_ano() # ok
gerar_tabdin_n_uc_ano() # ok

# testar funcoes de plotagem dos dados ------------------------------------

plotar_distribuicao_distancia_estatico() # ok
plotar_distribuicao_distancia_interativo() # ok
plotar_n_obs_sp_estatico() # ok, mas a fonte parece muito grande
plotar_n_obs_sp_interativo() # ok
plotar_n_obs_sp_uc_interativo() # ok, mas e um grafico confuso
plotar_n_obs_uc_estatico() # ok, mas esta confuso
plotar_n_obs_uc_interativo() # ok
plotar_n_obs_validadas_estatico() # ok
plotar_n_obs_validadas_interativo() # ok

# testar funcoes para transformar para formato distance -------------------

transformar_para_distanceR() #
transformar_para_distanceR_covariaveis() #
