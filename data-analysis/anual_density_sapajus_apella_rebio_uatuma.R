##%###################################################################%##
#                                                                       #
####       Estimativa de densidade por amostragem por distância      ####
####                  Reserva Biológica do Uatumã                    ####
####                        Sapajus apella                           ####
####                    2 de dezembro de 2023                        ####
#                                                                       #
##%###################################################################%##

# carregar pacote ---------------------------------------------------------

library(distanceMonitoraflorestal)

# filtrar dados -----------------------------------------------------------

primatas <- filtrar_dados(
  dados = monitora_aves_masto_florestal,
  nome_uc == "rebio_uatuma",
  validacao_obs = "especie"
)

# View(primatas)
# View(head(monitora_aves_masto_florestal))



gerar_tabdin(
  contar_n_obs_sp_uc_ano(primatas) |> 
    dplyr::arrange(dplyr::desc(n)) |> 
    dplyr::select(1, 3, 5, 6)
)


esec_da_terra_do_meio
flona_do_jamari
rebio_do_uatuma
resex_tapajos_arapiuns