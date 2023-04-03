# a função carregar_dados_filtrados carrega a planilha de dados selecioinando apenas
# a UC e espécie desejada no formato da Luciana Fusinatto
# deve carregar uma planilha que esteja no formato do arquivo:
# Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx
# enviada pelo Gerson por whatsapp no dia 08/03/2023
carregar_dados_filtrados <- function(dados,
                            nome_uc,
                            nome_sp) {
  # carraga os dados
  dados1 <- readxl::read_excel(dados, 
                               sheet = "dados brutos")
  
  # gerar o data.frame desejado
  dados2 <- dados1 |>  
    dplyr::select(
      uc_code = CDUC,
      uc_name = `Local - Nome da Unidade de Conservação`,
      ea_number = `Número da Estação Amostral`,
      ea_name = `Nome da EA`,
      season = `Estação do ano`,
      sampling_day = `data da amostragem`,
      day_effort = `Esforço de amostragem tamanho da trilha (m)`,
      sp = `Espécies validadas para análise do ICMBio`,
      distance = `distância (m)     do animal em relação a trilha`,
      group_size = `n° de animais`,
      observadores = `nome dos observadores`,
      termino = `horário de término (h:mm)`,
      inicio = `horário de início  (h:mm)`
    ) |> 
    dplyr::mutate(
      year = lubridate::year(sampling_day),
      across(
        where(is.character),
        as.factor
      ),
      cense_time = termino - inicio,
      novo = stringr::str_replace_all(observadores, 
                                      " e ",
                                      ", "),
      novo = stringr::str_replace_all(novo, 
                                      " E ",
                                      ", "),
      novo = stringr::str_replace_all(novo, 
                                      "/",
                                      ", "),
      novo = stringr::str_replace_all(novo, 
                                      ";",
                                      ", "),
      novo = stringr::str_replace_all(novo, 
                                      " a ",
                                      ", ") 
    ) |> 
    tidyr::separate_wider_delim(
      novo, 
      ",",
      names = c("obs1", "obs2", "obs3", "obs4", "obs5", "obs6"),
      too_few = "align_start"
    ) |> 
    dplyr::mutate(
      obs1 = ifelse(!is.na(obs1), 1, 0),
      obs2 = ifelse(!is.na(obs2), 1, 0),
      obs3 = ifelse(!is.na(obs3), 1, 0),
      obs4 = ifelse(!is.na(obs4), 1, 0),
      obs5 = ifelse(!is.na(obs5), 1, 0),
      obs6 = ifelse(!is.na(obs6), 1, 0),
      number_observers = obs1 + obs2 + obs3 + obs4 + obs5 + obs6
    ) |> 
    dplyr::group_by(ea_name, sampling_day) |> 
    tidyr::nest() |> 
    dplyr::mutate(day_effort2 = purrr::map(data, \(.x) rep(.x$day_effort[!is.na(.x$day_effort)][1]))) |> 
    tidyr::unnest(c(data, day_effort2)) |> 
    dplyr::ungroup() |> 
    dplyr::select(
      tidyselect::starts_with(c("uc", "ea")),
      season,
      year,
      sampling_day,
      day_effort = day_effort2,
      sp:number_observers,
      -day_effort,
      -tidyselect::starts_with("obs"),
      cense_time
    ) |> 
    dplyr::filter(
      uc_name == nome_uc,
      sp == nome_sp
    )
  
  # retornar o data.frame
  return(dados2)
}

# exemplo de uso da função
# lembre-se de especificar o diretório correto onde o arquivo se encontra
# na sua máquina
#dados <- carregar_dados_filtrados(dados = /home/usuario/Documentos/Vitor/Piper3D/WWF/Monitora/Dados/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx",
 #                        nome_uc = "Resex Tapajós-Arapiuns",
  #                       nome_sp = "Dasyprocta croconota")
#View(dados)
