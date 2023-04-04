# Descrição
# a função carregar_dados1() carrega toda a planilha de dados 
# selecionando automaticamente as colunas CDU, Local - Nome da Unidade de 
# Conservação
# deve carregar uma planilha que esteja no formato do arquivo:
# Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP
# enviada pelo Gerson por whatsapp no dia 08/03/2023

carregar_dados_completos <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data-raw/dados_brutos.rds'
      )
    )
    ) {
  # carraga os dados
  #dados_brutos <- readxl::read_excel(
   # path = corrigir_diretorio(
    #  corrige = "/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"
    #),
    #sheet = "dados brutos"
  #)
  
  # padronizar separadores
  # gerar o data.frame desejado
  dados_completos <- dados |>  
    dplyr::select(
      uc_code = CDUC,
      uc_name = `Local - Nome da Unidade de Conservação`,
      ea_number = `Número da Estação Amostral`,
      ea_name = `Nome da EA`,
      season = `Estação do ano`,
      sampling_day = `data da amostragem`,
      day_effort = `Esforço de amostragem tamanho da trilha (m)`,
      class = Classe,
      order = Ordem,
      family = Família,
      genus = Gênero,
      sp = `Espécies validadas para análise do ICMBio`,
      validation = `Clasificação taxonômica validada`, 
      distance = `distância (m)     do animal em relação a trilha`,
      group_size = `n° de animais`,
      observadores = `nome dos observadores`
    ) |> 
    dplyr::mutate(
      uc_category = stringi::stri_extract_first_words(
        uc_name
      ),
      # abrevia o nome das UCs
      uc_name_abv = forcats::lvls_revalue(
        uc_name,
        new_levels = c(
          "ETM", "EM", "EN", "ESGT", "FJ", "PCV", "PA", "PSBoc", "PSBod", "PSC",
          "PSM", "PSC", "PSD", "PSP", "PSO", "PPN", "PCO", "PI", "PJaú", "PJur",
          "PMR", "PS", "PV", "PCA", "PMT", "RG", "RJ", "RTap", "RU", "RG",
          "RTrom", "RAT", "RBA", "RCI", "RCM", "RRC", "RROP", "RIA", "RRA", "RTA"
        )
      ),
      validation = forcats::fct_recode(
        validation,
        "Espécie" = "E",
        "Espécie" = "e",
        "Família" = "F",
        "Gênero" = "G",
        "Gênero" = "g",
        "Ordem" = "O"
      ),
      year = lubridate::year(sampling_day),
      across(
        where(is.character),
        as.factor
      ),
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
      -tidyselect::starts_with("obs")
    ) |> 
    dplyr::relocate(
      uc_category,
      .before = uc_name
    ) |> 
    dplyr::relocate(
      uc_name_abv,
      .after = uc_name
    )
  
  # retornar o data.frame
  return(dados_completos)
}

# exemplo de uso da função
# lembre-se de especificar o diretório correto onde o arquivo se encontra
# na sua máquina
#dados <- carregar_dados_completos()
#View(dados)


# rm(list = ls())