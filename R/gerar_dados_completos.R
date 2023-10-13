# Documentação da função gerar_dados_completos() ----------------------
#' Seleciona, trasforma e renomea as colunas e observações nos dados brutos
#'
#' @description
#' A função \code{gerar_dados_completos()} recebe uma \code{tibble} gerada a
#' partir da função \code{carregar_dados_brutos_xlsx()}, selecionando,
#' transformando e renomeando suas colunas e observações.
#'
#' @usage gerar_dados_completos(dados)
#'
#' @param dados recebe uma \code{tibble} contendo os dados brutos.
#'
#' @details
#' A funcao \code{gerar_dados_completos()} seleciona, transforma e renomeia as
#' colunas e observações dos dados brutos do Projeto Monitora Componente
#' Florestal.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo uma seleção de
#' colunas transformadas e renomeadas a partir dos dados brutos do Projeto
#' Monitora Componente Florestal.

#' @details
#' A função gera novas colunas contendo o número de vezes que cada estação
#' amostral foi visitada, a categoria da UC, o nome abreviado das UC's, o nome
#' abreviado das espécies, o esforço amostral total empregado em cada estação
#' amostral, o número de observadores e o tempo total de cada censo. Quando
#' ausentes, as distâncias de trilhas percorridas sao imputadas. Outra
#' transfomração importante é a inlcusão de linhas para os dias em que houve
#' amostragem e uma dada espécie não foi observada (ex. distância = \code{NA})
#'
#' @export
#'
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto

#' @examples \dontrun{gerar_dados_completos(dados = dados_brutos)}
gerar_dados_completos <- function(dados) {

  dados <- dados|>
    janitor::clean_names()

  # gera tabela de consulta com os nomes antigos das colunas
  nomes_antigos <- names(dados)

  # gerar novos nomes em portugues para as colunas
  nomes_novos_pt <- c(
    codigo_uc = nomes_antigos[1],
    nome_uc = nomes_antigos[2],
    numero_ea = nomes_antigos[3],
    nome_ea = nomes_antigos[4],
    data_amostragem = nomes_antigos[6],
    nomes_antigos[7],
    estacao = nomes_antigos[8],
    esforco_dia = nomes_antigos[5],
    nome_classe = nomes_antigos[16],
    nome_ordem = nomes_antigos[17],
    nome_familia = nomes_antigos[18],
    nome_genero = nomes_antigos[19],
    nome_sp = nomes_antigos[22],
    validacao = nomes_antigos[23],
    distancia = nomes_antigos[27],
    tamanho_grupo = nomes_antigos[25],
    observadores = nomes_antigos[14],
    horario_inicio = nomes_antigos[9],
    horario_termino = nomes_antigos[10],
    velocidade_km_h = nomes_antigos[12]
  )



  dados_completos <- dados |>
    # selecionar as colunas de interesse
    dplyr::select(tidyselect::all_of(nomes_novos_pt)) |>
    dplyr::mutate(
      # gera uma nova coluna contendo a categoria das UCs
      categoria_uc = stringi::stri_extract_first_words(
        nome_uc
      ),
      # gera uma nova coluna com o nome das UCs abreviados
      nome_uc_abv = stringr::str_split(# divide o genero e epiteto em diferentes listas
        nome_sp, " "
      ) |>
        purrr::map(# retem apenas as 4 primeiras letras dos itens de cada lista
          \(string) stringr::str_sub(
            string, 1, 4
          )
        ) |>
        purrr::map(# aidiciona "." ao final dos itens de cada lista
          \(string) stringr::str_c(
            string, "."
          )
        ) |>
        purrr::map(# adiciona " " ao final dos itens de cada lista
          \(string) stringr::str_flatten(
            string, " "
          )
        ) |>
        purrr::list_c(), # concatena os itens das duas listas
      # gera nova coluna com o nome das sp abreviados
      nome_sp_abv = stringr::str_split(# divide o genero e epiteto em diferentes listas
        nome_sp, " "
      ) |>
        purrr::map(# retem apenas as 4 primeiras letras dos itens de cada lista
          \(string) stringr::str_sub(
            string, 1, 4
          )
        ) |>
        purrr::map(# aidiciona "." ao final dos itens de cada lista
          \(string) stringr::str_c(
            string, "."
          )
        ) |>
        purrr::map(# adiciona " " ao final dos itens de cada lista
          \(string) stringr::str_flatten(
            string, " "
          )
        ) |>
        purrr::list_c(), # concatena os itens das duas listas
      # padroniza a codificacao da coluna de validacao
      validacao = forcats::fct_recode(
        validacao,
        "Especie" = "E",
        "Especie" = "e",
        "Familia" = "F",
        "Genero" = "G",
        "Genero" = "g",
        "Ordem" = "O"
      ),
      # atribui o tipo fator a todas as colunas tipo caracter
      dplyr::across(
        tidyselect::where(is.character),
        as.factor
      ),
      # gera uma nova coluna com o tempo total do censo
      tempo_censo = horario_termino - horario_inicio,
      # padroniza os separadores entre os nomes dos observadores
      # substitu "e" por ","
      novo = stringr::str_replace_all(
        observadores,
        " e ",
        ", "
      ),
      # substitui "E" por ","
      novo = stringr::str_replace_all(
        novo,
        " E ",
        ", "
      ),
      # substitui "/" por ","
      novo = stringr::str_replace_all(
        novo,
        "/",
        ", "
      ),
      # substitui ";" por ","
      novo = stringr::str_replace_all(
        novo,
        ";",
        ", "
      ),
      novo = stringr::str_replace_all(# substitui "a" por ","
        novo,
        " a ",
        ", "
      )
    ) |>
    # gerar colunas individualizadas para cada observador
    tidyr::separate_wider_delim(
      novo,
      ",",
      names = c("obs1", "obs2", "obs3", "obs4", "obs5", "obs6"),
      too_few = "align_start"
    ) |>
    # gerar coluna com o numero total de observadores
    dplyr::mutate(
      # substituir o nome dos observadores por 1 e NAs por 0
      obs1 = ifelse(!is.na(obs1), 1, 0),
      obs2 = ifelse(!is.na(obs2), 1, 0),
      obs3 = ifelse(!is.na(obs3), 1, 0),
      obs4 = ifelse(!is.na(obs4), 1, 0),
      obs5 = ifelse(!is.na(obs5), 1, 0),
      obs6 = ifelse(!is.na(obs6), 1, 0),
      # somar colunas para gerar o numero total de observadores
      numero_observadores = obs1 + obs2 + obs3 + obs4 + obs5 + obs6
    ) |>
    # completar observacoes repetidas que estao ausentes
    # agrupar por nome da estacao amostral e data dee amostragem
    dplyr::group_by(
      nome_uc,
      nome_ea,
      data_amostragem
    ) |>
    # aninhar dados pelas colunas agrupadas
    tidyr::nest() |>
    dplyr::mutate(
      # gerar nova coluna de esforco sem observacoes ausentes
      esforco_dia2 = purrr::map(
        data,
        \(.x) rep(.x$esforco_dia[!is.na(.x$esforco_dia)][1])
      ),
      # gerar nova coluna de tempo de censo sem observacoes ausentes
      tempo_censo2 = purrr::map(
        data,
        \(.x) rep(.x$tempo_censo[!is.na(.x$tempo_censo)][1])
      ),
      # gerar nova coluna de velocidade sem observacoes ausentes
      velocidade_km_h2 = purrr::map(
        data,
        \(.x) rep(.x$velocidade_km_h[!is.na(.x$velocidade_km_h)][1])
      )
    ) |>
    # desaninhar os dados
    tidyr::unnest(
      c(data, esforco_dia2, tempo_censo2, velocidade_km_h2)
    ) |>
    # desagrupar os dados
    dplyr::ungroup() |>
    # selecionar as colunas de interesse
    dplyr::select(
      tidyselect::ends_with(
        c("uc", "ea")
      ),
      data_amostragem,
      estacao,
      ano,
      esforco_dia = esforco_dia2,
      nome_classe:numero_observadores,
      tempo_censo = tempo_censo2,
      velocidade_km_h = velocidade_km_h2,
      -velocidade_km_h2,
      -tempo_censo2,
      -esforco_dia,
      -tidyselect::starts_with("obs"),
      -tidyselect::starts_with("horario")
    ) |>
    # reposicionar colunas
    dplyr::relocate(
      categoria_uc,
      .before = nome_uc
    ) |>
    dplyr::relocate(
      nome_uc_abv,
      .after = nome_uc
    ) |>
    dplyr::relocate(
      nome_sp_abv,
      .after = nome_sp
    ) |>
    # preencher observacoes ausentes
    tidyr::fill(esforco_dia) |>
    tidyr::fill(
      velocidade_km_h,
      .direction = "down"
    ) |>
    # eliminar espacos, letras minusculas e caracteres especiais das observacoes
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.factor),
        \(.x) janitor::make_clean_names(
          .x, allow_dupes = TRUE
        )
      ),
      # atribui o tipo fator a todas as colunas tipo caracter
      dplyr::across(
        tidyselect::where(is.character),
        as.factor
      )
    )

  # retornar o tibble com os dados completos
  return(dados_completos)

}

utils::globalVariables(
  c(
    "horario_termino",
    "horario_inicio",
    "observadores",
    "novo",
    "obs1",
    "obs2",
    "obs3",
    "obs4",
    "obs5",
    "obs6",
    "esforco_dia2",
    "tempo_censo2",
    "velocidade_km_h2",
    "nome_classe",
    "numero_observadores",
    "categoria_uc",
    "n_visitas_repetidas"
  )
)
