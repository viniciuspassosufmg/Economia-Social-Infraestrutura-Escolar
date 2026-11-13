# Valida os microdados e prepara um RDS por edição.
if (.Platform$OS.type == "windows" && Sys.getlocale("LC_CTYPE") == "C")
  invisible(Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8"))
suppressPackageStartupMessages(library(dplyr))
source("scripts/configuracao.R", encoding = "UTF-8")

ler_censo <- function(arquivo, ano, campos) {
  if (!file.exists(arquivo)) stop("Censo ", ano, ": arquivo ausente: ", arquivo)
  contexto <- paste("Censo", ano, "-", arquivo)
  # Os CSVs atuais usam ponto e vírgula e codificação Latin-1. Lemos primeiro
  # o cabeçalho para apontar campos ausentes antes de processar o arquivo.
  localidade <- readr::locale(encoding = "Latin1", decimal_mark = ",")
  cabecalho <- names(readr::read_delim(arquivo, delim = ";", n_max = 0,
    locale = localidade, name_repair = "minimal", show_col_types = FALSE))
  faltantes <- setdiff(campos, cabecalho)
  if (length(faltantes)) stop(contexto, ": campos ausentes: ", paste(faltantes, collapse = ", "))
  if (anyDuplicated(cabecalho)) stop(contexto, ": campos duplicados no cabeçalho.")
  # Códigos são identificadores e ficam como texto. Lemos somente os campos
  # usados no cálculo, o que reduz o volume de dados em memória.
  texto <- c("CO_ENTIDADE", "CO_MUNICIPIO", "NO_MUNICIPIO", "SG_UF")
  tipos <- lapply(campos, function(campo) {
    if (campo %in% texto) readr::col_character() else readr::col_double()
  })
  selecao <- do.call(readr::cols_only, setNames(tipos, campos))
  dados <- readr::read_delim(arquivo, delim = ";", col_types = selecao,
    locale = localidade,
    na = c("", "NA"), name_repair = "minimal", trim_ws = FALSE, show_col_types = FALSE)
  # Uma falha de leitura numérica deve ser investigada na fonte antes
  # de ser interpretada como um dado ausente.
  problemas <- readr::problems(dados)
  if (nrow(problemas)) stop(contexto, ": valores inválidos nas colunas ",
    paste(unique(problemas$col), collapse = ", "), " (primeira linha: ", problemas$row[1], ").")
  if (anyNA(dados$NU_ANO_CENSO) || any(dados$NU_ANO_CENSO != ano))
    stop(contexto, ": NU_ANO_CENSO incompatível com o ano configurado.")
  if (any(!grepl("^[0-9]{8}$", dados$CO_ENTIDADE)) || anyDuplicated(dados$CO_ENTIDADE))
    stop(contexto, ": CO_ENTIDADE inválido ou duplicado.")
  if (any(!grepl("^[0-9]{7}$", dados$CO_MUNICIPIO)) || any(!dados$TP_DEPENDENCIA %in% 1:4))
    stop(contexto, ": CO_MUNICIPIO ou TP_DEPENDENCIA inválido.")
  dados
}

ler_ano <- function(ano, escolas, matriculas) {
  chave <- c("NU_ANO_CENSO", "CO_ENTIDADE")
  cadastro <- c(chave, "CO_MUNICIPIO", "TP_DEPENDENCIA")
  campos <- c(cadastro, "NO_MUNICIPIO", "SG_UF", "TP_SITUACAO_FUNCIONAMENTO", unlist(itens, use.names = FALSE))
  if (is.na(matriculas)) {
    return(ler_censo(escolas, ano, c(campos, unname(etapas))))
  }
  # Nos arquivos separados (como em 2025), cada tabela tem uma linha
  # por escola. Conferimos também se município e rede coincidem.
  x <- ler_censo(escolas, ano, campos)
  m <- ler_censo(matriculas, ano, c(cadastro, unname(etapas)))
  if (nrow(anti_join(m, x, by = cadastro)) > 0L)
    stop("Censo ", ano, ": NU_ANO_CENSO, CO_ENTIDADE, CO_MUNICIPIO ou TP_DEPENDENCIA divergente entre ",
      escolas, " e ", matriculas, ".")
  # Escolas sem correspondência ficam com matrículas desconhecidas.
  left_join(x, select(m, all_of(c(chave, unname(etapas)))), by = chave, relationship = "one-to-one")
}

ler_populacao <- function(arquivo) {
  # O JSON veio do SIDRA/IBGE. D3C filtra o ano de referência, D2C = 93
  # identifica a população residente, D1C guarda o código municipal e V
  # guarda a população usada para aplicar as faixas de porte.
  faixas <- c("Até 5.000 habitantes", "5.001-10.000 habitantes", "10.001-20.000 habitantes",
    "20.001-50.000 habitantes", "50.001-100.000 habitantes", "100.001-500.000 habitantes",
    "Mais de 500.000 habitantes")
  dados <- jsonlite::fromJSON(arquivo) |>
    filter(D3C == "2022", D2C == "93") |>
    transmute(codigo_ibge = D1C, populacao = as.double(V))
  stopifnot(nrow(dados) > 0L, !anyDuplicated(dados$codigo_ibge),
    all(grepl("^[0-9]{7}$", dados$codigo_ibge)), all(is.finite(dados$populacao) & dados$populacao >= 0))
  dados |>
    transmute(codigo_ibge, porte = as.character(cut(populacao,
      c(-Inf, 5000, 10000, 20000, 50000, 100000, 500000, Inf), labels = faixas)))
}

preparar_ano <- function(dados, ano, populacao, origem) {
  # O cadastro municipal é derivado antes do filtro de escolas públicas
  # ativas para conservar municípios sem resultado. Preenchemos nome e UF
  # somente com informações do mesmo código municipal.
  cadastro <- dados |>
    distinct(codigo_ibge = CO_MUNICIPIO, municipio = NO_MUNICIPIO, uf = SG_UF) |>
    group_by(codigo_ibge) |>
    tidyr::fill(municipio, uf, .direction = "downup") |>
    ungroup() |>
    distinct()
  if (anyNA(cadastro$municipio) || anyNA(cadastro$uf) || anyDuplicated(cadastro$codigo_ibge))
    stop("Censo ", ano, " - ", origem, ": NO_MUNICIPIO ou SG_UF ausente ou divergente por CO_MUNICIPIO.")
  # O primeiro dígito do código IBGE identifica a região. Municípios sem
  # população de referência continuam na tabela com uma categoria própria.
  regioes <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
  cadastro <- cadastro |>
    mutate(regiao = regioes[match(substr(codigo_ibge, 1, 1), as.character(1:5))]) |>
    left_join(populacao, by = "codigo_ibge", relationship = "one-to-one") |>
    mutate(porte = coalesce(porte, "Sem referência municipal no Censo 2022"))
  if (anyNA(cadastro$regiao)) stop("Censo ", ano, " - ", origem, ": região inválida em CO_MUNICIPIO.")

  if (any(!dados$TP_SITUACAO_FUNCIONAMENTO %in% 1:4))
    stop("Censo ", ano, " - ", origem, ": TP_SITUACAO_FUNCIONAMENTO inválido.")
  # Situação 1 indica escola ativa. Dependência 1, 2 e 3 identifica,
  # respectivamente, as redes federal, estadual e municipal.
  escolas <- dados |>
    filter(TP_SITUACAO_FUNCIONAMENTO == 1, TP_DEPENDENCIA %in% 1:3)
  for (campo in unlist(itens, use.names = FALSE)) {
    x <- escolas[[campo]]
    if (any(!is.na(x) & !x %in% 0:1))
      stop("Censo ", ano, " - ", origem, ": esperado 0, 1 ou NA em ", campo, ".")
  }
  for (campo in etapas) {
    x <- escolas[[campo]]
    if (any(!is.na(x) & (!is.finite(x) | x < 0 | x != floor(x))))
      stop("Censo ", ano, " - ", origem, ": matrículas inválidas em ", campo, ".")
  }
  list(cadastro = cadastro, escolas = escolas)
}

dir.create("dados/tratados", recursive = TRUE, showWarnings = FALSE)
populacao <- ler_populacao(arquivo_populacao)
for (i in seq_len(nrow(entradas))) {
  entrada <- entradas[i, ]
  ano <- entrada$ano
  message("Tratando Censo Escolar ", ano)
  escolar <- caminho_bruto(ano, entrada$escolas)
  matriculas <- if (is.na(entrada$matriculas)) NA_character_ else caminho_bruto(ano, entrada$matriculas)
  dados <- ler_ano(ano, escolar, matriculas)
  tratados <- preparar_ano(dados, ano, populacao, paste(na.omit(c(escolar, matriculas)), collapse = " e "))
  saveRDS(tratados, caminho_tratado(ano), compress = FALSE)
}
