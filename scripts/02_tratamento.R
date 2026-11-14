# 02 | Tratamento dos microdados
# Usa os CSVs e a população obtidos na etapa 01. Para cada edição, salva em
# dados/tratados/ um RDS com o cadastro municipal e os registros
# das escolas públicas ativas, com matrículas e infraestrutura.

# No Windows, usa uma configuração UTF-8 quando a sessão inicia no locale C.
if (.Platform$OS.type == "windows" && Sys.getlocale("LC_CTYPE") == "C")
  invisible(Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8"))
suppressPackageStartupMessages(library(dplyr))
# Localiza a configuração ao executar com Rscript ou com Source no RStudio.
arquivos <- c(
  unlist(lapply(rev(sys.frames()), function(quadro) quadro$ofile)),
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
)
candidatos <- c(file.path(dirname(arquivos), "configuracao.R"),
  "scripts/configuracao.R", "configuracao.R")
configuracao <- candidatos[file.exists(candidatos)][1L]
if (is.na(configuracao))
  stop("Não foi possível localizar scripts/configuracao.R a partir de ", getwd(),
    ". Abra o script na pasta extraída e use Source, ou execute com Rscript.", call. = FALSE)
source(configuracao, encoding = "UTF-8", chdir = TRUE)

# Leitura e validação de cada CSV --------------------------------------------

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
  # Cada arquivo deve representar uma única edição e conter uma linha por
  # escola. Isso permite vincular os dados sem duplicar matrículas.
  if (anyNA(dados$NU_ANO_CENSO) || any(dados$NU_ANO_CENSO != ano))
    stop(contexto, ": NU_ANO_CENSO incompatível com o ano configurado.")
  if (any(!grepl("^[0-9]{8}$", dados$CO_ENTIDADE)) || anyDuplicated(dados$CO_ENTIDADE))
    stop(contexto, ": CO_ENTIDADE inválido ou duplicado.")
  if (any(!grepl("^[0-9]{7}$", dados$CO_MUNICIPIO)) || any(!dados$TP_DEPENDENCIA %in% 1:4))
    stop(contexto, ": CO_MUNICIPIO ou TP_DEPENDENCIA inválido.")
  dados
}

# Compatibilização dos arquivos de escolas e matrículas ---------------------

ler_ano <- function(ano, escolas, matriculas) {
  chave <- c("NU_ANO_CENSO", "CO_ENTIDADE")
  cadastro <- c(chave, "CO_MUNICIPIO", "TP_DEPENDENCIA")
  campos <- c(cadastro, "NO_MUNICIPIO", "SG_UF", "TP_SITUACAO_FUNCIONAMENTO", unlist(itens, use.names = FALSE))
  if (is.na(matriculas)) {
    return(ler_censo(escolas, ano, c(campos, unname(etapas))))
  }
  # Em 2019–2024 as matrículas já constam do CSV escolar. Em 2025 são
  # necessárias duas tabelas com uma linha por escola.
  x <- ler_censo(escolas, ano, campos)
  m <- ler_censo(matriculas, ano, c(cadastro, unname(etapas)))
  # Toda linha de matrículas deve ter um cadastro do mesmo ano e escola
  # com o mesmo código de município e a mesma dependência administrativa.
  if (nrow(anti_join(m, x, by = cadastro)) > 0L)
    stop("Censo ", ano, ": NU_ANO_CENSO, CO_ENTIDADE, CO_MUNICIPIO ou TP_DEPENDENCIA divergente entre ",
      escolas, " e ", matriculas, ".")
  # Preservamos o cadastro escolar inteiro. Sem correspondência na tabela
  # de matrículas, os três campos da etapa ficam NA e o vínculo da escola
  # com município e rede continua disponível para as contagens de ativas.
  left_join(x, select(m, all_of(c(chave, unname(etapas)))), by = chave, relationship = "one-to-one")
}

# Porte populacional de referência ------------------------------------------

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
  # O limite superior pertence à faixa. Por exemplo, 5.000 habitantes
  # entram na primeira classe e 5.001 na segunda.
  dados |>
    transmute(codigo_ibge, porte = as.character(cut(populacao,
      c(-Inf, 5000, 10000, 20000, 50000, 100000, 500000, Inf), labels = faixas)))
}

# Cadastro municipal e seleção das escolas públicas ativas ------------------

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
  # população de referência continuam na tabela com uma categoria própria,
  # como Boa Esperança do Norte, que aparece no cadastro escolar de 2025.
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
  # A elegibilidade por matrícula é definida depois, na etapa 03, pois
  # uma escola pode ter matrículas em uma etapa e zero ou NA em outra.
  escolas <- dados |>
    filter(TP_SITUACAO_FUNCIONAMENTO == 1, TP_DEPENDENCIA %in% 1:3)
  # A infraestrutura deve vir em 0/1 ou NA. Matrículas observadas devem
  # ser números inteiros não negativos. Mantemos os ausentes para o cálculo.
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

# Gravação de uma base por edição --------------------------------------------

dir.create(pasta_tratados, recursive = TRUE, showWarnings = FALSE)
# O mesmo porte populacional de 2022 é usado em todas as edições.
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
