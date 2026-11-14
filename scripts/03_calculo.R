# Calcula coberturas e carências e grava o CSV na pasta de execução.
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

# Percentual = 100 × matrículas nas escolas que atendem à condição /
# matrículas em todas as escolas elegíveis do município, rede e etapa.
# A condição pode ser a presença de um item ou uma carência de pelo menos k
# itens. Se não houver matrículas elegíveis ou a condição for desconhecida
# em alguma escola com peso positivo, o percentual fica NA.
percentual <- function(condicao, peso) {
  total <- sum(peso)
  if (total == 0 || anyNA(condicao[peso > 0])) return(NA_real_)
  100 * sum(peso[condicao %in% TRUE]) / total
}

calcular_ano <- function(tratados, ano) {
  cadastro <- tratados$cadastro
  escolas <- tratados$escolas
  # Nas variáveis de infraestrutura, 1 = presente e 0 = ausente.
  # Para esgoto, basta rede pública ou fossa séptica confirmada. Se nenhuma
  # estiver confirmada e uma delas for desconhecida, o critério fica NA.
  coberturas <- lapply(itens, function(campos) Reduce(`|`, lapply(escolas[campos], `==`, 1)))
  presencas <- as.matrix(as_tibble(coberturas))
  # Contamos carências em cada escola antes de agregar. O mínimo usa
  # somente ausências confirmadas. O máximo inclui os itens desconhecidos.
  # Para cada k: confirma se o mínimo alcança k, descarta se o máximo
  # fica abaixo de k e mantém NA nos outros casos.
  min_carencias <- rowSums(!presencas, na.rm = TRUE)
  max_carencias <- min_carencias + rowSums(is.na(presencas))
  carencias <- setNames(lapply(1:7, function(k) {
    case_when(min_carencias >= k ~ TRUE, max_carencias < k ~ FALSE, .default = NA)
  }), limites)

  # Colocamos F1, F2 e FC em linhas separadas para usar a mesma regra em
  # todas as etapas. Matrículas positivas e observadas definem elegibilidade.
  # Atribuímos peso zero às escolas com matrícula desconhecida para
  # excluí-las da ponderação. Elas continuam na contagem de escolas ativas.
  por_etapa <- bind_cols(
    transmute(escolas, codigo_ibge = CO_MUNICIPIO, rede = c("F", "E", "M")[TP_DEPENDENCIA]),
    transmute(escolas, F1 = QT_MAT_FUND_AI, F2 = QT_MAT_FUND_AF, FC = QT_MAT_FUND),
    as_tibble(c(coberturas, carencias))
  ) |>
    tidyr::pivot_longer(all_of(names(etapas)), names_to = "etapa", values_to = "peso") |>
    mutate(peso = coalesce(peso, 0))

  # Para o total público, reunimos as escolas das três redes antes de
  # calcular os percentuais. Cada escola conserva seu peso em matrículas.
  # Em cada grupo, contamos ativas, elegíveis e matrículas elegíveis.
  agregados <- bind_rows(por_etapa, mutate(por_etapa, rede = "T")) |>
    group_by(codigo_ibge, rede, etapa) |>
    summarise(escolas_ativas = n(), escolas_elegiveis = sum(peso > 0),
      matriculas_elegiveis = sum(peso),
      across(all_of(c(names(itens), limites)), ~ percentual(.x, peso)),
      .groups = "drop")

  # Montamos as 12 combinações de rede e etapa de cada município.
  # Sem escola ativa na rede, a junção deixa contagens e medidas como NA.
  # Com ativas e sem matrículas elegíveis, as contagens de elegíveis e de
  # matrículas são zero e os percentuais ficam NA.
  tidyr::expand_grid(cadastro, rede = redes, etapa = names(etapas)) |>
    left_join(agregados, by = c("codigo_ibge", "rede", "etapa"), relationship = "one-to-one") |>
    mutate(ano = ano)
}

resultados <- lapply(entradas$ano, function(ano) {
  caminho <- caminho_tratado(ano)
  if (!file.exists(caminho)) stop("Execute scripts/02_tratamento.R. Arquivo ausente: ", caminho)
  message("Calculando ", ano)
  calcular_ano(readRDS(caminho), ano)
})
tabela <- bind_rows(resultados) |>
  arrange(codigo_ibge, ano, match(rede, redes), match(etapa, names(etapas))) |>
  select("Código IBGE" = codigo_ibge, "Município" = municipio, "UF" = uf,
    "Ano" = ano, "Rede" = rede, "Etapa" = etapa, all_of(names(itens)),
    "Região" = regiao, "Porte populacional 2022" = porte,
    "Escolas ativas" = escolas_ativas, "Escolas elegíveis" = escolas_elegiveis,
    "Matrículas elegíveis" = matriculas_elegiveis, all_of(limites))
readr::write_csv(tabela, arquivo_saida, na = "NA")
message("Tabela consolidada: ", nrow(tabela), " linhas em ", arquivo_saida)
