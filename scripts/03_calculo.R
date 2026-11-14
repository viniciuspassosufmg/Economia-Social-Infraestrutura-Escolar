# 03 | Cálculo e exportação
# Lê os RDS da etapa 02. Calcula sete coberturas e sete percentuais
# cumulativos de carência e grava uma linha por município, ano, rede e etapa.

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

# Regra comum aos percentuais -----------------------------------------------

# Percentual = 100 × matrículas nas escolas que atendem à condição /
# matrículas em todas as escolas elegíveis do município, rede e etapa.
# A condição pode ser a presença de um item ou uma carência de pelo menos k
# itens. Se não houver matrículas elegíveis ou a condição for desconhecida
# em alguma escola com peso positivo, o percentual fica NA. O denominador
# inclui todas as matrículas elegíveis do recorte. Quando a condição é
# conhecida para todas e nenhuma atende a ela, o resultado é zero.
percentual <- function(condicao, peso) {
  total <- sum(peso)
  if (total == 0 || anyNA(condicao[peso > 0])) return(NA_real_)
  100 * sum(peso[condicao %in% TRUE]) / total
}

calcular_ano <- function(tratados, ano) {
  cadastro <- tratados$cadastro
  escolas <- tratados$escolas
  # Presença dos sete itens por escola
  # Nas variáveis de infraestrutura, 1 = presente e 0 = ausente.
  # Para esgoto, basta rede pública ou fossa séptica confirmada. Se nenhuma
  # estiver confirmada e uma delas for desconhecida, o critério fica NA.
  coberturas <- lapply(itens, function(campos) Reduce(`|`, lapply(escolas[campos], `==`, 1)))
  presencas <- as.matrix(as_tibble(coberturas))
  # Carências simultâneas por escola
  # O mínimo conta as ausências confirmadas e o máximo soma os itens
  # desconhecidos a esse mínimo. Com três ausências e dois itens desconhecidos,
  # a escola atende aos limites 1, 2 e 3. Os limites 4 e 5 ficam indeterminados,
  # e os limites 6 e 7 ficam falsos. Os limites são cumulativos e se sobrepõem.
  min_carencias <- rowSums(!presencas, na.rm = TRUE)
  max_carencias <- min_carencias + rowSums(is.na(presencas))
  carencias <- setNames(lapply(1:7, function(k) {
    case_when(min_carencias >= k ~ TRUE, max_carencias < k ~ FALSE, .default = NA)
  }), limites)

  # Matrículas e elegibilidade por etapa
  # Colocamos F1, F2 e FC em linhas separadas para aplicar os mesmos sete itens
  # em todas as etapas. Matrículas positivas observadas definem a elegibilidade.
  # A mesma escola pode contribuir para F1 e F2. FC usa o total do Censo.
  # Atribuímos peso zero às escolas com matrícula desconhecida para
  # excluí-las da ponderação. Elas continuam na contagem de escolas ativas.
  por_etapa <- bind_cols(
    transmute(escolas, codigo_ibge = CO_MUNICIPIO, rede = c("F", "E", "M")[TP_DEPENDENCIA]),
    transmute(escolas, F1 = QT_MAT_FUND_AI, F2 = QT_MAT_FUND_AF, FC = QT_MAT_FUND),
    as_tibble(c(coberturas, carencias))
  ) |>
    tidyr::pivot_longer(all_of(names(etapas)), names_to = "etapa", values_to = "peso") |>
    mutate(peso = coalesce(peso, 0))

  # Agregação por município, rede e etapa
  # Para o total público, reunimos as escolas das três redes antes de dividir
  # os numeradores pelo denominador, dando o mesmo peso a cada matrícula.
  # Cada linha do grupo corresponde a uma escola ativa. Peso positivo
  # identifica uma escola elegível. A soma dos pesos conta suas matrículas.
  agregados <- bind_rows(por_etapa, mutate(por_etapa, rede = "T")) |>
    group_by(codigo_ibge, rede, etapa) |>
    summarise(escolas_ativas = n(), escolas_elegiveis = sum(peso > 0),
      matriculas_elegiveis = sum(peso),
      across(all_of(c(names(itens), limites)), ~ percentual(.x, peso)),
      .groups = "drop")

  # Combinações de rede e etapa no cadastro municipal
  # Montamos as 12 combinações de rede e etapa de cada município.
  # Sem escola ativa na rede, a junção deixa contagens e medidas como NA.
  # Com ativas e sem matrículas elegíveis, as contagens de elegíveis e de
  # matrículas são zero e os percentuais ficam NA.
  tidyr::expand_grid(cadastro, rede = redes, etapa = names(etapas)) |>
    left_join(agregados, by = c("codigo_ibge", "rede", "etapa"), relationship = "one-to-one") |>
    mutate(ano = ano)
}

# Consolidação da série e gravação do CSV ------------------------------------

resultados <- lapply(entradas$ano, function(ano) {
  caminho <- caminho_tratado(ano)
  if (!file.exists(caminho)) stop("Execute scripts/02_tratamento.R. Arquivo ausente: ", caminho)
  message("Calculando ", ano)
  calcular_ano(readRDS(caminho), ano)
})
# Aqui fixamos os nomes e a ordem das 25 colunas da tabela para a plataforma.
tabela <- bind_rows(resultados) |>
  arrange(codigo_ibge, ano, match(rede, redes), match(etapa, names(etapas))) |>
  select("Código IBGE" = codigo_ibge, "Município" = municipio, "UF" = uf,
    "Ano" = ano, "Rede" = rede, "Etapa" = etapa, all_of(names(itens)),
    "Região" = regiao, "Porte populacional 2022" = porte,
    "Escolas ativas" = escolas_ativas, "Escolas elegíveis" = escolas_elegiveis,
    "Matrículas elegíveis" = matriculas_elegiveis, all_of(limites))
readr::write_csv(tabela, arquivo_saida, na = "NA")
message("Tabela consolidada: ", nrow(tabela), " linhas em ", arquivo_saida)
