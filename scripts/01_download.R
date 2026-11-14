# Obtém os insumos oficiais. CSVs já presentes na pasta do ano têm prioridade.
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
options(timeout = max(900, getOption("timeout")))

baixar <- function(url, destino, fonte) {
  if (file.exists(destino)) return(invisible(destino))
  parcial <- tempfile(tmpdir = dirname(destino))
  on.exit(unlink(parcial), add = TRUE)
  # Repetimos a transferência em caso de falha temporária de conexão.
  for (tentativa in 1:3) {
    resultado <- tryCatch(
      suppressWarnings(download.file(url, parcial, mode = "wb", method = "libcurl")),
      error = conditionMessage
    )
    if (identical(resultado, 0L)) {
      if (!file.rename(parcial, destino))
        stop(fonte, " — ", url, ": falha ao gravar ", destino, call. = FALSE)
      return(invisible(destino))
    }
    unlink(parcial)
    if (tentativa < 3) Sys.sleep(tentativa)
  }
  stop(fonte, " — ", url, ": falha no download após três tentativas (", resultado, ").", call. = FALSE)
}

extrair_csv <- function(zip, membro, destino, tamanho) {
  parcial <- tempfile(tmpdir = dirname(destino))
  on.exit(unlink(parcial))
  # Lemos o membro pelo caminho original e escolhemos o nome do CSV na saída.
  # Assim o R não precisa converter o nome da pasta interna do ZIP.
  local({
    entrada <- unz(zip, membro, open = "rb")
    on.exit(close(entrada), add = TRUE)
    saida <- file(parcial, open = "wb")
    on.exit(close(saida), add = TRUE)
    repeat {
      bloco <- readBin(entrada, "raw", n = 1024L * 1024L)
      if (!length(bloco)) break
      writeBin(bloco, saida)
    }
  })
  if (!isTRUE(file.size(parcial) == tamanho) || !file.rename(parcial, destino))
    stop("falha ao extrair ", basename(destino))
}

obter_ano <- function(entrada) {
  ano <- entrada$ano
  pasta <- caminho_bruto(ano, "")
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  nomes <- na.omit(c(entrada$escolas, entrada$matriculas))
  faltantes <- nomes[!file.exists(file.path(pasta, nomes))]
  if (!length(faltantes)) {
    message(ano, ": usando CSVs locais.")
    return(invisible(NULL))
  }
  zip <- file.path(pasta, basename(entrada$url))
  fonte <- paste("Censo Escolar", ano)
  baixar(entrada$url, zip, fonte)
  tryCatch({
    indice <- unzip(zip, list = TRUE)
    # Alguns ZIPs do Inep contêm anexos com nomes em uma codificação antiga.
    # Os CSVs configurados têm nomes ASCII. Separamos os nomes dos caminhos por
    # bytes e mantemos o caminho original para extrair o CSV, mesmo que a pasta
    # interna tenha acentos em outra codificação.
    nomes_no_zip <- sub("^.*/", "", indice$Name, useBytes = TRUE)
    nomes_ascii <- grepl("^[ -~]+$", nomes_no_zip, useBytes = TRUE)
    indice <- indice[nomes_ascii, ]
    nomes_no_zip <- tolower(nomes_no_zip[nomes_ascii])
    for (nome in faltantes) {
      linha <- which(nomes_no_zip == tolower(nome))
      if (length(linha) != 1L) stop("esperado um arquivo ", nome, " no ZIP.")
      extrair_csv(zip, indice$Name[linha], file.path(pasta, nome), indice$Length[linha])
    }
  }, error = function(e) stop(fonte, " — ", entrada$url, ": ", conditionMessage(e), call. = FALSE))
}

for (i in seq_len(nrow(entradas))) obter_ano(entradas[i, ])
baixar(url_populacao, arquivo_populacao, "População municipal do Censo 2022")
message("Insumos disponíveis em ", pasta_brutos, ".")
