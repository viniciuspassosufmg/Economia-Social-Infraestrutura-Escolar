# Configuração compartilhada pelas três etapas
# Para incluir um ano, confira no ZIP oficial os nomes dos CSVs e acrescente
# uma linha abaixo com ano, arquivo escolar, arquivo de matrículas e URL.
# Use NA_character_ em matriculas quando os três campos de matrícula estiverem
# no CSV escolar. Se o dicionário mudar os campos ou a codificação, confira
# também a leitura e as validações em 02_tratamento.R antes de rodar a série.
entradas <- tibble::tribble(
  ~ano, ~escolas, ~matriculas, ~url,
  2019L, "microdados_ed_basica_2019.csv", NA_character_,
    "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2019.zip",
  2020L, "microdados_ed_basica_2020.CSV", NA_character_,
    "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2020.zip",
  2021L, "microdados_ed_basica_2021.csv", NA_character_,
    "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2021.zip",
  2022L, "microdados_ed_basica_2022.csv", NA_character_,
    "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2022.zip",
  2023L, "microdados_ed_basica_2023.csv", NA_character_,
    "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2023.zip",
  2024L, "microdados_ed_basica_2024.csv", NA_character_,
    "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2024.zip",
  2025L, "Tabela_Escola_2025_V2.csv", "Tabela_Matricula_2025_V2.csv",
    "https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2025_.zip"
)
# Tabela 4714, variável 93, população residente por município no Censo 2022.
# Atualizar o Censo Escolar acrescenta uma edição sem mudar essa referência.
url_populacao <- "https://apisidra.ibge.gov.br/values/t/4714/n6/all/v/93/p/2022"
# Os scripts carregam esta configuração com chdir = TRUE. Nesse momento,
# a pasta atual é scripts/. Em seguida, o R volta à pasta de execução.
raiz_projeto <- dirname(normalizePath(".", winslash = "/", mustWork = TRUE))
pasta_brutos <- file.path(raiz_projeto, "dados", "brutos")
pasta_tratados <- file.path(raiz_projeto, "dados", "tratados")
arquivo_populacao <- file.path(pasta_brutos, "populacao_censo_2022.json")
# O CSV final é gravado no diretório de trabalho de quem executa o cálculo.
arquivo_saida <- "infraestrutura_municipal_consolidada.csv"

# Os arquivos de cada ano ficam juntos em dados/brutos/<ano>.
# O RDS correspondente traz o cadastro municipal e as escolas públicas ativas.
caminho_bruto <- function(ano, nome) file.path(pasta_brutos, ano, nome)
caminho_tratado <- function(ano) file.path(pasta_tratados, paste0("escolas_", ano, ".rds"))

# Ordem de exportação. M = municipal, E = estadual, F = federal e T = total público.
redes <- c("M", "E", "F", "T")
# F1 = anos iniciais, F2 = anos finais e FC = Fundamental completo.
# FC usa diretamente o total informado no Censo Escolar. Os três campos
# definem, em cada recorte, quais escolas entram e com que peso.
etapas <- c(F1 = "QT_MAT_FUND_AI", F2 = "QT_MAT_FUND_AF", FC = "QT_MAT_FUND")
# À esquerda está o nome no CSV. À direita estão os campos originais do
# Censo. A lista vale para as três etapas, inclusive internet no F1.
# Os campos de rede pública e fossa séptica formam um único critério de
# esgoto, conforme a regra aplicada em 03_calculo.R.
itens <- list(
  "Esgoto (%)" = c("IN_ESGOTO_REDE_PUBLICA", "IN_ESGOTO_FOSSA_SEPTICA"),
  "Internet (%)" = "IN_INTERNET_APRENDIZAGEM",
  "Biblioteca / leitura (%)" = "IN_BIBLIOTECA_SALA_LEITURA",
  "Quadra (%)" = "IN_QUADRA_ESPORTES",
  "Banheiro acessível (%)" = "IN_BANHEIRO_PNE",
  "Cozinha (%)" = "IN_COZINHA",
  "Refeitório (%)" = "IN_REFEITORIO"
)
# Cabeçalhos para os limiares cumulativos de pelo menos 1 a 7 itens ausentes.
limites <- paste0("Pelo menos ", 1:7, c(" carência (%)", rep(" carências (%)", 6)))
