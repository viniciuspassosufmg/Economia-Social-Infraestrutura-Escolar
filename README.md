# Indicador de Infraestrutura em Escolas de Ensino Fundamental (I²E²F)

Este projeto da disciplina de Economia Social produz a tabela municipal de coberturas e carências de infraestrutura escolar para a plataforma de visualização. A série atual reúne todas as edições do Censo Escolar de **2019 a 2025**.

## Como rodar

Instale o R e, em uma sessão do R, instale os pacotes:

```r
install.packages(c("readr", "dplyr", "tidyr", "tibble", "jsonlite"))
```

Abra um terminal na raiz da cópia local do repositório e execute em ordem:

```sh
Rscript scripts/01_download.R
Rscript scripts/02_tratamento.R
Rscript scripts/03_calculo.R
```

1. **Download.** Obtém os insumos oficiais em `dados/brutos/`. Para reutilizar cópias locais, coloque os CSVs de cada edição em `dados/brutos/<ano>/`, com os nomes da configuração. Se preferir fornecer os ZIPs, coloque-os nessa mesma pasta com o nome do arquivo no link oficial. O JSON populacional fica em `dados/brutos/populacao_censo_2022.json`.
2. **Tratamento.** Valida campos e chaves, junta os CSVs de escolas e matrículas em 2025, aplica os filtros e prepara um RDS por ano em `dados/tratados/`.
3. **Cálculo.** Lê os RDS e grava `infraestrutura_municipal_consolidada.csv` diretamente na pasta em que o comando é executado.

O teste local usou R 4.5.1, readr 2.1.5, dplyr 1.1.4, tidyr 1.3.1, tibble 3.3.0 e jsonlite 2.0.0. No Windows, com os CSVs já disponíveis, as três etapas levaram cerca de 2, 27 e 45 segundos, respectivamente. Os CSVs brutos ocupam cerca de 1,5 GB, além do espaço para ZIPs e intermediários.

## Fontes e atualização anual

O Inep publica os microdados em ZIP com arquivos CSV. A configuração em `scripts/configuracao.R` especifica o nome de cada CSV, inclusive a edição **2025 V2**, com duas tabelas. As oito cópias locais usadas na validação, incluindo a edição **2024 defeso**, têm o mesmo tamanho e CRC-32 dos arquivos listados nos ZIPs oficiais. [Catálogo oficial do Inep](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar)

Downloads do Inep por edição: [2019](https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2019.zip), [2020](https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2020.zip), [2021](https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2021.zip), [2022](https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2022.zip), [2023](https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2023.zip), [2024](https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2024.zip) e [2025](https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_2025_.zip).

O porte municipal usa a população residente do Censo Demográfico de 2022 como referência fixa. Os 5.570 valores municipais da cópia local, obtida em 25/10/2026, conferem com a API. [Download do JSON do SIDRA, tabela 4714, variável 93](https://apisidra.ibge.gov.br/values/t/4714/n6/all/v/93/p/2022)

Para atualizar a série, consulte os dicionários da edição nova e acrescente uma linha à configuração com ano, nome do CSV escolar, nome do CSV separado de matrículas (ou `NA_character_` quando as matrículas já estiverem no CSV escolar) e URL do ZIP. Se o Inep alterar campos ou conceitos, revise a leitura correspondente antes de incorporar a edição. Rode as três etapas novamente.

## Como ler o CSV

- Uma linha por município, ano, rede e etapa. Importe `Código IBGE` como texto de sete dígitos. O CSV usa UTF-8, vírgula como separador e ponto decimal.
- As redes são `M` (municipal), `E` (estadual), `F` (federal) e `T` (total público). As etapas são `F1` (anos iniciais), `F2` (anos finais) e `FC` (Fundamental completo, conforme informado pelo Censo).
- Entram no cálculo as escolas públicas em atividade com matrículas positivas observadas na etapa. Cada cobertura é a proporção dessas matrículas em escolas com o item. O total público reúne as três redes antes de calcular o percentual.
- Os sete itens são esgoto, internet para aprendizagem, biblioteca ou sala de leitura, quadra, banheiro acessível, cozinha e refeitório. As carências de pelo menos 1 a 7 itens são contadas por escola antes da agregação. Os percentuais são cumulativos. A plataforma recebe as coberturas como insumo para a nota do I²E²F.
- `NA` indica valor indisponível. Uma escola com matrícula desconhecida fica fora da ponderação. Um item indeterminado em escola com peso positivo deixa indisponível a medida correspondente. Zero indica contagem ou percentual calculado igual a zero.

## Licença e validação externa

O CSV foi comparado com a planilha anual da v11. As 467.892 linhas e os 25 cabeçalhos conferem, incluindo a ordem das chaves, os zeros, os `NA` e os percentuais com tolerância absoluta de `1e-10`.

O código do grupo segue a licença [MIT](LICENSE). Os microdados e a população são publicados pelo Inep e pelo IBGE, respectivamente. Para reutilizar o CSV derivado, preserve a atribuição das fontes e consulte as condições de uso aplicáveis aos dados originais.

Antes de abrir o repositório ao público ou arquivar uma versão com DOI, uma pessoa de fora do grupo deve executar os três scripts em outro computador e conferir os resultados.
