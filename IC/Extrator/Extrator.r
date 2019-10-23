# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# BIBLIOTECAS
# ==============================================================================
library(data.table)
library(dplyr)
library(stringr)
library(rvest)
library(furrr)
plan(multiprocess, workers = 6)
# ==============================================================================
# FUNÇÕES
# ==============================================================================
# ==============================================================================
# ESTUDOS
# ==============================================================================
ParseCurriculoEstudos <- function(arquivo) {

  # HTML
  print(arquivo)
  curriculo_html <- read_html(arquivo)

  # NOME
  curriculo_nome <- curriculo_html %>%
    html_node(css = ".pv-top-card-v3--list li") %>%
    html_text() %>%
    str_trim()

  # NOME
  if (is.na(curriculo_nome)) {
    curriculo_nome <- curriculo_html %>%
      html_node(css = "h1") %>%
      html_text() %>%
      str_trim()
  }

  # ESTUDOS
  curriculo_estudos <- curriculo_html %>% html_nodes(css = ".pv-education-entity")

  # ESCOLA
  lapply(curriculo_estudos, function(curriculo_estudo) {
    curriculo_estudo_escola <- curriculo_estudo %>%
      html_nodes(css = "h3") %>%
      html_text() %>%
      str_trim()
    if (length(curriculo_estudo_escola) == 0) {
      curriculo_estudo_escola <- NA
    }

    # CURSO
    curriculo_estudo_curso <- curriculo_estudo %>%
      html_nodes(css = ".pv-entity__degree-name span:nth-child(2)") %>%
      html_text() %>%
      str_trim()
    if (length(curriculo_estudo_curso) == 0) {
      curriculo_estudo_curso <- NA
    }

    # AREA DE ESTUDO
    curriculo_estudo_area <- curriculo_estudo %>%
      html_nodes(css = ".pv-entity__fos span:nth-child(2)") %>%
      html_text() %>%
      str_trim()
    if (length(curriculo_estudo_area) == 0) {
      curriculo_estudo_area <- NA
    }

    # DATA
    curriculo_estudo_data <- curriculo_estudo %>%
      html_nodes(css = ".pv-entity__dates span:nth-child(2)") %>%
      html_text() %>%
      str_trim()
    if (length(curriculo_estudo_data) == 0) {
      curriculo_estudo_data <- NA
    }

    data.frame(
      Nome = curriculo_nome,
      Escola = curriculo_estudo_escola,
      Curso = curriculo_estudo_curso,
      Area = curriculo_estudo_area,
      Data = curriculo_estudo_data,
      stringsAsFactors = FALSE
    )
  }) %>% rbindlist()
}

# ==============================================================================
# EMPREGOS
# ==============================================================================
ParseCurriculoEmpregos <- function(arquivo) {

  # HTML
  print(arquivo)
  curriculo_html <- read_html(arquivo)

  # NOME
  curriculo_nome <- curriculo_html %>%
    html_node(css = ".pv-top-card-v3--list li") %>%
    html_text() %>%
    str_trim()

  # NOME
  if (is.na(curriculo_nome)) {
    curriculo_nome <- curriculo_html %>%
      html_node(css = "h1") %>%
      html_text() %>%
      str_trim()
  }

  # EMPRESAS
  curriculo_empresas <- curriculo_html %>% html_nodes(css = ".pv-position-entity")

  # VERIFICA SE POSSUI MAIS DE UM CARGO NA EMPRESA
  lapply(curriculo_empresas, function(curriculo_empresa) {
    possui_varios_cargos <- curriculo_empresa %>%
      html_nodes(css = ".pv-entity__position-group") %>%
      length() > 0

    if (possui_varios_cargos) {
      ParseCurriculoEmpregosVariosCargos(curriculo_nome, curriculo_empresa)
    } else {
      ParseCurriculoEmpregosUnicoCargo(curriculo_nome, curriculo_empresa)
    }
  }) %>% rbindlist()
}

# EMPREGOS
ParseCurriculoEmpregosUnicoCargo <- function(curriculo_nome, curriculo_empresa) {

  # NOME DA EMPRESA
  curriculo_empresa_nome <- curriculo_empresa %>%
    html_nodes(css = ".pv-entity__secondary-title") %>%
    html_text() %>%
    str_trim()
  if (length(curriculo_empresa_nome) == 0) {
    curriculo_empresa_nome <- NA
  }

  # CARGO
  curriculo_empresa_cargo_nome <- curriculo_empresa %>%
    html_nodes(css = "h3") %>%
    html_text() %>%
    str_trim()
  if (length(curriculo_empresa_cargo_nome) == 0) {
    curriculo_empresa_cargo_nome <- NA
  }

  # CIDADE
  curriculo_empresa_cargo_cidade <- curriculo_empresa %>%
    html_nodes(css = ".pv-entity__location span:nth-child(2)") %>%
    html_text() %>%
    str_trim()
  if (length(curriculo_empresa_cargo_cidade) == 0) {
    curriculo_empresa_cargo_cidade <- NA
  }

  # DATA
  curriculo_empresa_cargo_data <- curriculo_empresa %>%
    html_nodes(css = ".pv-entity__date-range span:nth-child(2)") %>%
    html_text() %>%
    str_trim()
  if (length(curriculo_empresa_cargo_data) == 0) {
    curriculo_empresa_cargo_data <- NA
  }

  data.frame(
    Nome = curriculo_nome,
    Empresa = curriculo_empresa_nome,
    Cargo = curriculo_empresa_cargo_nome,
    Cidade = curriculo_empresa_cargo_cidade,
    Data = curriculo_empresa_cargo_data,
    stringsAsFactors = FALSE
  )
}

# EMPREGOS
ParseCurriculoEmpregosVariosCargos <- function(curriculo_nome, curriculo_empresa) {
  # MÚLTIPLOS CARGOS
  curriculo_empresa_cargos <- curriculo_empresa %>% html_nodes(css = ".pv-entity__position-group-role-item")
  if (length(curriculo_empresa_cargos) == 0) {
    curriculo_empresa_cargos <- curriculo_empresa %>% html_nodes(css = ".pv-entity__position-group-role-item-fading-timeline")
  }

  # NOME DA EMPRESA
  lapply(curriculo_empresa_cargos, function(curriculo_empresa_cargo) {
    curriculo_empresa_nome <- curriculo_empresa %>%
      html_node(css = "h3 span:nth-child(2)") %>%
      html_text() %>%
      str_trim()
    if (length(curriculo_empresa_nome) == 0) {
      curriculo_empresa_nome <- NA
    }

    # CARGO
    curriculo_empresa_cargo_nome <- curriculo_empresa_cargo %>%
      html_nodes(css = ".pv-entity__summary-info-v2 h3 span:nth-child(2)") %>%
      html_text() %>%
      str_trim()
    if (length(curriculo_empresa_cargo_nome) == 0) {
      curriculo_empresa_cargo_nome <- NA
    }

    # CIDADE
    curriculo_empresa_cargo_cidade <- curriculo_empresa_cargo %>%
      html_nodes(css = ".pv-entity__location span:nth-child(2)") %>%
      html_text() %>%
      str_trim()
    if (length(curriculo_empresa_cargo_cidade) == 0) {
      curriculo_empresa_cargo_cidade <- NA
    }

    # DATA
    curriculo_empresa_cargo_data <- curriculo_empresa_cargo %>%
      html_nodes(css = ".pv-entity__date-range span:nth-child(2)") %>%
      html_text() %>%
      str_trim()
    if (length(curriculo_empresa_cargo_data) == 0) {
      curriculo_empresa_cargo_data <- NA
    }

    data.frame(
      Nome = curriculo_nome,
      Empresa = curriculo_empresa_nome,
      Cargo = curriculo_empresa_cargo_nome,
      Cidade = curriculo_empresa_cargo_cidade,
      Data = curriculo_empresa_cargo_data,
      stringsAsFactors = FALSE
    )
  }) %>% rbindlist()
}

# ==============================================================================
# EXECUÇÃO
# ==============================================================================
curriculos.arquivosEvadidos <- list.files("curriculos_evadidosADSGPI", pattern = "*.html", full.names = TRUE)
curriculos.empregosEvadidos <- future_map_dfr(curriculos.arquivosEvadidos, ParseCurriculoEmpregos, .progress = TRUE)
curriculos.estudosEvadidos <- future_map_dfr(curriculos.arquivosEvadidos, ParseCurriculoEstudos, .progress = TRUE)
# ==============================================================================
# GRAVAÇÃO
# ==============================================================================
CSVs <- list.files("../Pré-processamento/", pattern = "*.csv", full.names = TRUE)
file.remove(CSVs)
write.csv(curriculos.empregosEvadidos, file = "../Pré-processamento/EmpregosEvadidos.csv", row.names = FALSE)
write.csv(curriculos.estudosEvadidos, file = "../Pré-processamento/EstudosEvadidos.csv", row.names = FALSE)
