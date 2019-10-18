ConsultaTipoEmpresa <- function(evadidos) {
  # ==============================================================================
  # CONSULTA - TIPO DA EMPRESA ATUAL DE TRABALHO DOS EVADIDOS [PÚBLICA OU PRIVADA]
  # ==============================================================================
  empresaTrabalho <- empregos %>%
    select(Nome, Empresa, Publica) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(evadidos$Nome)) %>%
    filter(row_number() == 1) %>%
    mutate(
      Publica = str_replace_all(Publica, "FALSE", "PRIVADA"),
      Publica = str_replace_all(Publica, "TRUE", "PUBLICA")
    )
  
  names(empresaTrabalho)[3] <- "Tipo"
  empresaTrabalho[is.na(empresaTrabalho)] <- "SEM DADOS"
  # ==============================================================================
  # CONSULTA - QUANTIDADE DE EMPRESAS POR TIPO
  # ==============================================================================
  tipoDaEmpresa <- empresaTrabalho %>%
    select(Tipo) %>%
    group_by(Tipo) %>%
    summarise(Quantidade = n())
}
