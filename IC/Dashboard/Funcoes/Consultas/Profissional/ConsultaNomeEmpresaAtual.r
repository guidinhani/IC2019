ConsultaNomeEmpresaAtual <- function(evadidos) {
  # ==============================================================================
  # CONSULTA - TIPO DA EMPRESA ATUAL DE TRABALHO DOS EVADIDOS
  # ==============================================================================
  empresaTrabalho <- empregos %>%
    select(Nome, Empresa, Publica) %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    filter(Nome %in% c(evadidos$Nome)) %>%
    mutate(
      Publica = str_replace_all(Publica, "FALSE", "PRIVADA"),
      Publica = str_replace_all(Publica, "TRUE", "PUBLICA")
    )
  # ==============================================================================
  # AJUSTE NO DATAFRAME
  # ==============================================================================
  names(empresaTrabalho)[3] <- "Tipo"
  empresaTrabalho[is.na(empresaTrabalho)] <- "SEM DADOS"
  return(empresaTrabalho)
}
