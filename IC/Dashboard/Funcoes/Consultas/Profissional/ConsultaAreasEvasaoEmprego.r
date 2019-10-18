ConsultaAreasEvasaoEmprego <- function(evadidos, siglaCurso) {
  # ==============================================================================
  # CONSULTA - AREA DO CURSO EVADIDO
  # ==============================================================================
  if (siglaCurso == "ADS") {
    area <- "INFORMACAO"
  }
  if (siglaCurso == "GPI") {
    area <- "PROCESSOS INDUSTRIAIS"
  }
  # ==============================================================================
  # CONSULTA - AREA DO ATUAL TRABALHO
  # ==============================================================================
  areaEmprego <- empregos %>%
    select(Nome, AreaCargo) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(evadidos$Nome)) %>%
    filter(row_number() == 1) %>%
    mutate(
      Trabalha = str_detect(area, AreaCargo),
      Trabalha = str_replace_all(Trabalha, "FALSE", "NÃO"),
      Trabalha = str_replace_all(Trabalha, "TRUE", "SIM")
    )
  areaEmprego <- na.omit(areaEmprego)

  return(areaEmprego)
}
