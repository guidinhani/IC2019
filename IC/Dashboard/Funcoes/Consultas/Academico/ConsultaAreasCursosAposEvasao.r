ConsultaAreasCursosAposEvasao <- function(evadidos, siglaCurso) {
  # ==============================================================================
  # CONSULTA - CURSOS REALIZADOS PELOS EVADIDOS APÓS A EVASÃO
  # ==============================================================================
  estudosAposEvasao <- estudos %>%
    inner_join(evadidos, by = "Nome") %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    na.omit(estudosAposEvasao) %>%
    select(Nome, Curso, Area, AnoIngresso, AnoEvasao) %>%
    filter((AnoIngresso == AnoEvasao && Curso != siglaCurso) || (AnoIngresso > AnoEvasao))
  # ==============================================================================
  # CONSULTA - QUANTIDADE DE PESSOAS POR TIPO DE CURSO APÓS A EVASÃO
  # ==============================================================================
  quantidadePessoasArea <- estudosAposEvasao %>%
    mutate(
      Area = str_squish(Area)
    ) %>%
    group_by(Area) %>%
    summarise(Quantidade = n()) %>%
    arrange(desc(Quantidade))
}
