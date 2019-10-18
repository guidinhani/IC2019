ConsultaCursosAposEvasao <- function(evadidos, siglaCurso) {
  # ==============================================================================
  # CONSULTA - ÚLTIMOS CURSOS REALIZADOS PELOS EVADIDOS DEPOIS DA EVASÃO
  # ==============================================================================
  estudosAposEvasao <- estudos %>%
    inner_join(evadidos, by = "Nome") %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    na.omit(estudosAposEvasao) %>%
    select(Nome, Curso, Area, AnoIngresso, AnoEvasao) %>%
    filter((AnoIngresso == AnoEvasao && Curso != siglaCurso) || (AnoIngresso > AnoEvasao))
  # QUANTIDADE DE PESSOAS POR TIPO DE CURSO APÓS A EVASÃO
  quantidadePessoasCurso <- estudosAposEvasao %>%
    mutate(
      Curso = str_squish(Curso)
    ) %>%
    group_by(Curso) %>%
    summarise(Quantidade = n()) %>%
    arrange(desc(Quantidade))
}
