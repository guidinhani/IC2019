ConsultaCursosConcluidos <- function(evadidos, siglaCurso) {
  # ==============================================================================
  # CONSULTA - TODOS OS CURSOS FEITOS PELO ALUNOS EVADIDOS
  # ==============================================================================
  cursosConcluidos <- estudos %>%
    select(Nome, Curso) %>%
    na.omit(cursosConcluidos) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(evadidos$Nome)) %>%
    distinct(Curso, .keep_all = TRUE) %>%
    filter(Curso != siglaCurso) %>%
    mutate(
      Curso = str_squish(Curso)
    ) %>%
    group_by(Curso) %>%
    summarise(Quantidade = n()) %>%
    arrange(desc(Quantidade)) %>%
    group_by(Curso = factor(c(Curso[1:6], rep("OUTROS", n() - 6)),
      levels = c(Curso[1:6], "OUTROS")
    )) %>%
    tally(Quantidade) %>%
    rename(!!"Quantidade" := n)
}
