ConsultaProsseguiuEstudosAposEvasao <- function(evadidos, siglaCurso) {
  # ==============================================================================
  # CONSULTA - QUAIS ALUNOS EVADIDOS FIZERAM OUTRA CURSO APÓS A EVASÃO
  # ==============================================================================
  cursosAposEvasao <- estudos %>%
    inner_join(evadidos, by = "Nome") %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    select(Nome, Curso, Area, AnoIngresso, AnoEvasao) %>%
    na.omit(cursosAposEvasao) %>%
    mutate(
      Area = case_when(Curso == siglaCurso ~ "NAO", TRUE ~ Area),
      Area = case_when(AnoIngresso < AnoEvasao ~ "NAO", TRUE ~ Area),
      Area = case_when(AnoIngresso > AnoEvasao ~ "SIM", TRUE ~ Area),
      Area = case_when(AnoIngresso == AnoEvasao && Curso != siglaCurso ~ "SIM", TRUE ~ Area)
    )

  # AJUSTE NO DATAFRAME DA CONSULTA ACIMA PARA O GRÁFICO
  names(cursosAposEvasao)[3] <- "Continuou"

  # QUANTIDADE DE EGRESSOS QUE CONTINUARAM A ESTUDAR DEPOIS DA FORMAÇÃO
  quantidadeCursosAposEvasao <- cursosAposEvasao %>%
    group_by(Continuou) %>%
    summarise(Quantidade = n())

  # AJUSTE NO DATAFRAME DA CONSULTA ACIMA PARA O GRÁFICO
  TITULO <- c("FEZ ALGUM CURSO APÓS A EVASÃO?")
  SIM <- quantidadeCursosAposEvasao[, 2][2, ]
  NAO <- quantidadeCursosAposEvasao[, 2][1, ]
  quantidadeCursosAposEvasao <- data.frame(TITULO, SIM, NAO)
  names(quantidadeCursosAposEvasao) <- c("TITULO", "SIM", "NAO")
  return(quantidadeCursosAposEvasao)
}
