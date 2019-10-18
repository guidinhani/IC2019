PorcentagemPerfisEncontrados <- function(evadidos, siglaCurso) {
  totalDeEvadidosEncontrados <- evadidos %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    summarise(Quantidade = n())

  totalDeEvadidos <- TotalDeEvadidos(siglaCurso)
  return(round((dim(totalDeEvadidosEncontrados)[1] / totalDeEvadidos) * 100, digits = 2))
}
