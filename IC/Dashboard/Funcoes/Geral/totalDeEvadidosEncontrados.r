TotalDeEvadidosEncontrados <- function(evadidos) {
  totalDeEvadidosEncontrados <- evadidos %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    summarise(Quantidade = n())

  return(dim(totalDeEvadidosEncontrados)[1])
}
