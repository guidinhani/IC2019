QuantidadeEvadidosEstudos <- function(evadidos) {
  quantidadeEvadidosEstudos <- estudos %>%
    inner_join(evadidos, by = "Nome") %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    summarise(Quantidade = n())

  return(dim(quantidadeEvadidosEstudos)[1])
}
