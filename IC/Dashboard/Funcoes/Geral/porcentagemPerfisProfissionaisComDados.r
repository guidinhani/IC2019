PorcentagemPerfisProfissionaisComDados <- function(evadidos, siglaCurso) {
  quantidadeEvadidosEmpregos <- empregos %>%
    inner_join(evadidos, by = "Nome") %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    summarise(Quantidade = n())

  totalDeEvadidos <- TotalDeEvadidos(siglaCurso)
  return(round((dim(quantidadeEvadidosEmpregos)[1] / totalDeEvadidos) * 100, digits = 2))
}
