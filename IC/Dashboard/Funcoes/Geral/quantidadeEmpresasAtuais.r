QuantidadeEmpresasAtuais <- function(evadidos) {
  quantidadeEmpresasAtuais <- empregos %>%
    select(Nome, Empresa) %>%
    filter(Nome %in% c(evadidos$Nome)) %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    group_by(Empresa) %>%
    summarise(Quantidade = n())
  return(dim(quantidadeEmpresasAtuais)[1])
}
