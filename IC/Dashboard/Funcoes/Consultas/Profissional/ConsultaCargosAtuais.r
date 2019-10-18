ConsultaCargosAtuais <- function(evadidos) {
  # ==============================================================================
  # BIBLIOTECAS
  # ==============================================================================
  library(gdata)
  # ==============================================================================
  # CONSULTA - ATUAIS CARGOS DOS EVADIDOS NOS ATUAIS EMPREGOS
  # ==============================================================================
  cargosAtuais <- empregos %>%
    select(Nome, Cargo) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(evadidos$Nome)) %>%
    filter(row_number() == 1)
  
  # ==============================================================================
  # CONSULTA - QUANTIDADE DE EVADIDOS POR CARGOS
  # ==============================================================================
  quantidadeCargosAtuais <- cargosAtuais %>%
    group_by(Cargo) %>%
    summarise(Quantidade = n()) %>% 
    filter(Quantidade >= 2)
}
