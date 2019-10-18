ConsultaLocalizacaoEmpresas <- function(evadidos) {
  # ==============================================================================
  # BIBLIOTECAS
  # ==============================================================================
  library(gdata)
  # ==============================================================================
  # CONSULTA - LOCALIZAÇÃO DAS EMPRESAS DOS ATUAIS EMPREGOS DOS EVADIDOS
  # ==============================================================================
  localEmpresas <- empregos %>%
    select(Nome, Cidade) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(evadidos$Nome)) %>%
    filter(row_number() == 1) %>%
    trim(localEmpresas)
  # ==============================================================================
  # CONSULTA - QUANTIADE DE EVADIDOS POR LOCALIZAÇÃO
  # ==============================================================================
  quantidadeLocalEmpresas <- localEmpresas %>%
    group_by(Cidade) %>%
    summarise(Quantidade = n())
}
