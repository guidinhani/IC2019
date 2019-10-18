ConsultaAnoDeEvasao <- function(evadidos) {
  # ==============================================================================
  # CONSULTA - ANOS DE EVASÃO NO IFSP (CURSO DA GRADUACAO)
  # ==============================================================================
  anoDeEvasao <- evadidos %>%
    distinct(Nome, .keep_all = TRUE) %>%
    group_by(AnoEvasao) %>%
    summarise(Quantidade = n())
}
