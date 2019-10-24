# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# BIBLIOTECAS
# ==============================================================================
library(dplyr)
library(stringr)
library(readxl)
# ==============================================================================
# LEITURA E PRÉ-PROCESSAMENTO
# ==============================================================================
# ==============================================================================
# NOMES DOS EVADIDOS
# ==============================================================================
evadidosADS <- read_xlsx("EvadidosADS.xlsx") %>%
  # ==============================================================================
  # REMOÇÃO DE ACENTOS
  # ==============================================================================
  mutate(
    Nome = iconv(Nome, from = "UTF-8", to = "ASCII//TRANSLIT"),
    AnoEvasao = str_extract(AnoEvasao, "^\\d{4}"),
    # ==============================================================================
    # REMOVE ESPAÇOS EM BRANCO DO INICIO E DO FINAL
    # ==============================================================================
    Nome = str_trim(Nome, side = c("both")),
    Nome = str_squish(Nome)
  ) %>%
  mutate_all(str_to_upper)

evadidosGPI <- read_xlsx("EvadidosGPI.xlsx") %>%
  # ==============================================================================
  # REMOÇÃO DE ACENTOS
  # ==============================================================================
  mutate(
    Nome = iconv(Nome, from = "UTF-8", to = "ASCII//TRANSLIT"),
    AnoEvasao = str_extract(AnoEvasao, "^\\d{4}"),
    # ==============================================================================
    # REMOVE ESPAÇOS EM BRANCO DO INICIO E DO FINAL
    # ==============================================================================
    Nome = str_trim(Nome, side = c("both")),
    Nome = str_squish(Nome)
  ) %>%
  mutate_all(str_to_upper)
# ==============================================================================
# PADRONIZAÇÃO EMPREGOS
# ==============================================================================
empregos <- read.csv(file = "EmpregosEvadidos.csv", header = TRUE) %>%
  # ==============================================================================
  # REMOÇÃO DE ACENTOS
  # ==============================================================================
  mutate(
    Cargo = iconv(Cargo, to = "ASCII//TRANSLIT"),
    Cidade = iconv(Cidade, to = "ASCII//TRANSLIT"),
    Empresa = iconv(Empresa, to = "ASCII//TRANSLIT"),
    Nome = iconv(Nome, to = "ASCII//TRANSLIT"),
    # REMOVE ESPAÇOS EM BRANCO DO INICIO E DO FINAL
    Nome = str_trim(Nome, side = c("both")),
    Nome = str_squish(Nome)
  ) %>%
  # ==============================================================================
  # LETRAS MAIÚSCULAS
  # ==============================================================================
  mutate_all(str_to_upper) %>%
  # ==============================================================================
  # REMOÇÃO DE LINHAS PREENCHIDAS INCORRETAMENTE - SEM SOLUÇÃO
  # ==============================================================================
  filter(
    !str_detect(Empresa, ".*BUSCANDO RECOLOCACAO PROFISSIONAL.*|.*ANALISTA DE GESTAO DA QUALIDADE.*|^ESTAGIO$"),
    !str_detect(Cargo, ".*BRASIL KIRIN.*|.*ESTUDANTE.*|.*GESTAO DA PRODUCAO INDUSTRIAL.*|.*MEMBRO ESTUDANTE.*")
  ) %>%
  mutate(
    # ==============================================================================
    # REMOÇÃO DE TEXTOS INDESEJADOS
    # ==============================================================================
    # CARGO
    Cargo = str_replace_all(Cargo, "/| & ", " "),
    Cargo = str_replace_all(Cargo, "-|,", ""),
    Cargo = str_replace_all(Cargo, "JR.$|JR$|JUNIOR$|^JUNIOR|
                            |PL.$|PLENO$|PLENO I|
                            |SENIOR$|SN.$|
                            |\\(DEV1\\)|\\(SDCD\\)", ""),
    # CIDADE
    Cidade = str_replace_all(Cidade, "-|/", ""),
    Cidade = str_replace_all(Cidade, ", |/.*|, |/.*", " "),
    Cidade = str_replace_all(Cidade, "AREA, BRAZIL|AREA BRAZIL|AREA|BRAZIL|SP|
                             |E REGIAO.*|
                             |POLO SHOPPING|
                             |,.+(BRASIL|CAMPINAS)", ""),
    Cidade = str_replace_all(Cidade, ".*SALTO.*", "SALTO"),
    Cidade = str_replace_all(Cidade, "(.+)(SAO PAULO)", "\\1"),
    # EMPRESA
    Empresa = str_replace_all(Empresa, "\\.$| - ", " "),
    Empresa = str_replace_all(Empresa, ",|
                              |/SP|
                              |S\\.A|S\\.A\\.|
                              |\\(IFSP\\)|
                              |LTDA|INC|S/A", ""),
    # NOME
    Nome = str_replace_all(Nome, "\"", " "),
    Nome = str_replace_all(Nome, "PROFESSORA|INFORMATICA", ""),

    # ==============================================================================
    # CORREÇÃO DE PALAVRAS INCORRETAS
    # ==============================================================================
    Cargo = str_replace_all(Cargo, "ESTAGIARIOA", "ESTAGIARIO"),
    Cargo = str_replace_all(Cargo, "EGENHARIA", "ENGENHARIA"),
    Cargo = str_replace_all(Cargo, "ADMINSITRATIVO", "ADMINISTRATIVO"),
    Cargo = str_replace_all(Cargo, "LIGISTICA", "LOGISTICA"),
    Cargo = str_replace_all(Cargo, "MONATADOR", "MONTADOR"),

    # ==============================================================================
    # EMPRESAS
    # ==============================================================================
    # 0
    Empresa = str_replace_all(Empresa, ".*7IT.*", "7IT"),
    Empresa = str_replace_all(Empresa, ".*10DANCE.*", "10DANCE"),
    # A
    Empresa = str_replace_all(Empresa, ".*ABC BULL.*", "ABC BULL"),
    Empresa = str_replace_all(Empresa, ".*ACSN.*", "ASCN"),
    Empresa = str_replace_all(Empresa, ".*AGV LOGISTICA.*", "AGV LOGISTICA"),
    Empresa = str_replace_all(Empresa, ".*ACOKORTE.*", "ACOKORTE"),
    Empresa = str_replace_all(Empresa, ".*ALUFER.*", "ALUFER"),
    Empresa = str_replace_all(Empresa, ".*AMAZON.*", "AMAZON"),
    Empresa = str_replace_all(Empresa, ".*ACOP FILES.*", "ACOP FILES"),
    Empresa = str_replace_all(Empresa, ".*ASSOCIACAO EDUCACIONAL.+PEROLA", "ASSOCIACAO PEROLA"),
    Empresa = str_replace_all(Empresa, ".*ALLPET.*", "ALLPET"),
    Empresa = str_replace_all(Empresa, ".*EJ MAQUINAS E EQUIPAMENTOS/ MECANICA BONI/ WS FERRAMENTARIA.*", "AUTONOMO"),
    # B
    Empresa = str_replace_all(Empresa, ".*BRAFORCE.*", "BRAFORCE"),
    Empresa = str_replace_all(Empresa, "BANCO CARREFOUR.*", "BANCO CARREFOUR"),
    Empresa = str_replace_all(Empresa, ".*BENTELER.*", "BENTELER"),
    Empresa = str_replace_all(Empresa, ".*BIRO.*", "BIRO 2000"),
    Empresa = str_replace_all(Empresa, ".*BRASSUCO.*", "BRASSUCO"),
    Empresa = str_replace_all(Empresa, ".*BEETECH.*", "BEETECH"),
    Empresa = str_replace_all(Empresa, ".*SHINCARIOL.*", "BRASIL KIRIN"),
    # C
    Empresa = str_replace_all(Empresa, ".*CASA DO PRODUTOR RURAL.*", "CASA DO PRODUTOR RURAL"),
    Empresa = str_replace_all(Empresa, ".*COCA.+COLA.*", "COCA COLA"),
    Empresa = str_replace_all(Empresa, ".*COROLLARIUM.*", "COROLLARIUM"),
    Empresa = str_replace_all(Empresa, ".*CHARLES CAMBUR.*", "CHARLES CAMBUR"),
    Empresa = str_replace_all(Empresa, ".*COMET LEMASA.*", "COMET LEMASA"),
    Empresa = str_replace_all(Empresa, ".*COMPANHIA ITUANA DE SANEAMENTO.*", "COMPANHIA ITUANA DE SANEAMENTO"),
    Empresa = str_replace_all(Empresa, ".*CENTRO UNIVERSITARIO NOSSA SENHORA DO PATROCINIO.*", "CEUNSP"),
    # D
    Empresa = str_replace_all(Empresa, ".*DENTAL MORELLI.*", "DENTAL MORELLI"),
    Empresa = str_replace_all(Empresa, ".*DYNAPLAST.*", "DYNAPLAST"),
    Empresa = str_replace_all(Empresa, ".*DBUNIT.*", "DBUNIT"),
    Empresa = str_replace_all(Empresa, ".*DESPERTADOR DIGITAL.*", "DESPERTADOR DIGITAL"),
    # E
    Empresa = str_replace_all(Empresa, ".*E-DEPLOY.*", "E-DEPLOY"),
    Empresa = str_replace_all(Empresa, ".*EUCATEX.*", "EUCATEX"),
    Empresa = str_replace_all(Empresa, ".*EMFILS.*", "EMFILS"),
    Empresa = str_replace_all(Empresa, ".*ELICE IND.*", "ELICE"),
    Empresa = str_replace_all(Empresa, ".*EBX EXPRES.*", "EBX EXPRESS"),
    Empresa = str_replace_all(Empresa, ".*ESPECIFER.*", "ESPECIFER"),
    Empresa = str_replace_all(Empresa, ".*EXITNET.*", "EXITNET"),
    # F
    Empresa = str_replace_all(Empresa, ".*FACULDADE DE TECNOLOGIA.*|.*FATEC.*", "FATEC"),
    Empresa = str_replace_all(Empresa, ".*FIDELITY.*", "FIDELITY"),
    Empresa = str_replace_all(Empresa, ".*FERRAMENTAS GERAIS.*", "FERRAMENTAS GERAIS"),
    Empresa = str_replace_all(Empresa, ".*FIEC.*|.*FUNDACAO INDAIATUBANA DE EDUCACAO E CULTURA.*", "FIEC"),
    Empresa = str_replace_all(Empresa, ".*FREELANCER.*", "FREELANCER"),
    Empresa = str_replace_all(Empresa, ".*FLEXTEXTIL.*", "FLEXTEXTIL"),
    # G
    Empresa = str_replace_all(Empresa, ".*GRANOVA.*", "GRANOVA PRATA"),
    Empresa = str_replace_all(Empresa, ".*GREEND.*", "GREEND"),
    Empresa = str_replace_all(Empresa, ".*GRUPO APTI.*", "GRUPO APTI"),
    Empresa = str_replace_all(Empresa, ".*GRUPO DE ESTUDOS E PRATICAS EM OLERICULTURA.*|
                              |.*GEPOL.*", "GEPOL"),
    Empresa = str_replace_all(Empresa, ".*GUARANY.*", "GUARANY"),
    Empresa = str_replace_all(Empresa, ".*GUARNIERI.*", "GUARNIERI"),
    Empresa = str_replace_all(Empresa, ".*GEBAN.*", "GEBAN"),
    Empresa = str_replace_all(Empresa, ".*GUHRING.*", "GUHRING"),
    Empresa = str_replace_all(Empresa, ".*KION.*", "GRUPO KION"),
    # H
    Empresa = str_replace_all(Empresa, ".*HONDA.*", "HONDA"),
    Empresa = str_replace_all(Empresa, ".*HEINEKEN.*", "HEINEKEN"),
    Empresa = str_replace_all(Empresa, "HUZI MET", " HUZIMET"),
    Empresa = str_replace_all(Empresa, ".*HELPAY GRUPO.*", "HELPAY"),
    Empresa = str_replace_all(Empresa, ".*HI-LEX.*", "HI-LEX"),
    # I
    Empresa = str_replace_all(Empresa, "ISS.*", "ISS"),
    Empresa = str_replace_all(Empresa, ".*INSTITUTO DE ESTUDOS AVANCADOS|^IEAV$.*", "INSTITUTO DE ESTUDOS AVANCADOS"),
    Empresa = str_replace_all(Empresa, ".*IFSP", paste("IFSP", Cidade)),
    Empresa = str_replace_all(Empresa, ".*IFSP.+INSTITUTO FEDERAL.*|
                              |.*SAO PAULO FEDERAL INSTITUTE.*", paste("IFSP", Cidade)),
    Empresa = str_replace_all(Empresa, "^INSTITUTO FEDERAL.+SAO PAULO", paste("IFSP", Cidade)),
    Empresa = str_replace_all(Empresa, ".*INFOTURE.*", "INFOTURE"),
    Empresa = str_replace_all(Empresa, ".*ICMC.*", "ICMC"),
    # K
    Empresa = str_replace_all(Empresa, ".*KANJIKO.*|.*KANJICO.*", "KANJIKO"),
    # L
    Empresa = str_replace_all(Empresa, ".*LOJAS CEM.*", "LOJAS CEM"),
    Empresa = str_replace_all(Empresa, ".*LOJAS AMERICANAS.*", "LOJAS AMERICANAS"),
    # M
    Empresa = str_replace_all(Empresa, ".*MCDONALD.*", "MCDONALDS"),
    Empresa = str_replace_all(Empresa, ".*MAGNA.*", "MAGNA"),
    Empresa = str_replace_all(Empresa, ".*MICROCAMP.*", "MICROCAMP"),
    # N
    Empresa = str_replace_all(Empresa, ".*NISSIN FOODS.*", "NISSIN FOODS"),
    Empresa = str_replace_all(Empresa, ".*NACCO.*", "NACCO"),
    # O
    Empresa = str_replace_all(Empresa, ".*ORUOM.*", "ORUOM"),
    # P
    Empresa = str_replace_all(Empresa, "PREFEITURA DA ESTANCIA TURISTICA DE SALTO|
                              |PREFEITURA DE SALTO SP|.*PREFEITURA.+SALTO", "PREFEITURA DE SALTO"),
    Empresa = str_replace_all(Empresa, ".*PRINT ONE.*", "PRINT ONE"),
    Empresa = str_replace_all(Empresa, ".*PUROTEK.*", "PUROTEK"),
    Empresa = str_replace_all(Empresa, ".*PEPSICO.*", "PEPSICO"),
    Empresa = str_replace_all(Empresa, ".*PREFEITURA.+IPERO", "PREFEITURA DE IPERO"),
    Empresa = str_replace_all(Empresa, ".*PREFEITURA.+PORTO FELIZ", "PREFEITURA DE PORTO FELIZ"),
    Empresa = str_replace_all(Empresa, ".*PREFEITURA.+LIMEIRA", "PREFEITURA DE LIMEIRA"),
    Empresa = str_replace_all(Empresa, "^PREFEITURA$", paste("PREFEITURA DE", Cidade)),
    # R
    Empresa = str_replace_all(Empresa, ".*RENASOFT.*", "RENASOFT"),
    # S
    Empresa = str_replace_all(Empresa, ".*SABOO INDUSTRIA.*", "SABOO INDUSTRIA"),
    Empresa = str_replace_all(Empresa, ".*SEW-EURODRIVE.*|.*SEW EURODRIVE.*", "SEW-EURODRIVE"),
    Empresa = str_replace_all(Empresa, ".*SESI.*", "SESI"),
    Empresa = str_replace_all(Empresa, ".*SIMBAL.*", "SIMBAL"),
    Empresa = str_replace_all(Empresa, ".*SAFRA.*", "SAFRA"),
    Empresa = str_replace_all(Empresa, ".*SONDA.*", "SAFRA"),
    Empresa = str_replace_all(Empresa, ".*SENAI.*", "SENAI"),
    Empresa = str_replace_all(Empresa, ".*STEFANINI.*", "STEFANINI"),
    Empresa = str_replace_all(Empresa, ".*SECRETARIA ESTADUAL DE EDUCACAO DE SAO PAULO.*", "SECRETARIA ESTADUAL DE EDUCACAO"),
    # T
    Empresa = str_replace_all(Empresa, ".*TAXIVIRACOPOS.*", "TAXI VIRACOPOS"),
    Empresa = str_replace_all(Empresa, ".*TOYOTA.*", "TOYOTA"),
    Empresa = str_replace_all(Empresa, ".*TUCCA.*", "TUCCA"),
    Empresa = str_replace_all(Empresa, ".*TRIBUNAL REGIONAL DO TRABALHO.*", "TRT"),
    Empresa = str_replace_all(Empresa, ".*THS TELECOMUNICACOES.*", "THS TELECOMUNICACOES"),
    # U
    Empresa = str_replace_all(Empresa, ".*UPL.*", "UPL"),
    Empresa = str_replace_all(Empresa, ".*UNIITALO.*", "UNIITALO"),
    # V
    Empresa = str_replace_all(Empresa, ".*VENKO MOTORS.*", "VENKO MOTORS"),
    Empresa = case_when(str_detect(Empresa, "IFSP NA") ~ "IFSP", TRUE ~ Empresa),

    # ==============================================================================
    # CARGOS
    # ==============================================================================
    # A
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE LOGISTICA.*", "ANALISTA DE LOGISTICA"),
    Cargo = str_replace_all(Cargo, ".*INICIACAO CIENTIFICA.*|.*SCIENTIFIC INITIATION.*", "ALUNO DE INICIACAO CIENTIFICA"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE SISTEMAS.*|.*SYSTEM ANALYST.*", "ANALISTA DE SISTEMAS"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE SUPORTE.*|.*SUPORT ANALYSTY.*", "ANALISTA DE SUPORTE"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE TESTE.*", "ANALISTA DE TESTE"),
    Cargo = str_replace_all(Cargo, ".*ASSISTENTE EM ADMINISTRACAO.*|.*ASSISTANT ADMINISTRATIVE.*|.*ASSISTENTE ADMINISTRATIVO.*|
                            |.*AUX ADM.*|.*AUX.+ADM.*", "ASSISTENTE ADMINISTRATIVO"),
    Cargo = str_replace_all(Cargo, ".*SOCIAL MEDIA ANALYST.*", "ANALISTA DE MIDIA SOCIAL"),
    Cargo = str_replace_all(Cargo, ".*ASSISTENTE TECNICO.*", "ASSISTENTE TECNICO"),
    Cargo = str_replace_all(Cargo, ".*CONTROLLING ANALYST.*|.*ANALISTA DE CONTROLADORIA.*", "ANALISTA DE CONTROLE"),
    Cargo = str_replace_all(Cargo, ".*FRAUD ANALYST.*|.*FRAUD RISK.*", "ANALISTA DE FRAUDE"),
    Cargo = str_replace_all(Cargo, ".*FULFILLMENT.+ANALYST.*", "ANALISTA DE CUMPRIMENTO"),
    Cargo = str_replace_all(Cargo, ".*FINANCIAL PLANNING ANALYST.*", "ANALISTA DE PLANEJAMENTO FINANCEIRO"),
    Cargo = str_replace_all(Cargo, ".*FINANCIAL ANALYST COST.*", "ANALISTA DE CUSTO FINANCEIRO"),
    Cargo = str_replace_all(Cargo, ".*PROCESS ANALYST.*", "ANALISTA DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*PROCESS ASSISTANT.*", "ASSISTENTE DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*SUPPORT ASSISTANT.*", "ASSISTENTE DE SUPORTE"),
    Cargo = str_replace_all(Cargo, ".*SYSTEM AUDITS.*", "AUDITOR DE SISTEMA"),
    Cargo = str_replace_all(Cargo, ".*ASSISTENTE DE PLANEJAMENTO.*", "ASSISTENTE DE PLANEJAMENTO"),
    Cargo = str_replace_all(Cargo, "ADMINISTRADOR.*", "ADMINISTRADOR"),
    Cargo = str_replace_all(Cargo, ".*ALMOXARIFE.*", "ALMOXARIFE"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DA QUALIDADE.*", "ANALISTA DA QUALIDADE"),
    Cargo = str_replace_all(Cargo, "^ANALISTA DE$", "ANALISTA"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE PROGRAMA DE MELHORIA.*", "ANALISTA DE PROGRAMA DE MELHORIAS"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE TREINAMENTO E DESENVOLVIMENTO.*", "ANALISTA DE TREINAMENTO E DESENVOLVIMENTO"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA.*PCP.*", "ANALISTA DE PCP"),
    Cargo = str_replace_all(Cargo, ".*ASS. TECNICO DA QUALIDADE.*", "ASSISTENTE TECNICO DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, ".*ASSISTANT TOUR AGENT.*", "ASSISTENTE DE AGENTE DE PASSEIO"),
    Cargo = str_replace_all(Cargo, ".*ASSISTENCIA TECNICA.*", "ASSISTENCIA TECNICA"),
    Cargo = str_replace_all(Cargo, ".*ATENDENTE I.*", "ATENDENTE"),
    Cargo = str_replace_all(Cargo, ".*AUX. DE PRODUCAO.*|.*AUXILIAR DE PRODUCAO.*", "AUXILIAR DE PRODUCAO"),
    Cargo = str_replace_all(Cargo, ".*AUXILIAR DE LABORATORIO.*", "AUXILIAR DE LABORATORIO"),
    Cargo = str_replace_all(Cargo, ".*AUXILIAR DE CONFERENTE DE CARGAS.*", "AUXILIAR CONFERENTE DE CARGAS"),
    Cargo = str_replace_all(Cargo, ".*STRATEGIC MARKETING ANALYST.*", "ANALISTA ESTRATEGICO DE MARKETING"),
    Cargo = str_replace_all(Cargo, ".*SOFTWARE ARCHITECT.*", "ARQUITETO DE SOFTWARE"),
    Cargo = str_replace_all(Cargo, ".*PRODUCT DEVELOPMENT ASSISTANT.*", "ASSISTENTE DE DESENVOLVIMENTO DE PRODUTOS"),
    Cargo = str_replace_all(Cargo, ".*PHOTOGRAPHER ASSISTANT.*", "ASSISTENTE DE FOTOGRAFO"),
    # B
    Cargo = str_replace_all(Cargo, ".*BIOMEDICO.*", "BIOMEDICO"),
    # C
    Cargo = str_replace_all(Cargo, ".*SYSTEM DEVELOPMENT COORDINATOR.*", "COORDENADOR DE DESENVOLVIMENTO DE SISTEMAS"),
    Cargo = str_replace_all(Cargo, ".*WAREHOUSE CONTROLLER.*", "CONTROLADOR DE ARMAZEM"),
    Cargo = str_replace_all(Cargo, ".*CONSULTOR.+(AVON|NATURA)|.*SALES CONSULTANT.*|.*SALES REPRESENTATIVE.*", "CONSULTOR DE VENDAS"),
    Cargo = str_replace_all(Cargo, ".*TAPERA.*", "CAPITA DA EQUIPE TAPERA BABY"),
    Cargo = str_replace_all(Cargo, ".*HEAD OF NEW BUSINESS.*", "CHEFE DE NOVOS NEGOCIOS"),
    Cargo = str_replace_all(Cargo, ".*COMPRADOR.*", "COMPRADOR"),
    Cargo = str_replace_all(Cargo, ".*ACCOUNTANT.*", "CONTADOR"),
    Cargo = str_replace_all(Cargo, ".*COFUNDADOR.*", "COFUNDADOR"),
    # D
    Cargo = str_replace_all(Cargo, ".*DESIGNER GRAFICO.*", "DESIGNER GRAFICO"),
    Cargo = str_replace_all(Cargo, ".*DIAGRAMADOR.*", "DIAGRAMADOR"),
    Cargo = str_replace_all(Cargo, ".*DOCUMENTACAO.*", "DOCUMENTADOR"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA/DESENVOLVEDOR.*|.*PROGRAMMER.*|PROGRAMADORA ANALISTA|^PROGRAMADOR$|
                            |PROGRAMADOR DE SISTEMAS|.*DESENVOLVEDOR DE SISTEMAS.*|.*SYSTEM DEVELOPER.*|.*APPLICATION DEVELOPER.*|
                            |^DESENVOLVEDOR$", "DESENVOLVEDOR DE SOFTWARE"),
    Cargo = str_replace_all(Cargo, ".*DESENVOLVEDOR JAVA.*|.*JAVA DEVELOPER.*|.*PROGRAMADOR JAVA.*|.*JAVA.*", "DESENVOLVEDOR JAVA"),
    Cargo = str_replace_all(Cargo, ".*DESENVOLVEDOR DE WEB.*|PROGRAMADOR WEB|.*DESENVOLVEDOR.*WEB.*", "DESENVOLVEDOR WEB"),
    Cargo = str_replace_all(Cargo, ".*SHAREPOINT DEVELOPER.*", "DESENVOLVEDOR SHAREPOINT"),
    Cargo = str_replace_all(Cargo, ".*SOFTWARE DEVELOPER.*", "DESENVOLVEDOR DE SOFTWARE"),
    Cargo = str_replace_all(Cargo, ".*MOBILE DEVELOPER.*", "DESENVOLVEDOR DE SOFTWARE MOVEL"),
    Cargo = str_replace_all(Cargo, ".*UI UX DESIGNER.*", "DESIGN DE INTERFACE/EXPERIENCIA DO USUARIO"),
    # E
    Cargo = str_replace_all(Cargo, ".*INTERNACIONAL LOGISTICS INTERN.*", "ESTAGIARIO DE LOGISTICA INTERNACIONAL"),
    Cargo = str_replace_all(Cargo, ".*INTERNENVIRONMENT.*", "ESTAGIARIO DE AMBIENTE"),
    Cargo = str_replace_all(Cargo, ".*MECHANICAL TRAINEE.*", "ESTAGIARIO MECANICO"),
    Cargo = str_replace_all(Cargo, ".*QUALITY ASSURANCE INTERN.*", "ESTAGIARIO DE GARANTIA DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, "^INTERNSHIP$|^TRAINEE$|^INTERN$", "ESTAGIARIO"),
    Cargo = str_replace_all(Cargo, ".*FINANCIAL INTERN.*", "ESTAGIARIO DE FINANCAS"),
    Cargo = str_replace_all(Cargo, ".*CUSTOMER QUALITY ENGINEER.*|.*ENGENHEIRO DE QUALIDADE.*", "ENGENHEIRO DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, ".*DATA CENTER OPERATIONS INTERN.*", "ESTAGIARIO DE OPERACOES DO DATA CENTER"),
    Cargo = str_replace_all(Cargo, ".*PROCESS ENGINEER INTERNSHIP.*", "ESTAGIARIO DE ENGENHARIA DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*ADMINISTRATIVE INTERN.*", "ESTAGIARIO ADMINISTRATIVO"),
    Cargo = str_replace_all(Cargo, ".*ESTAGIO.*|.*ESTAGIARIA.*", "ESTAGIARIO"),
    Cargo = str_replace_all(Cargo, "ESTAGIARIO DE TECNOLOGIA DA INFORMACAO|ESTAGIARIO DESENVOLVEDOR WEB|IT TRAINEE|
                            |ESTAGIARIO\\(PROGRAMADOR\\)|ESTAGIO DE PROGRAMACAO|ESTAGIO EM ANALISE E DESENVOLVIMENTO DE SISTEMAS|
                            |.*IT INTERN.*", "ESTAGIARIO DE TI"),
    Cargo = str_replace_all(Cargo, ".*IT SPECIALIST.*", "ESPECIALISTA EM TI"),
    Cargo = str_replace_all(Cargo, ".*OPERATIONAL SPECIALIST.*", "ESPECIALISTA OPERACIONAL"),
    Cargo = str_replace_all(Cargo, ".*SOFTWARE ENGINEER.*|.*QA AUTOMATION INTERN.*", "ENGENHEIRO DE SOFTWARE"),
    Cargo = str_replace_all(Cargo, ".*ENGENHARIA DE PROCESSOS.*", "ENGENHARIA DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*SUPPORT ENGINEER.*", "ENGENHEIRO DE SUPORTE"),
    Cargo = str_replace_all(Cargo, ".*ENGENHEIRO CIVIL.*|.*ENGENHEIRA CIVIL.*", "ENGENHEIRO CIVIL"),
    Cargo = str_replace_all(Cargo, ".*FINANCE INTERN.*|.*FINANCE TRAINEE.*", "ESTAGIARIO DE FINANCAS"),
    Cargo = str_replace_all(Cargo, ".*INTERN IN PROCESS ENGINEERING.*", "ESTAGIARIO EM ENGENHARIA DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*LOGISTICS SPECIALIST.*", "ESPECIALISTA EM LOGISTICA"),
    Cargo = str_replace_all(Cargo, ".*WEALTH MANAGEMENT SPECIALIST.*", "ESPECIALISTA EM GESTAO DE RIQUEZA"),
    Cargo = str_replace_all(Cargo, "^AMT$", "ENGENHEIRO DE MANUTENCAO DE AERONAVES"),
    Cargo = str_replace_all(Cargo, ".*TRAINEE TECNICO.*", "ESTAGIARIO TECNICO"),
    Cargo = str_replace_all(Cargo, ".*PROCESS EXECUTIVE.*", "EXECUTIVO DE PROCESSO"),
    # F
    Cargo = str_replace_all(Cargo, ".*FREELANCER.*|.*FREE LANCER.*", "FREELANCER"),
    Cargo = str_replace_all(Cargo, ".*TOOLMAKER.*|.*TOOL MAKER.*", "FABRICANTE DE FERRAMENTAS"),
    Cargo = str_replace_all(Cargo, ".*FERRAMENTEIRO ECLETICO.*", "FERRAMENTEIRO ECLETICO"),
    Cargo = str_replace_all(Cargo, ".*FRESADOR FERRAMENTEIRO.*", "FRESADOR FERRAMENTEIRO"),
    # G
    Cargo = str_replace_all(Cargo, ".*PROJECT MANAGEMENT OEM.*", "GESTOR DE PROJETOS OEM"),
    Cargo = str_replace_all(Cargo, ".*RACIO PROJECT MANAGEMENT.*", "GESTOR DE PROJETOS RACIO"),

    Cargo = str_replace_all(Cargo, ".*ACCOUNT MANAGER.*|.*GERENTE DE CONTAS.*", "GERENTE DE CONTAS"),
    Cargo = str_replace_all(Cargo, ".*IT MANAGER.*", "GERENTE DE TI"),
    # I
    Cargo = str_replace_all(Cargo, ".*CODE INSTRUCTOR.*|.*INSTRUTOR DE CODIGO.*", "INSTRUTOR DE CODIGO"),
    Cargo = str_replace_all(Cargo, ".*QUALITY INSPECTOR.*|.*GESTAO DA QUALIDADE.*|.*INSPETOR DE QUALIDADE.*|
                            |.*INSPETOR DE CONTROLE DE QUALIDADE.*|.*QUALITY CONTROL.*|.*QUALITY ASSURANCE ANALYST.*|
                            |.*CONTROLE DE QUALIDADE.*", "INSPETOR DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, ".*INSTRUTOR DE PRATICA.*PROFISSIONA.*", "INSTRUTOR DE PRATICAS PROFISSIONAIS"),
    # L
    Cargo = str_replace_all(Cargo, ".*PROJECT LEAD.*|.*TEAM LEADER.*", "LIDER DE PROJETOS"),
    Cargo = str_replace_all(Cargo, ".*LIDER DE EQUIPE.*|.*LIDER DE GRUPO.*", "LIDER DE EQUIPE"),
    Cargo = str_replace_all(Cargo, ".*LEAD GAME DESIGENR.*", "LIDER DE DESIGN"),
    # M
    Cargo = str_replace_all(Cargo, ".*MECANICO DE MANUTENCAO.*|.*MECANICO.*MANUTENCAO.*", "MECANICO DE MANUTENCAO"),
    Cargo = str_replace_all(Cargo, ".*MECANICO DE REFRIGERACAO.*", "MECANICO DE REFRIGERACAO"),
    Cargo = str_replace_all(Cargo, ".*METROLOGIST.*", "METROLOGIA"),
    Cargo = str_replace_all(Cargo, "MONTADOR[A]?", "MONTADOR"),
    # O
    Cargo = str_replace_all(Cargo, ".*WIRE EDM OPERATOR.*", "OPERADOR DE ELETROEROSAO"),
    Cargo = str_replace_all(Cargo, ".*OPERADOR DE MAQUINA.*", "OPERADOR DE MAQUINA"),
    Cargo = str_replace_all(Cargo, ".*OPERADOR DE PRODUCAO.*", "OPERADOR DE PRODUCAO"),
    Cargo = str_replace_all(Cargo, ".*GENERAL OPERATIVE.*", "OPERATIVA GERAL"),
    Cargo = str_replace_all(Cargo, ".*INJECTION MACHINE OPERATOR.*", "OPERADOR DE MAQUINA DE INJECAO"),
    Cargo = str_replace_all(Cargo, ".*OPERADOR DE PROCESSO.*", "OPERADOR DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*OPERADOR CNC.*", "OPERADOR CNC"),
    Cargo = str_replace_all(Cargo, ".*OPERADOR DE EMPILHADEIRA.*", "OPERADOR DE EMPILHADEIRA"),
    # P
    Cargo = str_replace_all(Cargo, ".*RESEARCH SCIENTIST.*|.*RESEARCHER SCIENTIST.*", "PESQUISADOR CIENTIFICO"),
    Cargo = str_replace_all(Cargo, ".*TI GRADUATE PROGRAM.*", "PROGRAMA DE GRADUACAO EM TI"),
    Cargo = str_replace_all(Cargo, ".*PROGRAMADOR.+(CNC.*|CONTROLE NUMERICO.*).*", "PROGRAMADOR CNC"),
    Cargo = str_replace_all(Cargo, ".*PROFESSOR.*", "PROFESSOR"),
    Cargo = str_replace_all(Cargo, ".*COIN PROCESSOR.*", "PROCESSADOR DE MOEDA"),
    Cargo = str_replace_all(Cargo, ".*EUROPEAN PROJECT SEMESTER.*", "PROJETO EUROPEU DO SEMESTRE"),
    Cargo = str_replace_all(Cargo, ".*INTERNATIONAL COLLEGE PROGRAM.*", "PROGRAMA INTERNACIONAL DA FACULDADE"),
    # S
    Cargo = str_replace_all(Cargo, "IT SUPPORT.*", "SUPORTE DE TI"),
    Cargo = str_replace_all(Cargo, ".*SHIFT SUPERVISOR*", "SUPERVISOR DE TURNO"),
    Cargo = str_replace_all(Cargo, "^SUPORTE.*", "SUPORTE"),
    Cargo = str_replace_all(Cargo, ".*QC SUPERVISOR.*", "SUPERVISOR DE CONTROLE DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, ".*CUSTOMER SERVICE AND LOGISTICS SUPERVISOR.*", "SUPERVISOR DE SERVICO"),
    Cargo = str_replace_all(Cargo, ".*SUPERVISOR DE PLANEJAMENTO.*", "SUPERVISOR DE PLANEJAMENTO"),
    # T
    Cargo = str_replace_all(Cargo, ".*TEC. ELETROELETRONICO.*", "TECNICO ELETROELETRONICO"),
    Cargo = str_replace_all(Cargo, ".*MAINTENANCE TECHNICIAN.*", "TECNICO DE MANUTENCAO"),
    Cargo = str_replace_all(Cargo, ".*QUALITY ASSURANCE TESTER.*", "TESTADOR DE GARANTIA DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, ".*DATA CENTER OPERATIONS TECHNICIAN.*", "TECNICO DE OPERACOES DO DATA CENTER"),
    Cargo = str_replace_all(Cargo, ".*PROCESS TECHNICIAN.*", "TECNICO DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*TEC. MULTIFUNCIONAL DE MANUTENCAO.*|.*TECNICO DE MANUTENCAO MECANICA.*", "TECNICO DE MANUTENCAO"),
    Cargo = str_replace_all(Cargo, ".*TECNICO DE PRODUCAO.*", "TECNICO DE PRODUCAO"),
    Cargo = str_replace_all(Cargo, ".*TECNICO MECANICO.*", "TECNICO MECANICO"),
    Cargo = str_replace_all(Cargo, ".*TECHNICAL ANALYST.*|TECNICO ANALISTA", "TECNICO ANALISTA"),
    Cargo = str_replace_all(Cargo, ".*TECNICO (DE|EM) INFORMATICA.*", "TECNICO EM INFORMATICA"),
    Cargo = str_replace_all(Cargo, "^SDET$|^TESTER$", "TESTADOR"),
    Cargo = str_replace_all(Cargo, "T\\.I\\.$|T\\.I$", "TI"),
    Cargo = str_replace_all(Cargo, ".*TECNICO.+INFORMATICA", "TECNICO EM INFORMATICA"),
    # V
    Cargo = str_replace_all(Cargo, ".*SALES OEM.*", "VENDEDOR"),

    # ==============================================================================
    # CIDADES
    # ==============================================================================
    # A
    Cidade = str_replace_all(Cidade, ".*ARTANBURG.*", "ARTANBURG"),
    # C
    Cidade = str_replace_all(Cidade, ".*VIRACOPOS.*", "CAMPINAS"),
    # D
    Cidade = str_replace_all(Cidade, ".*DUBLIN.*", "DUBLIN"),
    # G
    Cidade = str_replace_all(Cidade, ".*GUARULHOS.*", "GUARULHOS"),
    # I
    Cidade = str_replace_all(Cidade, "3A VARA CIVEL DA COMARCA DE ITU|EXERCITO BRASILEIRO ITU|^ITU.*", "ITU"),
    # N
    Cidade = str_replace_all(Cidade, ".*NEW YORK.*", "NOVA IORQUE"),
    # O
    Cidade = str_replace_all(Cidade, ".*ONTARIO.*", "ONTARIO"),
    Cidade = str_replace_all(Cidade, ".*ORLANDO.*", "ORLANDO"),
    # P
    Cidade = str_replace_all(Cidade, ".*POZNAN.*", "POSNANIA"),
    # S
    Cidade = str_replace_all(Cidade, ".*SAO JOSE DOS CAMPOS.*", "SAO JOSE DOS CAMPOS"),
    Cidade = str_replace_all(Cidade, "AVENIDA DO ESTADO 5533", "SAO PAULO"),
    Cidade = str_replace_all(Cidade, "AVENIDA ENGENHEIRO CARLOS REINALDO.*", "SOROCABA"),

    # ==============================================================================
    # COMPLENTANDO NOMES DAS CIDADES NÃO ESPECÍFICADAS PELOS NOME DAS EMPRESAS
    # ==============================================================================
    # AGUAS DE LINDOIA
    Cidade = case_when(str_detect(Empresa, "MARCENARIA INNOVA") ~ "AGUAS DE LINDOIA", TRUE ~ Cidade),
    # AMERICANA
    Cidade = case_when(str_detect(Empresa, "PEOPLE COMPUTACAO") ~ "AMERICANA", TRUE ~ Cidade),
    # BRISBANE
    Cidade = case_when(str_detect(Empresa, ".*ADVANCE QUEENSLAND.*") ~ "BRISBANE", TRUE ~ Cidade),
    # BARUERI
    Cidade = case_when(str_detect(Empresa, ".*E-DEPLOY.*|.*TITAN SOFTWARE.*|ODONTOPREV|INMETRICS") ~ "BARUERI", TRUE ~ Cidade),
    # CAMPINAS
    Cidade = case_when(str_detect(Empresa, ".*EVOLUTTO.*|.*THE GIDEONS INTERNATIONAL.*|.*MATERA SYSTEMS.*|.*VIRACOPOS.*|
                                  |.*HITSS.*|CTI RENATO ARCHER|JR CONSULTORIA|.*CAMPINAS$") ~ "CAMPINAS", TRUE ~ Cidade),
    # CASA BRANCA
    Cidade = case_when(str_detect(Empresa, ".*MASSEY FERGUSON.*") ~ "CASA BRANCA", TRUE ~ Cidade),
    # CABREÚVA
    Cidade = case_when(str_detect(Empresa, ".*METALURGICA NAKAYONE .*") ~ "CABREUVA", TRUE ~ Cidade),
    # CURITIBA
    Cidade = case_when(str_detect(Empresa, "NUNESFARMA DISTRIBUIDORA FARMACEUTICA") ~ "CURITIBA", TRUE ~ Cidade),
    # DUBLIN
    Cidade = case_when(str_detect(Empresa, ".*DIGITAL CREW.*") ~ "DUBLIN", TRUE ~ Cidade),
    # ERECHIN
    Cidade = case_when(str_detect(Empresa, "INTECNIAL S.A") ~ "ERECHIN", TRUE ~ Cidade),
    # GASPAR
    Cidade = case_when(str_detect(Empresa, ".*ALUMETAF.*") ~ "GASPAR", TRUE ~ Cidade),
    # GUARULHOS
    Cidade = case_when(str_detect(Empresa, "SUPERSINGLE PNEUS") ~ "GUARULHOS", TRUE ~ Cidade),
    # HORTOLANDIA
    Cidade = case_when(str_detect(Empresa, ".*IBM.*") ~ "HORTOLANDIA", TRUE ~ Cidade),
    # HEELERN
    Cidade = case_when(str_detect(Empresa, "DSM") ~ "HEERLEN", TRUE ~ Cidade),
    # INDAIATUBA
    Cidade = case_when(str_detect(Empresa, ".*BIRO 2000.*|.*BRIO INFORMATICA.*|.*STEFANINI.*|.*BALILLA FIAT.*|
                                  |.*SEW-EURODRIVE.*|.*SPECIAL PACK.*|.*ESPECIFER.*|.*HUMMEL GROUP.*|
                                  |.*TUBERFIL.*|INSTITUTO TERAPEUTICO DELTA|RR64 INTELIGENCIA EM REDES|
                                  |COLEGIO META") ~ "INDAIATUBA", TRUE ~ Cidade),
    # ITU
    Cidade = case_when(str_detect(Empresa, ".*HEINEKEN.*|.*FLUIDO DIGITAL.*|.*ITU GARDEN SPA.*|.*STARRETT.*|.*EMFILS.*|.*FIDELITY.*|
                                  |BRASIL KIRIN|MHG ENGENHARIA|.*ITU$|FIS") ~ "ITU", TRUE ~ Cidade),
    # LENCOIS PAULISTA
    Cidade = case_when(str_detect(Empresa, "EXITNET") ~ "LENCOIS PAULISTA", TRUE ~ Cidade),
    # MUMBAI
    Cidade = case_when(str_detect(Empresa, ".*BEHUMAN FOUNDATION.*") ~ "MUMBAI", TRUE ~ Cidade),
    # JUNDIAI
    Cidade = case_when(str_detect(Empresa, ".*FOXCONN|.*EXPRESSO NEPOMUCENO.*|.*GOLDNET.*") ~ "JUNDIAI", TRUE ~ Cidade),
    # PINHAIS
    Cidade = case_when(str_detect(Empresa, ".*INFOCASE INFORMATICA.*") ~ "PINHAIS", TRUE ~ Cidade),
    # PIRACICABA
    Cidade = case_when(str_detect(Empresa, "JR CARTUCHOS") ~ "PIRACICABA", TRUE ~ Cidade),
    # PORTO FELIZ
    Cidade = case_when(str_detect(Empresa, ".*COOPER POWER SYSTEMS.*") ~ "PORTO FELIZ", TRUE ~ Cidade),
    # RIO DE JANEIRO
    Cidade = case_when(str_detect(Empresa, "GRUPO PETROPOLIS|.*EBX.*") ~ "RIO DE JANEIRO", TRUE ~ Cidade),
    # RIBEIRAO PRETO
    Cidade = case_when(str_detect(Empresa, "TEKPRINT INFORMATICA") ~ "RIBEIRAO PRETO", TRUE ~ Cidade),
    # SALTO
    Cidade = case_when(str_detect(Empresa, ".*MICROCAMP.*|.*DATA 1000 INFORMATICA.*|.*SALTOS ALIMENTOS.*|.*AUTO GERAL.*|.*CASA & BASE.*|
                                  |.*DELOGIC.*|.*GRANOVA PRATA.*|.*PREFEITURA.+SALTO.*|.*IFSP SALTO.*|
                                  |.*TAPERA BABY.*|ACT FOTOGRAFIA|CONTINENTAL|KANJIKO|
                                  |EUCATEX|CONFECCAO C.N|HUZIMET|.*SALTO$|.*SULBRAS.*|
                                  |GOLDEN POOL PISCINAS|TANCREDO DO AMARAL") ~ "SALTO", TRUE ~ Cidade),
    # SALTO DE PIRAPORA
    Cidade = case_when(str_detect(Empresa, "CELMARTHE") ~ "SALTO DE PIRAPORA", TRUE ~ Cidade),
    # SAO BERNARDO
    Cidade = case_when(str_detect(Empresa, ".*MOBITEL S/A.*") ~ "SAO BERNARDO", TRUE ~ Cidade),
    # SAO CARLOS
    Cidade = case_when(str_detect(Empresa, "MONITORA SOLUCOES TECNOLOGICAS") ~ "SAO CARLOS", TRUE ~ Cidade),
    # SAO PAULO
    Cidade = case_when(str_detect(Empresa, ".*MICROPRO.*|.*MILLENIUM INFORMATICA.*|.*BASE2 TECNOLOGIA.*|.*COMPWAY INFORMATICA.*|
                                  |.*ESX.*|.*TREVO SEGURADORA S/A.*|.*DIADEIS.*|.*GOVERNO DO ESTADO DE SAO PAULO.*|
                                  |.*BLITSI.*|.*EDITORA IBEP.*|.*FAPESP.*|PMESP|TUCCA|.*SAO PAULO$|
                                  |SABESP|MEG FORMACAO PROFISSIONAL|CIAGROUP|FELICIA BUFFET|
                                  |GRUPO ACERT") ~ "SAO PAULO", TRUE ~ Cidade),
    # SOROCABA
    Cidade = case_when(str_detect(Empresa, ".*BOSCH.*|.*ZF LEMFORDER.*|GFT|BORGES E RODRIGUES ADVOGADOS|.*CJA ADVOGADOS.*") ~ "SOROCABA", TRUE ~ Cidade),
    # TABOAO DA SERRA
    Cidade = case_when(str_detect(Empresa, ".*HELPAY.*|.*ALL NET.*|.*ADVANCE TECNOLOGIA.*") ~ "TABOAO DA SERRA", TRUE ~ Cidade),
    # TATUÍ
    Cidade = case_when(str_detect(Empresa, "7IT|.*TATUI.*|SEGURANCA MAIS|GALAXIA BRINQUEDOS") ~ "TATUI", TRUE ~ Cidade),

    # ==============================================================================
    # VERIFICA SE A EMPRESA É PÚBLICA OU PRIVADA
    # ==============================================================================
    Publica = str_detect(Empresa, "(.*IFSP.*|.*PREFEITURA.*|.*TRIBUNAL.*|.*ESTADUAL.*|^TRT$)"),

    # ==============================================================================
    # ÁREAS DOS CARGOS
    # ==============================================================================
    AreaCargo = Cargo,
    # AUTONOMO
    AreaCargo = str_replace_all(AreaCargo, ".*FREELANCER.*|.*AUTONOMO.*", "AUTONOMO"),
    # ADMINISTRAÇÃO
    AreaCargo = str_replace_all(AreaCargo, ".*ADMINISTRADOR.*|.*ESTAGIARIO DE ADMINISTRACAO.*|ANALISTA DE PROJETOS|.*MELHORIA CONTINUA.*|
                            |.*ADMINISTRATIVA.*|.*STRATEGIC DECISION.*|.*BUSINESS ADMINISTRATION.*|PROJECT MANAGEMENT|
                            |.*TECNICO EM ADMINISTRACAO.*|.*ADMINISTRATIVO.*|ASSISTENTE DE TRAFEGO|.*AUXILIAR DE ESCRITORIO.*|
                            |.*GESTOR DE PROJETOS.*|.*SERVIDOR PUBLICO.*|.*ESPECIALISTA EM PROJETOS.*|.*ADMINISTRACAO DE QUALIDADE.*|
                            |.*LIDER DE PROJETOS.*", "ADMINISTRACAO"),
    # ARTES
    AreaCargo = str_replace_all(AreaCargo, ".*ARTES.*|.*ASSISTENTE DE FOTOGRAFO.*|.*FOTOGRAFIA.*|.*FOTOGRAFA.*|.*ARTE.*", "ARTES"),
    # BIOMEDICINA
    AreaCargo = str_replace_all(AreaCargo, ".*ENFERMEIRO.*|.*FARMACEUTICA.*|.*FARMACIA.*|BIOMEDICO|.*BIOMEDICINA.*", "BIOMEDICINA"),
    # COMUNICAÇÃO
    AreaCargo = str_replace_all(AreaCargo, ".*ESTAGIARIO DE ASSESSORIA DE IMPRENSA.*|.*COMUNICACAO.*|.*IDIOMAS.*|.*IDIOMA.*|
                                |.*LANGUAGES.*|.*LANGUAGE.*|.*LETRAS.*|LINGUA.+(PORTUGUESA|JAPONESA|ESPANHOLA|INGLESA)", "COMUNICACAO"),
    # DESIGN
    AreaCargo = str_replace_all(AreaCargo, ".*EDITOR DE VIDEO.*|.*PROJETISTA.*|DESIGN GRAFICO|.*DESIGN.*|.*DESENHISTA.*|
                                |DIAGRAMADOR", "DESIGN"),
    # DIREITO
    AreaCargo = str_replace_all(AreaCargo, "ANALISTA TRIB|.*ASSESSOR PARLAMENTAR.*|.*ADVOGADO.*|.*JURIDICO.*", "DIREITO"),
    # ECONOMIA
    AreaCargo = str_replace_all(AreaCargo, ".*ECONOMIA.*|.*CIENCIAS ECONOMICAS.*", "ECONOMIA"),
    # EDUCACAO
    AreaCargo = str_replace_all(AreaCargo, ".*ACESSA ESCOLA.*|^INSTRUTOR.*|.*AGENTE ESCOLAR.*|BOLSISTA CNPQ|
                                |.*LABORATORISTA DE ESCOLAS MUNICIPAIS.*|.*REPRESENTANTE DA COMISSAO DE GRADUACAO.*|
                                |.*MEMBRO DO.*|.*MEMBRO DA.*|.*INICIACAO CIENTIFICA.*|.*PROFESSOR.*|.*MONITOR.*|.*ESTUDANTE.*|
                                |.*PESQUISA.*|.*PROGRAMA INTERNACIONAL DA FACULDADE.*|.*PROJETO EUROPEU DO SEMESTRE.*", "EDUCACAO"),
    # ED. FISICA
    AreaCargo = str_replace_all(AreaCargo, ".*EDUCACAO FISICA.*", "EDUCACAO FISICA"),
    # ELETRICA
    AreaCargo = str_replace_all(AreaCargo, "^ELETRO.*|.*ELETROELETRONICA.*|.*ELETRICO.*|.*ELETRONICA.*|.*ELETRICISTA.*|
                                |.*ELETROELETRONICO.*", "ELETRICA"),
    # FINANCAS
    AreaCargo = str_replace_all(AreaCargo, ".*FRAUDE.*|.*CONTADOR.*|.*FINANCEIRO.*|.*CAIXA EXECUTIVO.*|.*FINANCAS.*|
                                |.*ANALISTA DE COBRANCA.*|.*INFORMACAO DE COBRANCA.*", "FINANCAS"),
    # GESTAO DE PESSOAS
    AreaCargo = str_replace_all(AreaCargo, ".*SUPERVISORA DE ATENDIMENTO.*|.*SUPERVISOR TEXTIL.*|
                                |.*ESPECIALISTA EM TREINAMENTO E DESENVOLVIMENTO.*|.*GESTAO DE RIQUEZA.*|
                                |ASSISTENTE DE AGENTE DE PASSEIO|.*INFLIGHT.*|.*CLIENTE.*|.*VISTORIADOR.*|
                                |.*REPRESENTANTE DE ATENDIMENTO AO CLIENTE.*|.*GESTORA DE PESSOAS.*|
                                |.*FUNCIONARIA PUBLICA ESTADUAL.*|.*FISCAL DE LOJA.*|.*GESTAO DE PESSOAS.*|
                                |ATENDENTE|AUXILIAR GERAL|BALCONISTA", "GESTAO DE PESSOAS"),
    # GESTAO DE EMPRESAS
    AreaCargo = str_replace_all(AreaCargo, ".*ANALISTA DE SECRETARIA.*|.*INTELIGENCIA DE MERCADO.*|^ANALISTA$|.*GESTAO DE PROJETOS.*|
                                |^CEO$|.*CUMPRIMENTO.*|^PROPRIETARIO$|.*EMPRESAS.*|^GERENTE$|.*GESTAO DE EMPRESAS.*|
                                |.*ADMINISTRACAO DE EMPRESAS.*|.*BUSINESS MANAGEMENT.*|.*ANALISTA DE PROGRAMA DE MELHORIAS.*|.*FUNDADOR.*|
                                |.*SUPERVISOR DE PLANEJAMENTO.*|.*SOCIO.*|.*COLABORADOR.*|.*PRODUTOS E FORNECEDORES.*|.*NEGOCIOS.*|
                                |ASSISTENTE SUPRIMENTOS|.*GESTAO EMPRESARIAL.*|ASSISTENTE DE PROJETOS|^ASSISTENTE DE SUPORTE$|
                                |ASSISTENTE DE IMPLEMENTACAO|.*ESTOQUISTA.*|.*EXECUTIVO DE PROCESSO.*|ASSISTENTE DE PLANEJAMENTO|
                                |CONSULTORA TECNICA|.*ALMOXARIFE.*|^CONFERENTE$|^CONSULTOR$|.*CONTROL.*|CONTROLADOR DE ARMAZEM|
                                |.*COORDENADOR.*|.*GESTAO DE EMPRESAS.*|.*DIRETOR.*|.*EXPEDIDOR.*", "GESTAO DE EMPRESAS"),
    # INFORMACAO
    AreaCargo = str_replace_all(AreaCargo, "ANALISTA.+REDE.*|ANALISTA DE PERFORMANCE|.*INTEGRACAO SOA.*|.*INFORMATICA.*|
                                |.*MOBILE DEVELOPER.*|.*ANALISTA DE SISTEMAS.*|.*ANALISTA DE SUPORTE.*|.*ANALISTA DE TI.*|
                                |.*ANALISTA DE TESTE.*|.*ANALISTA DE TREINAMENTO.*|.*DESENVOLVEDOR.*|.*ANALISTA JUNIOR.*|
                                |.*ENGENHEIRO DE SOFTWARE.*|.*ESTAGIARIO DE TI.*|ESTAGIARIO.+PROGRAMA.*|.*INSTRUTOR DE CODIGO.*|
                                |.*TECNOLOGIA.*|.*COMPUTACAO.*|PROGRAMADOR.+(JAVA|WEB|JUNIOR)|.*LIDER TECNICO.*|.*MANUTENCAO E SUPORTE.*|
                                |.*COMPUTACAO.*|.*ANALISTA FUNCIONAL.*|.*TECNICO (EM|DE) INFORMATICA.*|.*(GESTOR|GERENTE) DE TI.*|
                                |.*ESPECIALISTA EM TI.*|^PROGRAMADOR.*$|.*ASSISTENTE DE TI.*|.*FREE LANCER.*|.*ENGENHEIRO DE SUPORTE.*|
                                |.*FULL STACK DEVELOPER.*|.*JUNIOR SUPPORT ANALYST.*|PROGRAMADOR C#|.*TESTADOR.*|.*WEB DEVELOPER.*|
                                |^ANALISTA $|APRENDIZ.+(PROGRAMACAO|SOFTWARE|HARDWARE)|AUDITOR DE SISTEMA|.*OPERADOR CPD.*|
                                |AUXILIAR DE DESENVOLVIMENTO|AUXILIAR DE TI|.*DESENVOLVIMENTO WEB.*|.*DATA CENTER.*|.*SUPORTE DE TI.*|
                                |.*ASSISTENTE DE SUPORTE DE SISTEMAS.*|.*TI$|^TECNICO ANALISTA$|ANALISTA PROGRAMADOR.*|.*PROCESSOS.*|
                                |.*ASSISTENCIA TECNICA.*|.*APPLICATION DEVELOPER.*|.*ARQUITETO DE SOFTWARE.*|
                                |.*SECURITY ANALYST.*", "INFORMACAO"),
    # INFRAESTRUTURA
    AreaCargo = str_replace_all(AreaCargo, "^ENGENHEIRO$|.*ESTAGIARIO DE ENGENHARIA.*|^TECNICO EM ENGENHARIA$|ASSISTENTE DE ENGENHARIA|
                                |.*APRENDIZ DE ENGENHARIA.*|AUXILIAR DE ENGENHARIA|.*CONSTRUCAO CIVIL.*|.*ARQUITETURA.*|
                                |.*TECNICO PROJETISTA.*|.*EDIFICACOES.*|.*ENGENHARIA CIVIL.*|
                                |.*ARQUITETA.*|.*ENGENHEIRO CIVIL.*", "INFRAESTRUTURA"),
    # LOGISTICA
    AreaCargo = str_replace_all(AreaCargo, ".*LOGISTICA.*|^SUPERVISOR$|.*MONTADOR DE INSTRUMENTOS DE PRECISAO.*|
                                |.*ESTAGIARIO TECNICO.*|.*AUXILIAR TECNICO.*|.*SUPERVISOR TECNICO.*|.*ESTAGIARIO DE APOIO TECNICO.*|
                                |.*AUXILIAR DE ARMAZEM.*|^AUXILIAR$|^APRENDIZ$|ASSISTENTE DE SUPRIMENTOS|SUPERVISOR DE TRAFEGO|
                                |^ASSISTENTE TECNICO$", "LOGISTICA"),
    # MARKETING
    AreaCargo = str_replace_all(AreaCargo, ".*MARKETING.*|ASSISTENTE DE COMPRAS|AUXILIAR DE POSVENDA|.*COMERCIO EXTERIOR.*|.*VENDEDOR.*|.*VENDAS.*|.*DESENVOLVIMENTO DE PRODUTOS.*|.*MARKETING.*|.*ECOMMERCE.*|.*EXPORTACAO.*|CONSULTOR DE VENDAS|
                                |.*SINISTRO AUTO.*|.*COMPRADOR.*|.*REGULADOR DE SINISTROS.*|CORRETOR DE SEGUROS.*|.*ASSISTENTE COMERCIAL.*|.*TECNICO DE SEGUROS.*|ESPECIALISTA EM SEGUROS", "MARKETING"),
    # MIDIA
    AreaCargo = str_replace_all(AreaCargo, ".*CINEMA.*", "MIDIA"),
    # MILITAR
    AreaCargo = str_replace_all(AreaCargo, ".*AERONAUTICA.*|.*AVIATION.*|.*SOLDADO.*", "MILITAR"),
    # PROCESSOS INDUSTRIAIS
    AreaCargo = str_replace_all(AreaCargo, ".*AJUSTADOR DE MAQUINAS.*|.*METALURGICO.*|.*MECANTRONICO.*|.*GARANTIA.*|
                                |.*GARANTIA DE QUALIDADE.*|.*QUALITY.+ANALYST.*|.*ANALISTA DA QUALIDADE.*|.*INDUSTRIAL.*|.*PRODUCAO.*|
                                |.*MECANICA.*|.*MECHANICAL ENGINEERING.*|.*MECANICO.*|.*MECATRONICA.*|.*SUPERVISOR DE OPERACOES.*|
                                |.*INDUSTRIAL.*|.*USINAGEM.*|.*MECANICO.*|.*ENGENHARIA DE PROCESSO.*|.*PRODUCAO.*|ANALISTA DE PROCESSOS|
                                |ASSISTENTE DE PCP|AUXILIAR DE PCP|CAPITA DA EQUIPE TAPERA BABY|.*PROCESSOS DE FABRICACAO.*|
                                |AUDITOR DE QUALIDADE|.*ENGENHARIA DE MANUFATURA.*|.*ENGENHARIA DE PRODUTO.*|.*ENGENHARIA DE QUALIDADE.*|
                                |.*ESPECIALISTA OPERACIONAL.*|AUXILIAR DE LABORATORIO|TECNICO LABORATORIO|.*SUPERVISOR DE SERVICO.*|
                                |AUXILIAR DE SERVICOS|.*QUALIDADE E PROCESSOS.*|FABRICANTE DE FERRAMENTAS|FACILITADOR DE TURNO|
                                |.*FRESADOR.*|.*FRESA.*|.*TORNEIRO.*|.*MANUTENCAO.*|INSPETOR DE QUALIDADE|METROLOGISTA|
                                |TECNICO DA QUALIDADE|TECNICO DE PROCESSOS|TECNICO DE TESTES|.*PROCESSO PLASTICO.*|
                                |.*ADMINISTRACAO DE QUALIDADE.*|AJUDANTE DE PRODUCAO|ENGENHEIRO COMERCIAL DE APLICACAO|
                                |.*ANALISTA DE METODOS E PROCESSOS.*|.*FERRAMENTEIRO.*|.*ANALISTA DE PCP.*|.*CNC.*|OPERADOR.+[MAQUINA].*|
                                |INJETORA|MULTINACIONAL|TECNICO|CPD]|.*TREFILA.*|^OPERADOR$|ASSISTENTE DE MELHORIA CONTINUA|
                                |.*ASSISTENTE TECNICO DE QUALIDADE.*|.*OPERADOR SET.*|ASSISTENTE DE PROCESSOS|AUXILIAR DE ALMOXARIFADO|
                                |.*AUXILIAR CONFERENTE DE CARGAS.*|ENGENHEIRO DE QUALIDADE|.*ENGENHARIA DE PROCESSOS.*|
                                |.*ENGENHARIA DA QUALIDADE.*|.*SUPERVISOR TEXTIL.*|LIDER DE ACABAMENTO|.*ANALISTA OPERACIONAL.*|
                                |.*PRENCISTA.*|.*PROCESSADOR DE MOEDA.*|.*OPERATIVA GERAL.*|.*ORCAMENTISTA E PROJETISTA.*|
                                |.*ASSISTENTE DE ABASTECIMENTO.*|.*AUXILIAR DE INSPECAO.*|
                                |MONTADOR DE INSTRUMENTOS DE MEDICAO", "PROCESSOS INDUSTRIAIS"),
    # RECURSOS NATURAIS
    AreaCargo = str_replace_all(AreaCargo, ".*METROLOGIA.*|.*CENTRAL DE RECICLAGEM.*|.*ENGENHARIA FLORESTAL.*|.*AMBIENTE.*|
                                |.*AGRONOMICA.*|.*AGRICOLA.*|AUXILIAR FLORESTAL|AGRONOMIA", "RECURSOS NATURAIS"),

    # QUIMICA
    AreaCargo = str_replace_all(AreaCargo, ".*QUIMICO.*", "QUIMICA"),

    # Outros
    AreaCargo = str_replace_all(AreaCargo, "BRASIL KIRIN|DESEMPREGADO|MASSOTERAPIA|^EMPREGADO$|PARTICIPANTE|.*MOTORISTA.*|
                                |^ESTAGIARIO$|.*JOVEM APRENDIZ.*|OFFICE BOY|^SUPORTE$|^INTERN$|
                                |^TECNICO$|ESTAGIARIO.+SUPORTE TECNICO", "OUTROS")
  ) %>%
  mutate(
    # ==============================================================================
    # REMOVE ESPAÇOS EM BRANCO NO INÍCIO E FINAL
    # ==============================================================================
    Nome = str_trim(Nome, side = c("both")),
    Nome = str_squish(Nome),
    Empresa = str_trim(Empresa, side = c("both")),
    Empresa = str_squish(Empresa),
    Cargo = str_trim(Cargo, side = c("both")),
    Cargo = str_squish(Cargo),
    Cidade = str_trim(Cidade, side = c("both")),
    Cidade = str_squish(Cidade),
    AreaCargo = str_trim(AreaCargo, side = c("both")),
    AreaCargo = str_squish(AreaCargo)
  )

# ==============================================================================
# PADRONIZAÇÃO ESTUDOS
# ==============================================================================
estudos <- read.csv(file = "EstudosEvadidos.csv", header = TRUE) %>%
  # ==============================================================================
  # REMOÇÃO DE ACENTOS
  # ==============================================================================
  mutate(
    Nome = iconv(Nome, to = "ASCII//TRANSLIT"),
    Escola = iconv(Escola, to = "ASCII//TRANSLIT"),
    Curso = iconv(Curso, to = "ASCII//TRANSLIT"),
    Area = iconv(Area, to = "ASCII//TRANSLIT"),
    # Remove espaços em branco do início e do final
    Nome = str_trim(Nome, side = c("both")),
    Nome = str_squish(Nome)
  ) %>%
  # ==============================================================================
  # LETRAS MAIÚSCULAS
  # ==============================================================================
  mutate_all(str_to_upper) %>%
  mutate(
    # ==============================================================================
    # REMOÇÃO DE TEXTOS INDESEJADOS
    # ==============================================================================
    # ESCOLA
    Escola = str_replace_all(Escola, "\"|E\\. E\\. |E E |EE\\.", ""),
    Escola = str_replace_all(Escola, " - |E\\.E\\.?|EEPG|SALTO|PROVIDING|PROF|PROFESSOR|PROFESSORA", " "),

    # CURSO
    Curso = str_replace_all(Curso, "-", " "),
    Curso = str_replace_all(Curso, "2003|", ""),
    # NOME
    Nome = str_replace_all(Nome, "\"", " "),
    Nome = str_replace_all(Nome, "PROFESSORA|INFORMATICA", ""),

    # ==============================================================================
    # CORREÇÃO DE PALAVRAS INCORRETAS
    # ==============================================================================
    # CURSO
    Curso = str_replace_all(Curso, "IDUSTRIAL", "INDUSTRIAL"),
    Curso = str_replace_all(Curso, "ENSIONO", "ENSINO"),
    Curso = str_replace_all(Curso, "ADMINSTRACAO", "ADMINISTRACAO"),
    Curso = str_replace_all(Curso, ".*TECHNICAL COURSE|.*TECHNICAL COURSES", "TECNICO EM"),
    Curso = str_replace_all(Curso, ".*TEC\\.", "TECNICO"),
    Curso = str_replace_all(Curso, "MECHATRONICS", "MECATRONICA"),
    Curso = str_replace_all(Curso, "HIGH SCHOOL", "ENSINO MEDIO"),
    # ESCOLA
    Escola = str_replace_all(Escola, "UNIVERSITY OF CALIFORNIA.*", "UNIVERSIDADE DA CALIFORNIA"),
    Escola = str_replace_all(Escola, "ISTITUTO ", "INSTITUTO "),
    # AREA
    Area = str_replace_all(Area, "COMPUTER SCIENCE", "CIENCIAS DA COMPUTACAO"),
    Area = str_replace_all(Area, ".*EDUCATIONAL GAMIFICATION.*", "GAMIFICACAO EDUCACIONAL"),
    Area = str_replace_all(Area, "INFORMATION TECHNOLOGY", "TECNOLOGIA DA INFORMACAO"),
    Area = str_replace_all(Area, "TECHNOLOGY", "TECNOLOGIA"),
    Area = str_replace_all(Area, "INFORMATION", "INFORMACAO"),
    Area = str_replace_all(Area, ".*HEALTH.*", "SAUDE"),
    Area = str_replace_all(Area, "PESOAS", "PESSOAS"),

    # ==============================================================================
    # ESCOLAS
    # ==============================================================================
    # A
    Escola = str_replace_all(Escola, ".*ANHANGUERA.*", "ANHANGUERA"),
    Escola = str_replace_all(Escola, ".*ANCHIETA.*|.*CENTRO UNIVERSITARIO PADRE ANCHIETA.*", "ANCHIETA"),
    # C
    Escola = str_replace_all(Escola, ".*COLEGIO ANGLO.*|.*ANGLO.*", "COLEGIO ANGLO"),
    Escola = str_replace_all(Escola, ".*COLEGIO TECNICO INDUSTRIAL.*", "CTI"),
    Escola = str_replace_all(Escola, ".*CEUNSP.*|.*UNIVERSIDADE CRUZEIRO DO SUL.*|.*CENTRO UNIVERSITARIO NOSSA SENHORA DO PATROCINIO.*", "CEUNSP"),
    Escola = str_replace_all(Escola, ".*COTUCA.*|.*COLEGIO TECNICO CAMPINAS.*", "COTUCA"),
    Escola = str_replace_all(Escola, ".*COLEGIO OBJETIVO.*|.*OBJETIVO.*", "COLEGIO OBJETIVO"),
    # E
    Escola = str_replace_all(Escola, ".*ENIAC.*", "ENIAC"),
    Escola = str_replace_all(Escola, ".*ESAMC.*", "ESAMC"),
    Escola = str_replace_all(Escola, ".*ESTACIO DE SA.*", "ESTACIO"),
    Escola = str_replace_all(Escola, ".*EMBASSY SCHOOL HASTINGS UK.*", "EMBASSY SCHOOL HASTINGS"),
    Escola = str_replace_all(Escola, ".*ESALQ.*|.*ESCOLA SUPERIOR DE AGRICULTURA LUIZ DE QUEIROZ.*", "ESALQ"),
    Escola = str_replace_all(Escola, ".*ESCOLA TECNICA ESTADUAL.*|.*DR. FRANCISCO NOGUEIRA DE LIMA.*|E\\.T\\.E\\.(C\\.)?.*|
                             |.*CENTRO PAULA SOUZA.*|.*RUBENS DE FARIA E SOUZA.*|.*FERNANDO PRESTES.*|
                             |.*GUARACY SILVEIRA.*|.*ESSOR JOSE SANT'ANA DE CASTRO.*|.*SALES GOMES CENTRO PAULA DE SOUZA.*|
                             |.*ETEC DE HORTOLANDIA.*", "ETEC"),
    # F
    Escola = str_replace_all(Escola, ".*FAAT.*|.*FACULDADES ATIBAIA.*", "FAAT"),
    Escola = str_replace_all(Escola, ".*FATEC.*|.*FACULDADE DE TECNOLOGIA DE SAO PAULO.*|.*FACULDADE DE TECNOLOGIA DE.*", "FATEC"),
    Escola = str_replace_all(Escola, ".*FUNDACAO INDAIATUBANA.*|.*FIEC.*", "FIEC"),
    Escola = str_replace_all(Escola, ".*FUNDACAO SANTO ANDRE.*", "FSA"),
    Escola = str_replace_all(Escola, ".*FACENS.*|.*FACULDADE DE ENGENHARIA DE SOROCABA.*", "FACENS"),
    Escola = str_replace_all(Escola, ".*FEMA.*", "FEMA"),
    Escola = str_replace_all(Escola, ".*FADITU.*|.*FACULDADE DE DIREITO DE ITU.*", "FADITU"),
    Escola = str_replace_all(Escola, ".*FISK.*", "FISK"),
    # G
    Escola = str_replace_all(Escola, ".*AEDHA.*|.*HOMEM DE AMANHA.*", "GUARDINHA"),
    # I
    Escola = str_replace_all(Escola, ".*INSTITUTO BORGES DE ARTES E OFICIOS.*|.*INSTITUTO BORGES.*", "IBAO"),
    Escola = str_replace_all(Escola, ".*CENTRO UNIVERSITARIO ITALO BRASILEIRO.*", "ITALO"),
    Escola = str_replace_all(Escola, ".*IPCA.*|.*INSTITUTO POLITECNICO DO CAVADO E DO AVE.*", "IPCA"),
    Escola = str_replace_all(Escola, ".*ICMC.*|.*INSTITUTO.+CIENCIAS MATEMATICAS.*", "ICMC"),
    Escola = str_replace_all(Escola, ".*IQA.*", "IQA"),
    Escola = str_replace_all(Escola, ".*IMAPES.*|.*INSTITUTO MANCHESTER DE ENSINO SUPERIOR.*", "IMAPES"),
    Escola = str_replace_all(Escola, ".*IFSP.*|^INSTITUTO FEDERAL.+SAO PAULO.*|^INSTITUTO FEDERAL.+SALTO.*|.*CEFET.*|
                             |.*INSTITUTO FEDERAL DE SOROCABA.*", "IFSP"),
    # J
    Escola = str_replace_all(Escola, ".*JOAO RAMALHO.*", "JOAO RAMALHO"),
    # L
    Escola = str_replace_all(Escola, ".*LEONOR FERNANDES DA SILVA.*|.*LEONOR FERNANDES.*", "LEONOR FERNANDES DA SILVA"),
    Escola = str_replace_all(Escola, ".*COLEGIO LICEU.*|.*LICEU*", "LICEU"),
    # M
    Escola = str_replace_all(Escola, ".*METROCAMP.*|.*METROPOLITANA CAMPINAS.*", "METROCAMP"),
    Escola = str_replace_all(Escola, ".*MAX PLANCK.*|.*MAXPLANCK.*", "MAX PLANCK"),
    Escola = str_replace_all(Escola, ".*MACKENZIE.*", "MACKENZIE"),
    Escola = str_replace_all(Escola, ".*MICROLINS.*", "MICROLINS"),
    # P
    Escola = str_replace_all(Escola, ".*PAULA SANTOS.*", "PAULA SANTOS"),
    # Q
    Escola = str_replace_all(Escola, ".*QUEENSLAND UNIVERSITY OF TECHNOLOGY.*", "QUT"),
    # S
    Escola = str_replace_all(Escola, ".*SENAI.*|.*SERVICO NACIONAL DE APRENDIZAGEM INDUSTRIAL.*", "SENAI"),
    Escola = str_replace_all(Escola, ".*SCHOOL OF ART, GAME AND ANIMATION.*", "SAGA"),
    Escola = str_replace_all(Escola, ".*CENTRO UNIVERSITARIO SANT'ANNA.*", "SANTANNA"),
    Escola = str_replace_all(Escola, ".*SENAC.*|.*SERVICO NACIONAL DE APRENDIZAGEM COMERCIAL.*", "SENAC"),
    # T
    Escola = str_replace_all(Escola, ".*TANCREDO DO AMARAL.*", "TANCREDO DO AMARAL"),
    Escola = str_replace_all(Escola, ".*TREINACOM.*|.*TREINACON.*", "TREINACON"),
    # U
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE CANDIDO MENDES.*", "UCAM"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE FEDERAL DE SAO PAULO.*", "UNIFESP"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE FEDERAL RURAL DO RIO DE JANEIRO.*", "UFRRJ"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE GAMA FILHO.*", "UGF"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE METODISTA.*", "UNIVERSIDADE METODISTA"),
    Escola = str_replace_all(Escola, ".*JULIO DE MESQUITA.*", "UNESP"),
    Escola = str_replace_all(Escola, ".*UNIARARAS.*", "UNIARARAS"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE METODISTA DE PIRACICABA.*", "UNIMEP"),
    Escola = str_replace_all(Escola, ".*UNINOVE UNIVERSIDADE NOVE DE JULHO.*", "UNINOVE"),
    Escola = str_replace_all(Escola, ".*UNINTER.*", "UNINTER"),
    Escola = str_replace_all(Escola, ".*UNICAMP.*|.*UNIVERSIDADE ESTADUAL DE CAMPINAS.*", "UNICAMP"),
    Escola = str_replace_all(Escola, ".*UNIESP.*", "UNIESP"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE NORTE DO PARANA.*", "UNOPAR"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE DE SAO PAULO.*", "USP"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE ESTADUAL DE LONDRINA.*", "UEL"),
    Escola = str_replace_all(Escola, ".*UNIP.*|.*UNIVERSIDADE PAULISTA.*", "UNIP"),
    Escola = str_replace_all(Escola, ".*UNIRP.*|.*CENTRO UNIVERSITARIO DE RIO PRETO.*", "UNIRP"),
    Escola = str_replace_all(Escola, ".*UNISO.*|.*UNIVERSIDADE DE SOROCABA.*", "UNISO"),
    Escola = str_replace_all(Escola, ".*UNITAU.*|.*UNIVERSIDADE DE TAUBATE.*", "UNITAU"),
    Escola = str_replace_all(Escola, ".*STANFORD UNIVERSITY.*", "UNIVERSIDADE DE STANFORD"),
    Escola = str_replace_all(Escola, ".*UFSCAR.*|.*UNIVERSIDADE FEDERAL DE SAO CARLOS.*", "UFSCAR"),
    Escola = str_replace_all(Escola, ".*UFPR.*|.*UNIVERSIDADE FEDERAL DO PARANA*", "UFPR"),
    Escola = str_replace_all(Escola, ".*UNIFEI.*|.*UNIVERSIDADE FEDERAL DE ITAJUBA.*", "UNIFEI"),
    Escola = str_replace_all(Escola, ".*UNICENTRO.*|.*UNIVERSIDADE ESTADUAL DO CENTRO-OESTE.*", "UNICENTRO"),
    Escola = str_replace_all(Escola, ".*UBS.*", "UBS"),
    Escola = str_replace_all(Escola, ".*UNIBAN.*", "UNIBAN"),
    Escola = str_replace_all(Escola, ".*UNIBTA.*", "UNIBTA"),
    Escola = str_replace_all(Escola, ".*UNISC.*", "UNISC"),
    Escola = str_replace_all(Escola, ".*UNISINOS.*", "UNISINOS"),
    Escola = str_replace_all(Escola, ".*UNIVERSIDADE FEDERAL DO ABC.*", "UFABC"),
    # W
    Escola = str_replace_all(Escola, ".*DEVRY.*", "WYDEN"),
    # Y
    Escola = str_replace_all(Escola, ".*YAZIGI.*", "YAZIGI"),

    # ==============================================================================
    # AREAS
    # ==============================================================================
    # C
    Area = str_replace_all(Area, ".*ESPECIALIZACAO EM ESTRUTURAS DE CONSTRUCAO CIVIL.*", "CONSTRUCAO CIVIL"),
    # E
    Area = str_replace_all(Area, ".*ENGENHARIA DE MATERIAIS E PROCESSOS.*", "ENGENHARIA DE MATERIAIS"),
    # I
    Area = str_replace_all(Area, ".*BUSINESS INTELLIGENCE.*", "INTELIGENCIA DE NEGOCIOS"),
    # M
    Area = str_replace_all(Area, ".*MARKETING.*", "MARKETING"),
    # ==============================================================================
    # CURSOS UTILIZANDO ÁREAS
    # ==============================================================================
    Curso = case_when(str_detect(Curso, "^MESTRADO$|.*MESTRADO IN.*|^MESTRANDO$|.*MASTER OF SCIENCE.*") ~ paste("MESTRADO EM", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^POS GRADUACAO$|^POST GRADUATE.*$") ~ paste("POS GRADUACAO EM", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^DOUTORADO$") ~ paste("DOUTORADO EM", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^BACHAREL$|^BACHARELADO$") ~ paste(Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*BACHARELADO EM ENGENHARIA.+MECANICA.*|MECHANICAL ENGINEER") ~ paste("ENGENHARIA MECANICA"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^GRADUACAO$|^GRADUADO$|^GRADUATION$") & !is.na(Area) ~ paste("GRADUACAO EM ", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^ESPECIALIZACAO$") ~ paste("ESPECIALIZACAO EM ", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^ESTUDANTE$") ~ paste(Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^TECNICO$|.*TECHNICAL DEGREE.*|.*CURSO TECNICO.*") & str_detect(Area, ".*TECNICO.*") ~ paste(Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^TECNICO$|.*TECHNICAL DEGREE.*|.*CURSO TECNICO.*") ~ paste("TECNICO EM ", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*TECNICO EM.*MECATRONICA") ~ paste("TECNICO EM MECATRONICA"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^TECNOLOGO$") ~ paste("TECNOLOGO EM ", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^TECNOLOGIA$") ~ paste("TECNOLOGO EM ", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^GRAU TECNOLOGICO$") ~ paste("TECNOLOGO EM ", Area), TRUE ~ Curso),

    # ==============================================================================
    # CURSOS
    # ==============================================================================
    Curso = case_when(is.na(Curso) & !is.na(Area) ~ paste(Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^\\.$") & !is.na(Area) ~ paste(Area), TRUE ~ Curso),
    # A
    Curso = str_replace_all(Curso, ".*BACHAREL EM AGRONOMIA.*", "AGRONOMIA"),
    Curso = str_replace_all(Curso, ".*BACHARELADO EM ADMINISTRACAO.*|.*ADMINISTRACAO DE EMPRESAS.*|.*ADMINISTRATIVO.*|
                            |ADMINISTRATION TECNICAN", "ADMINISTRACAO"),
    Curso = str_replace_all(Curso, ".*TECNOLOGIA EM ANALISE E DESENVOLVIMENTO DE SISTEMAS.*|.*ANALISE D?E? DESENVOLVIMENTO DE SISTEMA.*|
                            |.*TECNOLOGO EM ADS.*|.*SYSTEM ANALYSIS.*|.*TECHNOLOGIST ON SYSTEMS ANALYSIS.*|.*TECNOLOGIA ANALISE.*|
                            |.*ANALISTA E DESENVOLVEDOR DE SISTEMAS.*|.*ANALYSIS OF SYSTEM.*|.*ANALISE DESENVOLVIMENTO DE SISTEMAS.*|
                            |.*ANALISE E DESENVOLVIMENTOS DE SISTEMA.*|.*ANALISE E DESENVOLVIMENTO DE SISTEMAS.*", "ADS"),
    # B
    Curso = str_replace_all(Curso, ".*BACHELOR'S DEGREE.*|.*BACHELOR OF ENGINEERING.*|.*ENGINEER'S DEGREE.*|^BACHARELADO EM ENGENHARIA$", "BEL. EM ENGENHARIA"),
    # C
    Curso = str_replace_all(Curso, ".*BACHELOR OF SCIENCE AND TECHNOLOGY.*", "CIENCIA E TECNOLOGIA"),
    Curso = str_replace_all(Curso, ".*SCIENCE WITHOUT BORDERS.*", "CIENCIA SEM FRONTEIRAS"),
    Curso = str_replace_all(Curso, ".*CIENCIA.*DA COMPUTACAO.*", "CIENCIA DA COMPUTACAO"),
    Curso = str_replace_all(Curso, ".*CONTROLADOR DIMENCIONAL.*", "CONTROLADOR DIMENSIONAL"),
    Curso = str_replace_all(Curso, ".*CIENCIAS BIOMEDICAS.*", "CIENCIAS BIOMEDICAS"),
    Curso = str_replace_all(Curso, ".*DIMENSIONAL CONTROL.*", "CONTROLE DIMENSIONAL"),
    # D
    Curso = str_replace_all(Curso, ".*ANDROID DEVELOPER.*|.*DESENVOLVEDOR ANDROID.*", "DESENVOLVEDOR ANDROID"),
    Curso = str_replace_all(Curso, ".*DIREITO.*", "DIREITO"),
    Curso = str_replace_all(Curso, ".*JAVA DEVELOPER.*|.*DESENVOLVEDOR JAVA.*", "DESENVOLVEDOR JAVA"),
    Curso = str_replace_all(Curso, ".*GRAPHIC DESIGN.*", "DESIGN GRAFICO"),
    # E
    Curso = str_replace_all(Curso, ".*ENGENHEIRO AGRICOLA.*", "ENGENHARIA AGRICOLA"),
    Curso = str_replace_all(Curso, ".*ENSINO MEDIO.*|.*ENSINO.+MEDIO.*|.*MEDIO.*", "ENSINO MEDIO"),
    Curso = str_replace_all(Curso, "^ESPECIALIST$|^SPECIALIST$", "ESPECIALIZACAO"),
    Curso = str_replace_all(Curso, ".*ENGENHARIA DE PRODUCAO.*", "ENGENHARIA DE PRODUCAO"),
    Curso = str_replace_all(Curso, ".*ROBOTICS ENGINEERING.*", "ENGENHARIA ROBOTICA"),
    Curso = str_replace_all(Curso, ".*BACHARELADO EM ENGENHARIA ELETRICA.*", "ENGENHARIA ELETRICA"),
    # F
    Curso = str_replace_all(Curso, ".*FERRAMENTARIA E MECANICA.*", "FERRAMENTARIA E MECANICA"),
    # G
    Curso = str_replace_all(Curso, ".*GESTAO EMPRESARIAL.*", "GESTAO EMPRESARIAL"),
    Curso = str_replace_all(Curso, "GESTAO PUBLICA.*", "GESTAO PUBLICA"),
    Curso = str_replace_all(Curso, ".*GESTAO.+TECNOLOGIA DA INFORMACAO*", "GESTAO EM TI"),
    Curso = str_replace_all(Curso, ".*GESTAO (DE|E.+) EMPRESAS.*|.*GESTAO EMPRESARIAL.*", "GESTAO DE EMPRESAS"),
    Curso = str_replace_all(Curso, "^SUPERIOR$", "GRADUACAO"),
    Curso = str_replace_all(Curso, ".*GESTAO DA?E? PRODUCAO INDUSTRIAL.*|.*GRADUACAO EM GPI.*|
                            |.*INDUSTRIAL PRODUCTION MANAGEMENT.*", "GPI"),
    Curso = str_replace_all(Curso, ".*GESTAO FISCAL.*", "GESTAO FISCAL"),
    # I
    Curso = str_replace_all(Curso, ".*INGLES.*|.*ENGLISH.*", "INGLES"),
    # J
    Curso = str_replace_all(Curso, ".*JORNALISMO.*", "JORNALISMO"),
    # M
    Curso = str_replace_all(Curso, ".*MASTER'S DEGREE*|MESTRADO*", "MESTRADO"),
    Curso = str_replace_all(Curso, ".*MASTER OF BUSINESS.*|.*SPECIALIZATION COURSE.*ADMINISTRATION.*|MBA.*", "MBA"),
    # N
    Curso = str_replace_all(Curso, ".*COMPRAS, ESTRATEGIAS DE NEGOCIACAO.*", "NEGOCIACAO"),
    # P
    Curso = str_replace_all(Curso, ".*PROJETOS MECANICOS.*", "PROJETOS MECANICOS"),
    Curso = str_replace_all(Curso, ".*POKA YOKE.*", "POKA YOKE"),
    Curso = str_replace_all(Curso, ".*PROGRAMADOR.*CNC.*", "PROGRAMADOR CNC"),
    # Q
    Curso = str_replace_all(Curso, ".*QUIMICA BACHAREL.*", "QUIMICA"),
    # S
    Curso = str_replace_all(Curso, ".*SISTEMAS INTEGRADOS DE GESTAO DA QUALIDADE.*, .*AMBIENTE E SEGURANCA.*", "SIG-QAS"),
    Curso = str_replace_all(Curso, ".*INFORMATION SYSTEMS.*", "SISTEMA DE INFORMACAO"),
    # T
    Curso = str_replace_all(Curso, ".*BACHELOR OF TECHNOLOGY.*|.*BTECH.*", "TECNOLOGIA"),
    Curso = str_replace_all(Curso, "^TECNICO.+(.*AUTOMACAO INDUSTRIAL.*)|.*TECNOLOGIA EM PROCESSOS.*|
                            |^AUTOMACAO INDUSTRIAL$", "TECNICO EM AUTOMACAO INDUSTRIAL"),
    Curso = str_replace_all(Curso, "^TECNICO.+(.*ANALISE E DESENVOLVIMENTO.*|.*PROGRAMACAO DE SISTEMAS.*|.*SYSTEM PROGRAMMING AND NETWORKS|
                            |.*TECHNICAL ON INFORMATION TECHNOLOGY.*|.*INFORMATICA.*)", "TECNICO EM INFORMATICA"),
    Curso = str_replace_all(Curso, ".*TECHNICAL ON INFORMATION TECHNOLOGY.*|.*IT TECHNICIAN.*", "TECNICO EM TI"),
    Curso = str_replace_all(Curso, ".*TECHNICAL DRAWING INTERPRETATION.*|.*TECNICO.*DESENHO.*", "TECNICO EM DESENHO"),
    Curso = str_replace_all(Curso, ".*TECNICO.+ELETROELETRONICA.*", "TECNICO EM ELETROELETRONICA"),
    Curso = str_replace_all(Curso, ".*TECNOLOGO.*COMERCIO EXTERIOR.*", "TECNOLOGO EM COMERCIO EXTERIOR"),
    # U
    Curso = str_replace_all(Curso, ".*MECHANICAL MACHINING.*|.*USINAGEM MECANICA.*", "USINAGEM MECANICA"),

    # ==============================================================================
    # AREAS
    # ==============================================================================
    # ADMINISTRACAO
    Area = str_replace_all(Area, ".*STRATEGIC DECISION.*|.*BUSINESS ADMINISTRATION.*|PROJECT MANAGEMENT|
                           |GESTAO EM LOGISTICA|.*OPERADOR DE LOGISTICA.*|
                           |.*TECNICO EM ADMINISTRACAO.*|.*ASSITENTE ADMINISTRATIVO.*|.*ADMINISTRATIVA.*", "ADMINISTRACAO"),
    # ARTES
    Area = str_replace_all(Area, ".*ARTES.*|.*FOTOGRAFIA.*|.*FOTOGRAFA.*|.*ARTE.*", "ARTES"),
    # BIOMEDICINA
    Area = str_replace_all(Area, ".*FARMACIA.*|.*BIOMEDICA.*|.*MICROBIOLOGIA.*", "BIOMEDICINA"),
    # COMUNICACAO
    Area = str_replace_all(Area, ".*COMUNICACAO.*|.*IDIOMAS.*|.*IDIOMA.*|.*LANGUAGES.*|.*LANGUAGE.*|.*LETRAS.*|LINGUA.+(PORTUGUESA|JAPONESA|ESPANHOLA|INGLESA)", "COMUNICACAO"),
    # DESIGN
    Area = str_replace_all(Area, "DESIGN GRAFICO|.*DESIGN.*|.*DESENHISTA.*|DIAGRAMADOR|.*DESENHO TECNICO.*", "DESIGN"),
    # DIREITO
    Area = str_replace_all(Area, "ANALISTA TRIB|.*ASSESSOR PARLAMENTAR.*|.*ADVOGADO.*|.*JURIDICO.*", "DIREITO"),
    # ECONOMIA
    Area = str_replace_all(Area, ".*ECONOMIA.*|.*CIENCIAS ECONOMICAS.*", "ECONOMIA"),
    # EDUCACAO
    Area = str_replace_all(Area, ".*ENSINO MEDIO.*|.*PEDAGOGIA.*", "EDUCACAO"),
    # ED. FISICAO
    Area = str_replace_all(Area, ".*EDUCACAO FISICA.*", "EDUCACAO FISICA"),
    # ELETRICA
    Area = str_replace_all(Area, "^ELETRO.*|.*ELETROELETRONICA.*|.*ELETRONICA.*|
                           |.*ENGENHARIA ELETRICA.*|.*ELETRICA.*|.*ELETROTECNICA.*", "ELETRICA"),
    # FINANCAS
    Area = str_replace_all(Area, ".*CONTABILIDADE.*|.*FINANCAS.*", "FINANCAS"),
    # GESTAO DE PESSOAS
    Area = str_replace_all(Area, ".*GESTAO DE PESSOAS.*|.*6 SIGMA.*", "GESTAO DE PESSOAS"),
    # GESTAO DE PESSOAS
    Area = str_replace_all(Area, ".*GESTAO FISCAL.*|.*GESTAO DE SERVICOS.*|.*LOGISTICA.*|.*GESTAO DE EMPRESAS.*|.*ADMINISTRACAO DE EMPRESAS.*|.*BUSINESS MANAGEMENT.*|
                           |.*NEGOCIOS.*|.*GESTAO EMPRESARIAL.*|.*STRATEGIC PLANNING.*|.*SEGURANCA DO TRABALHO.*|^BUSINESS$|^OPERACOES$", "GESTAO DE EMPRESAS"),
    # INFORMACAO
    Area = str_replace_all(Area, "^TECNOLOGIA$|.*COMPUTACAO.*|.*INFORMACAO.*|.*ANALISE DE SISTEMAS.*|.*ANALISE E DESENVOLVIMENTO DE SISTEMAS.*|
                           |.*BANCO DE DADOS.*|.*BIG DATA.*|.*INFORMATICA.*|.*SOFTWARE.*|.*INTELIGENCIA DE NEGOCIOS.*|
                           |.*COMPUTER.*|.*INFORMATION TECHNOLOGY.*|.*SYSTEM ANALYSIS.*|.*T[.]I[.]*|.*CIENCIA DA APRENDIZAGEM.*|
                           |.*REDES DE COMPUTADORES.*|.*INFORMATION.*|.*GAMIFICACAO EDUCACIONAL.*|.*DESENVOLVIMENTO DE GAMES.*|
                           |.*ANDROID.*|.*PROGRAMACAO DE COMPUTADORES.*|^TI$|WEB.*|.*ANALYSIS AND SYSTEMS DEVELOPMENT.*|
                           |.*COMPUTACIONAL SCIENCES.*|.*CIENCIA E TECNOLOGIA.*|.*PROJETOS DE TI.*", "INFORMACAO"),
    # INFRAESTRUTURA
    Area = str_replace_all(Area, ".*CONSTRUCAO CIVIL.*|.*ARQUITETURA.*|.*ENGENHARIA CIVIL.*|.*EDIFICACOES.*", "INFRAESTRUTURA"),
    # MARKETING
    Area = str_replace_all(Area, ".*GESTAO DE VAREJO.*|.*COMERCIO EXTERIOR.*|.*VENDEDOR.*|.*VENDAS.*|.*DESENVOLVIMENTO DE PRODUTOS.*|.*MARKETING.*|.*ECOMMERCE.*|.*EXPORTACAO.*|CONSULTOR DE VENDAS|
                                |.*SINISTRO AUTO.*|REGULADOR DE SINISTROS|CORRETOR DE SEGUROS.*|.*TECNICO DE SEGUROS.*|ESPECIALISTA EM SEGUROS", "MARKETING"),
    # MIDIA
    Area = str_replace_all(Area, ".*CINEMA.*", "MIDIA"),
    # MILITAR
    Area = str_replace_all(Area, ".*AERONAUTICA.*|.*AVIATION.*", "MILITAR"),
    # PROCESSOS INDUSTRIAIS
    Area = str_replace_all(Area, ".*PROCESSOS INDUSTRIAIS.*|.*INDUSTRIAL.*|.*PRODUCAO.*|.*MECANICA.*|.*MECHANICAL ENGINEERING.*|.*MECANICO.*|.*MECATRONICA.*|.*MECHATRONICS.*|
                           |.*4TH EDITION OF THE MSA MANUAL.*|.*TECNOLOGIA EM ENGENHARIA.*|^CONTROLE DA QUALIDADE$|.*DEVICES FOOLPROOF.*|.*ENGENHARIA DE MANUFATURA.*|
                           |.*ENGENHARIA DE MATERIAIS.*|.*TECNICO EM PLASTICO.*|.*SUPPLY QUALITY.*|.*QUALITY MANAGEMENT.*|.*	TECNICO EM PLASTICO.*|.*FMEA 4A EDICAO.*|.*MANUTENCAO DE AERONAVES.*", "PROCESSOS INDUSTRIAIS"),
    # RADIOLOGIA
    Area = str_replace_all(Area, ".*DIAGNOSTICO POR IMAGEM.*", "RADIOLOGIA"),
    # RECURSOS NATURAIS
    Area = str_replace_all(Area, ".*AGRICOLA.*|.*MEIO AMBIENTE.*|.*ENGENHARIA FLORESTAL.*|.*AGRONOMIA.*|.*AGRONOMICA.*|.*AGRICOLA.*
                           |.*AMBIENTAIS.*|.*METROLOGY.*|.*SANEAMENTO AMBIENTAL.*", "RECURSOS NATURAIS"),

    # QUIMICA
    Area = str_replace_all(Area, ".*QUIMICO.*|.*ENGENHARIA QUIMICA.*", "QUIMICA"),

    # ==============================================================================
    # ÁREAS UTILIZANDO OS CURSOS
    # ==============================================================================
    Area = case_when(str_detect(Area, ".*EXATAS.*") & str_detect(Curso, ".*ENGENHARIA MECATRONICA.*") ~ paste("PROCESSOS INDUSTRIAIS"), TRUE ~ Area),
    Area = case_when(str_detect(Area, ".*	FABRICACAO EM METAL.*") & str_detect(Curso, ".*MECANICO DE USINAGEM.*") ~ paste("PROCESSOS INDUSTRIAIS"), TRUE ~ Area),
    Area = case_when(str_detect(Area, "TECNOLOGIA") & str_detect(Curso, ".*ADS.*") ~ paste("INFORMACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Area, "TECNOLOGIA|ENGENHARIA") & str_detect(Curso, ".*GPI.*") ~ paste("PROCESSOS INDUSTRIAIS"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*ENSINO MEDIO.*") ~ paste("EDUCACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, "^ADMINISTRACAO$") ~ paste("ADMINISTRACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Area, ".*AGRONOMIA.*") ~ paste("RECURSOS NATURAIS"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*SOCIOLOGIA.*") ~ paste("CIENCIAS SOCIAIS"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*LETRAS.*|.*FISK.*") ~ paste("COMUNICACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*ADS.*|.*TECNICO EM INFORMATICA.*|.*TECNOLOGIA DA INFORMACAO.*|
                                |.*SISTEMAS DE INFORMACAO.*") ~ paste("INFORMACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*TECNICO EM AUTOMACAO.*|.*GPI.*|.*AUTOMACAO INDUSTRIAL.*|.*ENGENHARIA MECANICA.*|.*SISTEMAS INTEGRADOS DE GESTAO.*|
                                |.*ENGENHARIA DE PRODUCAO.*|.*USINAGEM MECANICA.*") ~ paste("PROCESSOS INDUSTRIAIS"), TRUE ~ Area),

    # ==============================================================================
    # CURSOS UTILIZANDO AS AREAS
    # ==============================================================================
    Curso = case_when(str_detect(Curso, ".*TECNOLOGO.*|.*TECHNOLOGIST.*") & str_detect(Area, "INFORMACAO") ~ paste("ADS"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^GRADUACAO") & str_detect(Area, ".*ANALISE.+SISTEMAS.*") ~ paste("ADS"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*CURSO.+EXTENSAO.*") & str_detect(Area, "MILITAR") ~ paste("CURSO DE EXTENSAO NA AREA", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*CURSO.+EXTENSAO.*") & str_detect(Area, "GESTAO E NEGOCIOS") ~ paste("CURSO DE EXTENSAO NA AREA DE", Area), TRUE ~ Curso),

    # ==============================================================================
    # EXTRAI OS ANOS DE INÍCIO E FIM DE CADA CURSO
    # ==============================================================================
    AnoIngresso = str_extract(Data, "^\\d{4}"),
    AnoEgresso = str_extract(Data, "\\d{4}$")
  ) %>%
  mutate(
    # ==============================================================================
    # REMOVE ESPAÇOS EM BRANCO NO INÍCIO E FINAL
    # ==============================================================================
    Nome = str_trim(Nome, side = c("both")),
    Nome = str_squish(Nome),
    Escola = str_trim(Escola, side = c("both")),
    Escola = str_squish(Escola),
    Curso = str_trim(Curso, side = c("both")),
    Curso = str_squish(Curso),
    Area = str_trim(Area, side = c("both")),
    Area = str_squish(Area)
  )
# ==============================================================================
# GRAVAÇÃO
# ==============================================================================
CSVs <- list.files("../Dashboard/", pattern = "EmpregosPadronizadoEvadidos.csv", full.names = TRUE)
file.remove(CSVs)
CSVs <- list.files("../Dashboard/", pattern = "EstudosPadronizadoEvadidos.csv", full.names = TRUE)
file.remove(CSVs)
write.csv(evadidosADS, "../Dashboard/EvadidosADS.csv", row.names = FALSE)
write.csv(evadidosGPI, "../Dashboard/EvadidosGPI.csv", row.names = FALSE)
write.csv(empregos, "../Dashboard/EmpregosPadronizadoEvadidos.csv", row.names = FALSE)
write.csv(estudos, "../Dashboard/EstudosPadronizadoEvadidos.csv", row.names = FALSE)
