# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# BIBLIOTECAS
# ==============================================================================
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(stringr)
library(dashboardthemes)
# ==============================================================================
# LEITURA DOS DADOS PADRONIZADOS
# ==============================================================================
estudos <- read.csv(file = "EstudosPadronizadoEvadidos.csv", header = TRUE, stringsAsFactors = FALSE)
empregos <- read.csv(file = "EmpregosPadronizadoEvadidos.csv", header = TRUE, stringsAsFactors = FALSE)
evadidosADS <- read.csv(file = "EvadidosADS.csv", header = TRUE, stringsAsFactors = FALSE)
evadidosGPI <- read.csv(file = "EvadidosGPI.csv", header = TRUE, stringsAsFactors = FALSE)
# ==============================================================================
# LEITURA DAS FUNÇÕES
# ==============================================================================
# GERAL
source("Funcoes/Geral/quantidadeEmpresasAtuais.r", encoding = "UTF-8")
source("Funcoes/Geral/quantidadeEvadidosEstudos.r", encoding = "UTF-8")
source("Funcoes/Geral/quantidadeEvadidosProfissional.r", encoding = "UTF-8")
source("Funcoes/Geral/totalDeEvadidos.r", encoding = "UTF-8")
source("Funcoes/Geral/totalDeEvadidosEncontrados.r", encoding = "UTF-8")
source("Funcoes/Geral/porcentagemPerfisEncontrados.r", encoding = "UTF-8")
source("Funcoes/Geral/porcentagemPerfisAcademicosComDados.r", encoding = "UTF-8")
source("Funcoes/Geral/porcentagemPerfisProfissionaisComDados.r", encoding = "UTF-8")
# DASHBOARD
source("Funcoes/TemaDashboard/TemaDashboard.r", encoding = "UTF-8")
# CONSULTAS/GRÁFICOS ACADÊMICOS
source("Funcoes/Consultas/Academico/ConsultaCursosConcluidos.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoCursosConcluidos.r", encoding = "UTF-8")
source("Funcoes/Consultas/Academico/ConsultaAnoDeEvasao.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoAnoDeEvasao.r", encoding = "UTF-8")
source("Funcoes/Consultas/Academico/ConsultaProsseguiuEstudosAposEvasao.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoProsseguiuEstudosAposEvasao.r", encoding = "UTF-8")
source("Funcoes/Consultas/Academico/ConsultaAreasCursosAposEvasao.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoAreasCursosAposEvasao.r", encoding = "UTF-8")
source("Funcoes/Consultas/Academico/ConsultaCursosAposEvasao.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoCursosAposEvasao.r", encoding = "UTF-8")
# CONSULTAS/GRÁFICOS PROFISSIONAIS
source("Funcoes/Consultas/Profissional/ConsultaCargosAtuais.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoCargosAtuais.r", encoding = "UTF-8")
source("Funcoes/Consultas/Profissional/ConsultaLocalizacaoEmpresas.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoLocalizacaoEmpresas.r", encoding = "UTF-8")
source("Funcoes/Consultas/Profissional/ConsultaNomeEmpresaAtual.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoNomeEmpresaAtual.r", encoding = "UTF-8")
source("Funcoes/Consultas/Profissional/ConsultaTipoEmpresa.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoTipoEmpresa.r", encoding = "UTF-8")
source("Funcoes/Consultas/Profissional/ConsultaAreasEvasaoEmprego.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoAreaAtualEmprego.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoTrabalhaAreaEvasao.r", encoding = "UTF-8")
# ==============================================================================
# DASHBOARD
# ==============================================================================
# INTERFACE DO USUÁRIO
ui <- dashboardPage(
  # CABEÇALHO
  dashboardHeader(
    title = "Evadidos IFSP",
    # CURSOS/MENU SUPERIOR
    tags$li(
      class = "dropdown",
      radioButtons("idCurso", "",
        c("ANÁLISE E DESENVOLVIMENTO DE SISTEMAS" = "idADS", "GESTÃO DA PRODUÇÃO INDUSTRIAL" = "idGPI"),
        inline = TRUE
      )
    ),
    # FAVICON DO IFSP
    tags$li(
      class = "dropdown link-ifsp",
      tags$a(
        href = "http://slt.ifsp.edu.br/portal/", target = "_blank",
        tags$img(height = "30px", alt = "IFSP Logo", src = "http://www.ifpb.edu.br/prpipg/inovacao/formulariosPasta/pasta-de-formularios/if.png")
      )
    )
  ),
  # TIPOS DE DADOS/MENU LATERAL ESQUERDO
  dashboardSidebar(
    sidebarMenu(
      menuItem("DADOS ACADÊMICOS", tabName = "estudos", icon = icon("graduation-cap")),
      menuItem("DADOS PROFISSIONAIS", tabName = "empregos", icon = icon("briefcase"))
    )
  ),
  # CORPO DA DASHBOARD
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "http://slt.ifsp.edu.br/portal/templates/padraogoverno01/favicon.ico")),
    tags$style(".sidebar-toggle{display:none !important}"),
    tags$style(".navbar-custom-menu, .main-header .navbar-right {float: none !important; display: flex !important; justify-content:space-around !important;}"),
    tags$style(".link-ifsp{position:absolute !important; right:0 !important;}"),
    TemaDashboard,
    tabItems(
      # DASHBOARD DE DADOS ACADÊMICOS
      tabItem(
        tabName = "estudos",
        fluidRow(
          valueBoxOutput("quantidadeEvadidosGeralAcademicos"),
          valueBoxOutput("quantidadeEvadidosPerfisEncontradosAcademicos"),
          valueBoxOutput("quantidadeEvadidosPerfisEncontradosComDadosAcademicos")
        ),
        # GRÁFICOS
        fluidRow(
          box(title = "Todos os cursos concluídos antes da evasão", status = "primary", solidHeader = T, plotlyOutput("cursosConcluidos"), width = 6, collapsible = TRUE),
          box(title = "Ano de evasão do curso", status = "primary", solidHeader = T, plotlyOutput("anoDeEvasao"), width = 6, collapsible = TRUE),
          box(title = "Prosseguiu nos estudos após a evasão", status = "primary", solidHeader = T, plotlyOutput("prosseguiuEstudosAposEvasao"), width = 6, collapsible = TRUE),
          box(title = "Áreas dos cursos concluídos após a evasão", status = "primary", solidHeader = T, plotlyOutput("areasCursosAposEvasao"), width = 6, collapsible = TRUE),
          box(title = "Cursos concluídos após a evasão", status = "primary", solidHeader = T, plotlyOutput("cursosAposEvasao"), width = 12, collapsible = TRUE)
        )
      ),
      # DASHBOARD DE DADOS PROFISSIONAIS
      tabItem(
        tabName = "empregos",
        fluidRow(
          valueBoxOutput("quantidadeEvadidosPerfisEncontradosComDadosProfissionais"),
          valueBoxOutput("quantidadeEmpresasAtuais")
        ),
        # GRÁFICOS
        fluidRow(
          box(title = "Alunos evadidos que trabalham na área do curso evadido", status = "primary", solidHeader = T, plotlyOutput("trabalhaAreaEvasao"), width = 6, collapsible = TRUE),
          box(title = "Áreas em que os alunos evadidos trabalham", status = "primary", solidHeader = T, plotlyOutput("areaAtualEmprego"), width = 6, collapsible = TRUE),
          box(title = "Classificação das empresas onde os alunos evadidos trabalham", status = "primary", solidHeader = T, plotlyOutput("tipoDaEmpresa"), width = 6, collapsible = TRUE),
          box(title = "Empresas onde os alunos evadidos trabalham (>= 2 estudantes evadidos)", status = "primary", solidHeader = T, plotlyOutput("nomeEmpresaAtual"), width = 6, collapsible = TRUE),
          box(title = "Cidades onde os alunos evadidos trabalham", status = "primary", solidHeader = T, plotlyOutput("localizacaoEmpresas"), width = 6, collapsible = TRUE),
          box(title = "Atuais cargos de trabalho (>= 2 estudantes evadidos)", status = "primary", solidHeader = T, plotlyOutput("cargosAtuais"), width = 6, collapsible = TRUE)
        )
      )
    )
  )
)

# ==============================================================================
# SERVIDOR
# ==============================================================================
# ==============================================================================
# CONSULTAS ACADÊMICAS
# ==============================================================================
server <- function(input, output) {
  # ==============================================================================
  # VARIÁVEIS GLOBAIS PARA ARGUMENTOS
  # ==============================================================================
  evadidos <- reactive(switch(input$idCurso,
    idADS = evadidosADS,
    idGPI = evadidosGPI
  ))

  siglaCurso <- reactive(switch(input$idCurso,
    idADS = "ADS",
    idGPI = "GPI"
  ))

  # ==============================================================================
  # CONSULTAS ACADÊMICAS
  # ==============================================================================
  # Quantidade de evadidos geral
  output$quantidadeEvadidosGeralAcademicos <- renderValueBox({
    valueBox(
      paste0(TotalDeEvadidos(siglaCurso()), " estudantes"), "Total de estudantes evadidos",
      icon = icon("address-book"),
      color = "light-blue"
    )
  })

  # Quantidade de perfis encontrados
  output$quantidadeEvadidosPerfisEncontradosAcademicos <- renderValueBox({
    valueBox(
      paste0(PorcentagemPerfisEncontrados(evadidos(), siglaCurso()), "% ", TotalDeEvadidosEncontrados(evadidos()), "/", TotalDeEvadidos(siglaCurso())), "Quantidade de perfis encontrados",
      icon = icon("address-book"),
      color = "light-blue"
    )
  })

  # Quantidade de perfis encontrados com dados acadêmicos
  output$quantidadeEvadidosPerfisEncontradosComDadosAcademicos <- renderValueBox({
    valueBox(
      paste0(PorcentagemPerfisAcademicosComDados(evadidos(), siglaCurso()), "% ", QuantidadeEvadidosEstudos(evadidos()), "/", TotalDeEvadidos(siglaCurso())), "Perfis com dados acadêmicos",
      icon = icon("address-book"),
      color = "light-blue"
    )
  })

  # Todos os cursos concluídos pelos estudantes evadidos antes da evasão
  output$cursosConcluidos <- renderPlotly({
    dataframe <- ConsultaCursosConcluidos(evadidos(), siglaCurso())
    GraficoCursosConcluidos(dataframe)
  })

  # Ano de evasão da graduação
  output$anoDeEvasao <- renderPlotly({
    dataframe <- ConsultaAnoDeEvasao(evadidos())
    GraficoAnoDeEvasao(dataframe)
  })

  # Mostra se o aluno evadido prosseguiu nos estudos após a evasão
  output$prosseguiuEstudosAposEvasao <- renderPlotly({
    dataframe <- ConsultaProsseguiuEstudosAposEvasao(evadidos(), siglaCurso())
    GraficoProsseguiuEstudosAposEvasao(dataframe)
  })

  # Quais as áreas dos cursos o aluno evadido fez após a evasão
  output$areasCursosAposEvasao <- renderPlotly({
    dataframe <- ConsultaAreasCursosAposEvasao(evadidos(), siglaCurso())
    GraficoAreasCursosAposEvasao(dataframe)
  })

  # Quais cursos o aluno evadido fez após a evasão
  output$cursosAposEvasao <- renderPlotly({
    dataframe <- ConsultaCursosAposEvasao(evadidos(), siglaCurso())
    GraficoCursosAposEvasao(dataframe)
  })
  # ==============================================================================
  # CONSULTAS PROFISSIONAIS
  # ==============================================================================
  # Quantidade de evadidos na parte profissional
  output$quantidadeEvadidosPerfisEncontradosComDadosProfissionais <- renderValueBox({
    valueBox(
      paste0(PorcentagemPerfisProfissionaisComDados(evadidos(), siglaCurso()), "% ", QuantidadeEvadidosProfissional(evadidos()), "/", TotalDeEvadidos(siglaCurso())), "Perfis com dados profissionais",
      icon = icon("address-book"),
      color = "light-blue"
    )
  })

  # Quantidade de empresas que empregam os alunos evadidos atualmente
  output$quantidadeEmpresasAtuais <- renderValueBox({
    valueBox(
      paste0(QuantidadeEmpresasAtuais(evadidos()), " empresas"), "Empregam os alunos evadidos",
      icon = icon("building"),
      color = "light-blue"
    )
  })

  # Mostra se o aluno evadido trabalha na área do curso evadido
  output$trabalhaAreaEvasao <- renderPlotly({
    dataframe <- ConsultaAreasEvasaoEmprego(evadidos(), siglaCurso())
    GraficoTrabalhaAreaEvasao(dataframe)
  })

  # Qual área o aluno evadido trabalha no último/atual emprego
  output$areaAtualEmprego <- renderPlotly({
    dataframe <- ConsultaAreasEvasaoEmprego(evadidos(), siglaCurso())
    GraficoAreaAtualEmprego(dataframe)
  })

  # Mostra se a empresa atual de trabalho é pública ou privada
  output$tipoDaEmpresa <- renderPlotly({
    dataframe <- ConsultaTipoEmpresa(evadidos())
    GraficoTipoEmpresa(dataframe)
  })

  # Localização da empresa no último/atual emprego
  output$localizacaoEmpresas <- renderPlotly({
    dataframe <- ConsultaLocalizacaoEmpresas(evadidos())
    GraficoLocalizacaoEmpresas(dataframe)
  })

  # Nome da empresa atual de trabalho
  output$nomeEmpresaAtual <- renderPlotly({
    dataframe <- ConsultaNomeEmpresaAtual(evadidos())
    GraficoNomeEmpresaAtual(dataframe)
  })

  # Atual cargo na ampresa no último/atual emprego
  output$cargosAtuais <- renderPlotly({
    dataframe <- ConsultaCargosAtuais(evadidos())
    GraficoCargosAtuais(dataframe)
  })
}

runApp(shinyApp(ui, server, options = list(port = 80)))
