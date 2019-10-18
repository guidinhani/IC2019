TemaDashboard <- shinyDashboardThemeDIY(

  # GERAL
  appFontFamily = "Roboto"
  , appFontColor = "rgb(0,0,0))"
  , bodyBackColor = "rgb(245,245,245)"

  # CABEÇALHO
  , logoBackColor = "rgb(10, 36, 48)"

  , headerButtonBackColor = "rgb(255,255,255)"
  , headerButtonIconColor = "rgb(0,0,0)"
  , headerButtonBackColorHover = "rgb(255,255,255)"
  , headerButtonIconColorHover = "rgb(0,0,0)"

  , headerBackColor = "rgb(255,255,255)"
  , headerBoxShadowColor = "rgb(170, 170, 170)"
  , headerBoxShadowSize = "2px 2px 2px"

  # BARRA LATERAL
  , sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    , colorStart = "rgb(10, 36, 48)"
    , colorMiddle = "rgb(22, 75, 99)"
    , colorEnd = "rgb(36, 102, 132)"
    , colorStartPos = 0
    , colorMiddlePos = 40
    , colorEndPos = 100
  )
  , sidebarPadding = 0

  , sidebarMenuBackColor = "transparent"
  , sidebarMenuPadding = 0
  , sidebarMenuBorderRadius = 0

  , sidebarShadowRadius = "2px 2px 2px"
  , sidebarShadowColor = "rgb(170, 170, 170)"

  , sidebarUserTextColor = "rgb(255,255,255)"

  , sidebarSearchBackColor = "rgb(255,255,255)"
  , sidebarSearchIconColor = "rgb(255,255,255)"
  , sidebarSearchBorderColor = "rgb(255,255,255)"

  , sidebarTabTextColor = "rgb(255,255,255)"
  , sidebarTabTextSize = 13
  , sidebarTabBorderStyle = "none none solid none"
  , sidebarTabBorderColor = "rgb(255,255,255)"
  , sidebarTabBorderWidth = 1

  , sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    , colorStart = "rgb(10, 36, 48)"
    , colorMiddle = "rgb(22, 75, 99)"
    , colorEnd = "rgb(26, 92, 122)"
    , colorStartPos = 0
    , colorMiddlePos = 5
    , colorEndPos = 100
  )
  , sidebarTabTextColorSelected = "rgb(255,255,255)"
  , sidebarTabRadiusSelected = "0px 0px 0px 0px"

  , sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    , colorStart = "rgb(20, 46, 58)"
    , colorMiddle = "rgb(32, 85, 109)"
    , colorEnd = "rgb(36, 102, 132)"
    , colorStartPos = 0
    , colorMiddlePos = 5
    , colorEndPos = 100
  )
  , sidebarTabTextColorHover = "rgb(255,255,255)"
  , sidebarTabBorderStyleHover = "none none solid none"
  , sidebarTabBorderColorHover = "rgb(255,255,255)"
  , sidebarTabBorderWidthHover = 1
  , sidebarTabRadiusHover = "0px 0px 0px 0px"

  # CAIXAS
  , boxBackColor = "rgb(255,255,255)"
  , boxBorderRadius = 5
  , boxShadowSize = "2px 2px 2px"
  , boxShadowColor = "rgb(170, 170, 170)"
  , boxTitleSize = 16
  , boxDefaultColor = "rgb(53, 152, 48)"
  ,boxPrimaryColor = "rgb(36, 102, 132)"
  , boxSuccessColor = "rgba(0,255,213,1)"
  , boxWarningColor = "rgb(244,156,104)"
  , boxDangerColor = "rgb(255,88,55)"

  , tabBoxTabColor = "rgb(255,255,255)"
  , tabBoxTabTextSize = 14
  , tabBoxTabTextColor = "rgb(0,0,0)"
  , tabBoxTabTextColorSelected = "rgb(0,0,0)"
  , tabBoxBackColor = "rgb(255,255,255)"
  , tabBoxHighlightColor = "rgba(44,222,235,1)"
  , tabBoxBorderRadius = 5

  # INPUTS
  , buttonBackColor = "rgb(245,245,245)"
  , buttonTextColor = "rgb(0,0,0)"
  , buttonBorderColor = "rgb(200,200,200)"
  , buttonBorderRadius = 5

  , buttonBackColorHover = "rgb(235,235,235)"
  , buttonTextColorHover = "rgb(100,100,100)"
  , buttonBorderColorHover = "rgb(200,200,200)"

  , textboxBackColor = "rgb(255,255,255)"
  , textboxBorderColor = "rgb(200,200,200)"
  , textboxBorderRadius = 5
  , textboxBackColorSelect = "rgb(245,245,245)"
  , textboxBorderColorSelect = "rgb(200,200,200)"

  # TABELAS
  , tableBackColor = "rgb(255,255,255)"
  , tableBorderColor = "rgb(240,240,240)"
  , tableBorderTopSize = 1
  , tableBorderRowSize = 1
)
