library(shiny)
library(leaflet)
library(tjspApp)
library(shinydashboard)
library(dplyr)
library(stringr)

# global ----------------------------------------------------------------------
data(prod_tjsp_spr)
data(coma_m)

rm_accent <- function (x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))

fx <- function(x) {ifelse(x==-1, NA, x)}
dados <- prod_tjsp_spr %>%
  mutate(data=as.character(data)) %>%
  mutate_each(funs(fx)) %>%
  mutate(comarca=ifelse(municipio=='SAO SEBASTIAO DA GRAMA', 'SAO JOSE DO RIO PARDO', comarca)) %>%
  do(.[,colSums(is.na(.))<nrow(.)]) %>%
  mutate(entrancia=ifelse(str_detect(comarca, 'ARARAQUARA'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'ATIBAIA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'AVARE'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'BARUERI'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'BOTUCATU'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'BRAGANCA PAULISTA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'CAMPINAS'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'CATANDUVA'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'COTIA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'FERNANDOPOLIS'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'FRANCO DA ROCHA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'ITANHAEM'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'ITAPEVA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'JUNDIAI'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'LORENA'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'MIRASSOL'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'MOGI DAS CRUZES'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'PIRACICABA'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'RIBEIRAO PIRES'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'RIO CLARO'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'SANTA ISABEL'), 'INICIA', entrancia),
         entrancia=ifelse(str_detect(comarca, 'SAO CARLOS'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'SAO SEBASTIAO'), 'INTER', entrancia),
         entrancia=ifelse(str_detect(comarca, 'SOROCABA'), 'FINAL', entrancia),
         entrancia=ifelse(str_detect(comarca, 'TUPA'), 'INTER', entrancia)) %>%
  mutate(tipo_vara=ifelse(str_detect(nm_vara, 'FAZ') & planilha=='civel', 'fazen',
                          ifelse(str_detect(nm_vara, 'FAM') & planilha=='civel', 'famil',
                                 #ifelse(str_detect(nm_vara, 'UNI|CUMU') & planilha=='civel', 'cumul',
                                 planilha))) %>%
  filter(!str_detect(nm_vara, 'FALENCIAS')) %>%
  filter(!tipo_vara %in% c('jecri', 'infju', 'jeciv'))


prod_tjsp_spr %>%
  select(-starts_with('p_')) %>%
  filter(planilha == 'civel',
         !str_detect(nm_vara, 'FALENC|EMPRES')) %>%
  count(nm_vara)


lab_entrancias <- setNames(unique(dados$entrancia), c('Final', 'Inicial', 'Intermediária'))
lab_tipo_varas <- setNames(unique(dados$tipo_vara), c('Cível',
                                                      'Família e Sucessões',
                                                      'Fazenda Pública',
                                                      'Criminal',
                                                      'Execução Fiscal'))

# ui --------------------------------------------------------------------------
header <- dashboardHeader()
sidebar <- dashboardSidebar()
body <- dashboardBody(
  # Controles
  absolutePanel(top=0, left=0, fixed=TRUE, actionButton('show1', label='', icon=icon('gear')), style='z-index:100'),
  tags$script(sprintf("$('#show1').click(function(){$('#div-controls').toggle();})")),

  div(id="div-controls", class="", style="margin: 0px 0px 0px 10px;",
      absolutePanel(wellPanel(
        selectInput('entrancia', 'Entrância', lab_entrancias, selected='FINAL'),
        selectInput('tipo_vara', 'Tipo de vara', lab_tipo_varas, selected='civel'),
        numericInput('kmeans', 'Quantos grupos', value=2, min=1, max=5, step=1),
        checkboxInput('distritais', 'Mostrar municípios com foro distrital?', value=FALSE)),
        top=20, left=100, width=200, style='z-index:10;', draggable=TRUE)
  ),

  # Filtros
  absolutePanel(top=0, right=0, fixed=TRUE, actionButton('show2', label='', icon=icon('tasks')), style='z-index:100'),
  tags$script(sprintf("$('#show2').click(function(){$('#div-filters').toggle();})")),

  div(id="div-filters", class="niveis", style="margin: 0px 0px 0px 10px;",
      absolutePanel(wellPanel(
        dateRangeInput(inputId='corte_temporal', label='De',
                       min='2011-09-01', max='2014-07-01',
                       start='2011-09-01', end='2014-07-01',
                       format='dd/mm/yyyy', separator='até', language='pt-BR'),
        h5("Selecionar comarcas")#,
        # shinyTree("tree", checkbox=TRUE, search=TRUE)
        ),
        top=20, width=300, left='70%', style='z-index:10;', draggable=TRUE)
  ),

  fluidRow(column(12, leafletOutput('map', '100%', '800px')))
)

ui <- dashboardPage(header, sidebar, body)

# server ----------------------------------------------------------------------
server <- shinyServer(function(session, input, output) {

  map <- reactive({
    m <- leaflet() %>%
      addTiles('http://{s}.tiles.mapbox.com/v3/jtrecenti.map-oskm8vhn/{z}/{x}/{y}.png',
               'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(-48.7706, -22.46558, zoom = 7)
    m
  })

  output$map <- renderLeaflet({
    map()
  })
})

# app -------------------------------------------------------------------------
shinyApp(ui, server)
