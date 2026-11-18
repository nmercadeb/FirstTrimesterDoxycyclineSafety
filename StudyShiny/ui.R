panelKinds <- c(population='table_one',table='table_one',temporal='table_one',balance='table_one',ps='ps_distribution',coef='ps_coefficients',weight='weights',outcome='outcomes',forest='outcomes')
plotPanels <- c('population','temporal','balance','ps','coef','weight','outcome','forest')
filterUI <- function(id) {
  kind <- panelKinds[[id]]
  vars <- choices[[kind]]$variable_name %||% character()
  if(kind=='outcomes')vars<-choices[[kind]]$variable_level %||% character()
  if(id=='temporal')vars<-intersect(vars,temporalVariables)
  if(id=='table')vars<-setdiff(vars,temporalVariables)
  if(id=='population')vars<-intersect(vars,c('number records','number subjects'))
  shiny::tagList(
    shinyWidgets::pickerInput(paste0(id,'_db'),'Database',choices=databases,selected=databases,multiple=TRUE,options=list(`actions-box`=TRUE)),
    shinyWidgets::pickerInput(paste0(id,'_comparison'),'Comparison',choices=comparisons,selected=comparisons[1],multiple=TRUE,options=list(`actions-box`=TRUE,`selected-text-format`='count > 2')),
    if(kind %in% c('table_one','outcomes')) shinyWidgets::pickerInput(paste0(id,'_weight'),'Weighting',choices=c('Crude'='none','PS weighted'='ps_weight'),selected=if(id=='balance')c('none','ps_weight')else 'none',multiple=TRUE,options=list(`actions-box`=TRUE)),
    if(id %in% c('table','temporal','balance','coef','outcome','forest'))shinyWidgets::pickerInput(paste0(id,'_variables'),if(kind=='outcomes')'Outcomes' else 'Variables',choices=vars,selected=vars,multiple=TRUE,options=list(`actions-box`=TRUE,`selected-text-format`='count > 3',size=10)),
    if(id %in% c('population','temporal','weight','ps'))shinyWidgets::pickerInput(paste0(id,'_exposure'),'Exposure',choices=character(),selected=character(),multiple=TRUE,options=list(`actions-box`=TRUE)),
    if(id=='weight')shiny::radioButtons(paste0(id,'_strata'),'Weight summaries',choices=c('Overall'='overall','By PS stratum'='ps_stratum'),selected='overall',inline=TRUE),
    if(id=='table')shiny::checkboxInput(paste0(id,'_smd'),'Include SMD',FALSE)
  )
}
smallButton <- function(id,iconName)shiny::actionButton(id,NULL,icon=shiny::icon(iconName),class='btn btn-sm btn-outline-secondary')
downloadPopup <- function(ns,type) bslib::popover(
  smallButton(ns(paste0(type,'_open')),'download'),
  if(type=='plot')shiny::tagList(shiny::numericInput(ns('plot_width'),'Width (inches)',10,min=2,max=40),shiny::numericInput(ns('plot_height'),'Height (inches)',6,min=2,max=40),shiny::numericInput(ns('plot_dpi'),'DPI',300,min=72,max=1200)),
  shiny::downloadButton(ns(paste0(type,'_download')),'Download',class='btn btn-sm btn-primary'),title='Download options',placement='bottom'
)
panelUI <- function(id,title,plot=TRUE,plotOnly=FALSE) {
  ns<-shiny::NS(id)
  raw<-bslib::nav_panel('Raw results',bslib::card(
    bslib::card_header(title,downloadPopup(ns,'raw')),
    bslib::layout_sidebar(sidebar=bslib::sidebar(position='right',shinyWidgets::pickerInput(ns('raw_cols'),'Columns to display',choices=character(),multiple=TRUE,options=list(`actions-box`=TRUE))),
      shiny::div(style='width:100%;overflow-x:auto;',DT::DTOutput(ns('raw'))))))
  tab<-bslib::nav_panel('Table',bslib::card(
    bslib::card_header(title,downloadPopup(ns,'table'),
      bslib::popover(smallButton(ns('table_layout_open'),'sliders'),
        sortable::bucket_list(header=NULL,
          sortable::add_rank_list('None',labels=c('variable','level'),input_id=ns('bucket_none')),
          sortable::add_rank_list('Header',labels=c('exposure','estimate'),input_id=ns('bucket_header')),
          sortable::add_rank_list('Group columns',labels=c('database'),input_id=ns('bucket_group')),
          sortable::add_rank_list('Hide',labels=c('comparison','weighting'),input_id=ns('bucket_hide'))),title='Table layout',placement='bottom'),
      bslib::popover(smallButton(ns('table_fields_open'),'filter'),
        shinyWidgets::pickerInput(ns('table_variables'),'Variable names',choices=character(),multiple=TRUE,options=list(`actions-box`=TRUE)),
        shinyWidgets::pickerInput(ns('table_levels'),'Variable levels',choices=character(),multiple=TRUE,options=list(`actions-box`=TRUE)),
        shinyWidgets::pickerInput(ns('table_estimates'),'Estimates',choices=character(),multiple=TRUE,options=list(`actions-box`=TRUE)),title='Table fields')),
    shiny::div(style='width:100%;overflow-x:auto;',gt::gt_output(ns('table')))))
  graph<-bslib::nav_panel('Plot',bslib::card(
    bslib::card_header(title,downloadPopup(ns,'plot')),
    bslib::layout_sidebar(sidebar=bslib::sidebar(position='right',
      if(id!='ps')shiny::selectInput(ns('colour'),'Colour by',choices=c('Weighting'='weighting','Exposure'='group_level','Database'='cdm_name','None'='none'),selected=if(id%in%c('forest','balance'))'weighting'else 'group_level'),
      if(id!='ps')shiny::selectInput(ns('facet'),'Facet by',choices=c('None'='none','Database'='cdm_name','Comparison'='group_level','Variable'='variable_name'),selected='none'),
      shiny::radioButtons(ns('mode'),'Display',choices=c('Interactive'='interactive','Static ggplot'='static'),selected='interactive',inline=TRUE)),
      shiny::conditionalPanel(sprintf("input['%s'] == 'static'",ns('mode')),shiny::plotOutput(ns('plot'),height='550px')),
      shiny::conditionalPanel(sprintf("input['%s'] == 'interactive'",ns('mode')),plotly::plotlyOutput(ns('plotly'),height='550px')))))
  if(plotOnly)return(bslib::navset_card_tab(graph))
  do.call(bslib::navset_card_tab,c(list(raw,tab),if(plot)list(graph)else list()))
}
ui<-bslib::page_navbar(title='Doxycycline Safety During Pregnancy',theme=bslib::bs_theme(bootswatch='flatly'),
  bslib::nav_panel('Background',bslib::card(shiny::includeMarkdown('background.md'))),
  bslib::nav_menu('Population Characteristics',
    bslib::nav_panel('Population Overview',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('population')),panelUI('population','Population Overview'))),
    bslib::nav_panel('Table 1',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('table')),panelUI('table','Table 1',plot=FALSE))),
    bslib::nav_panel('Temporal Distributions',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('temporal')),panelUI('temporal','Temporal Distributions'))),
    bslib::nav_panel('Covariate Balance',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('balance')),panelUI('balance','Covariate Balance')))),
  bslib::nav_menu('PS Diagnostics',
    bslib::nav_panel('PS Distribution',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('ps')),panelUI('ps','PS Distribution',plotOnly=TRUE))),
    bslib::nav_panel('PS Coefficients',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('coef')),panelUI('coef','PS Coefficients'))),
    bslib::nav_panel('Weight Distribution',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('weight')),panelUI('weight','Weight Distribution')))),
  bslib::nav_menu('Outcome Analysis',
    bslib::nav_panel('Outcome Overview',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('outcome')),panelUI('outcome','Outcome Overview'))),
    bslib::nav_panel('Forest Plot',bslib::layout_sidebar(sidebar=bslib::sidebar(filterUI('forest')),panelUI('forest','Forest Plot')))))
