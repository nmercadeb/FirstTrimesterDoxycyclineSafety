server <- function(input,output,session) {
  for(id in names(panelKinds)) local({
    panel<-id;kind<-panelKinds[[panel]];ns<-shiny::NS(panel)
    getInput<-function(field)input[[ns(field)]]
    # Controls outside module namespace are intentionally addressed by their literal IDs.
    selectedData<-shiny::reactive({
      x<-data[[kind]]
      shiny::req(nrow(x)>0)
      db<-input[[paste0(panel,'_db')]]
      cmp<-input[[paste0(panel,'_comparison')]]
      shiny::req(length(db)>0,length(cmp)>0)
      weighting<-input[[paste0(panel,'_weight')]] %||% c('none','ps_weight')
      vars<-input[[paste0(panel,'_variables')]]
      if(panel %in% c('table','temporal','balance','coef','outcome','forest') && is.null(vars)) vars<-character()
      if(panel=='balance')weighting<-c('none','ps_weight')
      x<-selectResult(x,kind,db,cmp,weighting,vars,
                      exposures=input[[paste0(panel,'_exposure')]],
                      strata=if(panel=='weight')input[[paste0(panel,'_strata')]] else NULL)
      if(panel=='population')x<-x[x$variable_name %in% c('number records','number subjects'),,drop=FALSE]
      if(panel=='table')x<-x[!x$variable_name %in% temporalVariables,,drop=FALSE]
      if(panel=='temporal')x<-x[x$variable_name %in% temporalVariables,,drop=FALSE]
      x
    })
    shiny::observeEvent(list(input[[paste0(panel,'_db')]],input[[paste0(panel,'_comparison')]]),{
      x<-data[[kind]]
      db<-input[[paste0(panel,'_db')]];cmp<-input[[paste0(panel,'_comparison')]]
      if(!length(db)||!length(cmp)||!nrow(x))return()
      z<-comparisonMatch(x[x$cdm_name %in% db,,drop=FALSE],cmp,kind)
      arms<-if(kind=='table_one')sort(unique(z$group_level)) else if(kind=='ps_distribution')sort(unique(z$variable_name)) else if(kind=='weights')sort(unique(z$variable_name)) else character()
      if(panel %in% c('population','temporal','weight','ps')){
        old<-input[[paste0(panel,'_exposure')]]
        shinyWidgets::updatePickerInput(session,paste0(panel,'_exposure'),choices=arms,selected=if(is.null(old))arms else intersect(old,arms))
      }
    },ignoreInit=FALSE)
    # Raw results are always the tidy representation.
    tidy<-shiny::reactive(tidyResult(selectedData(),kind))
    shiny::observeEvent(tidy(),{
      x<-tidy();if(!nrow(x))return()
      updatePicker<-function(field,opts){old<-getInput(field);shinyWidgets::updatePickerInput(session,ns(field),choices=opts,selected=if(is.null(old))opts else intersect(old,opts))}
      updatePicker('raw_cols',names(x))
      updatePicker('table_variables',sort(unique(x$variable)))
      updatePicker('table_levels',sort(unique(x$level[!is.na(x$level)])))
      updatePicker('table_estimates',sort(unique(x$estimate)))
    },ignoreInit=FALSE)
    output[[paste0(panel,'-raw')]]<-DT::renderDT({
      x<-tidy();shiny::validate(shiny::need(nrow(x)>0,if(panel=='temporal')'Temporal variables are absent from the supplied exports.'else 'No matching results'))
      cols<-intersect(getInput('raw_cols') %||% names(x),names(x))
      DT::datatable(x[,cols,drop=FALSE],filter='top',extensions='Buttons',options=list(scrollX=TRUE,autoWidth=TRUE,pageLength=15,dom='ftip'))
    })
    output[[paste0(panel,'-raw_download')]]<-shiny::downloadHandler(filename=function()paste0(panel,'_raw.csv'),content=function(file)utils::write.csv(tidy(),file,row.names=FALSE,na=''))
    tableData<-shiny::reactive({
      x<-tidy();if(!nrow(x))return(x)
      if(!is.null(getInput('table_variables')))x<-x[x$variable %in% getInput('table_variables'),,drop=FALSE]
      if(!is.null(getInput('table_levels')))x<-x[is.na(x$level)|x$level %in% getInput('table_levels'),,drop=FALSE]
      if(!is.null(getInput('table_estimates')))x<-x[x$estimate %in% getInput('table_estimates'),,drop=FALSE]
      if(panel=='table' && isTRUE(input$table_smd)) {
        cmp<-input$table_comparison
        if(length(cmp)==1L){s<-makeSMD(data$table_one[data$table_one$cdm_name %in% input$table_db,],cmp)
          if(nrow(s)){
            sm<-tibble::tibble(database=input$table_db[1],comparison=cmp,exposure=NA_character_,weighting=s$weighting,
              stratum=cmp,variable=s$variable_name,level=s$variable_level,estimate='SMD',value=as.character(round(abs(s$smd),3)))
            x<-dplyr::bind_rows(x,sm[,intersect(names(sm),names(x)),drop=FALSE])
          }
        }
      }
      x
    })
    tableGt<-shiny::reactive({
      x<-tableData();shiny::validate(shiny::need(nrow(x)>0,'No table rows for selection'))
      # Use OMOP's visOmopTable for standard OMOP result tables when no custom layout is requested.
      # For customised tidy layouts, gt renders the rearranged table.
      z<-makeTidyTable(x,header=getInput('bucket_header') %||% c('exposure','estimate'),
          groups=getInput('bucket_group') %||% 'database',hide=getInput('bucket_hide') %||% character())
      shiny::validate(shiny::need(nrow(z)>0,'No table values'))
      g<-attr(z,'group_col');if(length(g)&&!is.na(g)&&g %in% names(z))gt::gt(z,groupname_col=g)else gt::gt(z)
    })
    output[[paste0(panel,'-table')]]<-gt::render_gt(tableGt())
    output[[paste0(panel,'-table_download')]]<-shiny::downloadHandler(filename=function()paste0(panel,'_table.html'),content=function(file)gt::gtsave(tableGt(),file))
    makePlot<-shiny::reactive({
      x<-selectedData();if(!nrow(x))return(NULL)
      if(panel=='balance') {
        cmp<-input$balance_comparison
        if(length(cmp)!=1L)return(NULL)
        raw<-data$table_one
        raw<-raw[raw$cdm_name %in% input$balance_db,,drop=FALSE]
        z<-makeSMD(raw,cmp)
        vars<-input$balance_variables
        if(!is.null(vars))z<-z[z$variable_name %in% vars,,drop=FALSE]
        weights<-input$balance_weight %||% c('none','ps_weight')
        z<-z[z$weighting %in% weights & is.finite(z$smd),,drop=FALSE]
        if(!nrow(z))return(NULL)
        z$characteristic<-ifelse(is.na(z$variable_level)|z$variable_level=='NA',z$variable_name,paste(z$variable_name,z$variable_level,sep=': '))
        colour<-getInput('colour') %||% 'weighting'
        z$colour_key<-if(colour=='none')'All' else if(colour=='weighting')z$weighting else if(colour=='variable_name')z$variable_name else z$weighting
        p<-ggplot2::ggplot(z,ggplot2::aes(abs(smd),reorder(characteristic,abs(smd)),colour=colour_key))+ggplot2::geom_vline(xintercept=.1,linetype=2)+ggplot2::geom_point(size=2)+ggplot2::labs(x='Absolute SMD',y=NULL,colour=colour)+ggplot2::theme_bw()
        if(length(input$balance_db)>1)p<-p+ggplot2::facet_wrap(~weighting)
        return(p)
      }
      if(panel=='forest'){
        z<-plotData(x,'forest')
        if(is.null(z))return(NULL)
        colour<-getInput('colour') %||% 'weighting'
        if(colour=='none')z<-z+ggplot2::guides(colour='none')
        return(z)
      }
      if(panel=='ps')return(plotData(x,'ps'))
      colour<-getInput('colour') %||% 'group_level';facet<-getInput('facet') %||% 'none'
      plotGeneric(x,panel,colour,facet)
    })
    output[[paste0(panel,'-plot')]]<-shiny::renderPlot({p<-makePlot();shiny::validate(shiny::need(!is.null(p),'No plottable results for this selection'));p})
    output[[paste0(panel,'-plotly')]]<-plotly::renderPlotly({p<-makePlot();shiny::validate(shiny::need(!is.null(p),'No plottable results for this selection'));plotly::ggplotly(p)})
    output[[paste0(panel,'-plot_download')]]<-shiny::downloadHandler(filename=function()paste0(panel,'_plot.png'),content=function(file){p<-makePlot();shiny::req(!is.null(p));ggplot2::ggsave(file,plot=p,width=getInput('plot_width') %||% 10,height=getInput('plot_height') %||% 6,dpi=getInput('plot_dpi') %||% 300)})
  })
}
