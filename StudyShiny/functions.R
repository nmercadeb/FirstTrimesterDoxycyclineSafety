`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
num <- function(x) suppressWarnings(as.numeric(x))
temporalVariables <- c('lmp_year','exposure_start_year','exposure_start_gestational_week')
readExport <- function(file) {
  x <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE, na.strings = NULL)
  s <- x[x$variable_name == 'settings', c('result_id','estimate_name','estimate_value')]
  type <- s$estimate_value[s$estimate_name == 'result_type']
  if (!length(type)) stop('Missing result_type: ', file)
  w <- s[s$estimate_name == 'weighting', c('result_id','estimate_value')]
  names(w)[2] <- 'weighting'
  x <- x[x$variable_name != 'settings', , drop = FALSE]
  x <- dplyr::left_join(x, w, by = 'result_id')
  x$weighting[is.na(x$weighting)] <- 'none'
  list(type = type[1], rows = tibble::as_tibble(x))
}
getValues <- function(x) {
  fields <- intersect(c('cdm_name','group_level','strata_level','variable_name','variable_level','estimate_name','weighting'), names(x))
  stats::setNames(lapply(fields, function(z) sort(unique(x[[z]][!is.na(x[[z]]) & x[[z]] != 'NA']))), fields)
}
comparisonArms <- function(comparison) unique(unlist(strsplit(comparison, ' vs. ', fixed = TRUE)))
comparisonMatch <- function(x, comparison, kind) {
  if (!length(comparison)) return(x[0,,drop=FALSE])
  if (kind == 'table_one') return(x[x$group_level %in% unique(unlist(lapply(comparison, comparisonArms))),,drop=FALSE])
  if (kind == 'outcomes') return(x[x$group_level %in% comparison,,drop=FALSE])
  lookup <- vapply(comparison, function(z) { a <- comparisonArms(z); if(length(a)==2) paste(a[2], '&&&', a[1]) else '' }, character(1))
  x[x$group_level %in% lookup,,drop=FALSE]
}
selectResult <- function(x, kind, db, comparison, weighting = 'none', variables = NULL, exposures = NULL, strata = NULL) {
  if (!nrow(x) || !length(db)) return(x[0,,drop=FALSE])
  x <- x[x$cdm_name %in% db,,drop=FALSE]
  x <- comparisonMatch(x, comparison, kind)
  if (kind == 'table_one') {
    levels <- if ('none' %in% weighting) 'none' else character()
    if ('ps_weight' %in% weighting) levels <- c(levels, comparison)
    x <- x[x$strata_level %in% levels,,drop=FALSE]
  }
  if (kind == 'outcomes') x <- x[x$weighting %in% weighting & x$estimate_name != 'status',,drop=FALSE]
  if (length(exposures)) {
    field <- if (kind %in% c('table_one','ps_distribution','ps_coefficients','weights')) {
      if (kind == 'table_one') 'group_level' else 'variable_name'
    } else NULL
    if (!is.null(field) && kind != 'ps_coefficients') x <- x[x[[field]] %in% exposures,,drop=FALSE]
  }
  if (!is.null(variables)) {
    field <- if (kind == 'outcomes') 'variable_level' else 'variable_name'
    x <- x[x[[field]] %in% variables,,drop=FALSE]
  }
  if (!is.null(strata) && kind == 'weights') x <- x[x$strata_name %in% strata,,drop=FALSE]
  x
}
# Display labels, rather than raw OMOP storage columns.
tidyResult <- function(x, kind) {
  if (!nrow(x)) return(tibble::as_tibble(x))
  z <- tibble::as_tibble(x)
  names(z)[names(z) == 'cdm_name'] <- 'database'
  names(z)[names(z) == 'variable_name'] <- 'variable'
  names(z)[names(z) == 'variable_level'] <- 'level'
  names(z)[names(z) == 'estimate_name'] <- 'estimate'
  names(z)[names(z) == 'estimate_value'] <- 'value'
  names(z)[names(z) == 'estimate_type'] <- 'value_type'
  names(z)[names(z) == 'group_level'] <- 'comparison'
  names(z)[names(z) == 'strata_level'] <- 'stratum'
  if (kind == 'table_one') {
    names(z)[names(z) == 'comparison'] <- 'exposure'
    names(z)[names(z) == 'stratum'] <- 'comparison'
    z$weighting <- ifelse(z$comparison == 'none', 'none', 'ps_weight')
  }
  if (kind %in% c('ps_distribution','ps_coefficients','weights')) {
    z$comparison <- gsub('^([^&]+) &&& (.+)$', '\\2 vs. \\1', z$comparison)
  }
  z <- z[, c(intersect(c('database','comparison','exposure','weighting','stratum','variable','level','estimate','value'),names(z)),
             setdiff(names(z),c('database','comparison','exposure','weighting','stratum','variable','level','estimate','value'))),drop=FALSE]
  z
}
# Summary-based SMDs: unweighted pooled variance is used for both states.
makeSMD <- function(x, comparison) {
  if (length(comparison) != 1L) return(tibble::tibble())
  arms <- comparisonArms(comparison)
  if (length(arms) != 2L) return(tibble::tibble())
  x <- x[x$group_level %in% arms & x$strata_level %in% c('none',comparison) & x$estimate_name %in% c('mean','sd','percentage'),,drop=FALSE]
  if (!nrow(x)) return(tibble::tibble())
  x$value <- num(x$estimate_value)
  x$value[x$estimate_name == 'percentage'] <- x$value[x$estimate_name == 'percentage']/100
  x <- tidyr::pivot_wider(x[,c('strata_level','variable_name','variable_level','estimate_name','group_level','value')], names_from='group_level', values_from='value',values_fn=function(z) z[1])
  if (!all(arms %in% names(x))) return(tibble::tibble())
  x <- tidyr::pivot_wider(x, names_from='estimate_name', values_from=tidyselect::all_of(arms), names_sep='__')
  get <- function(arm, measure) {nm <- paste(arm,measure,sep='__'); if(nm %in% names(x)) x[[nm]] else rep(NA_real_,nrow(x))}
  p1 <- get(arms[1],'percentage'); p0 <- get(arms[2],'percentage')
  s1 <- get(arms[1],'sd'); s0 <- get(arms[2],'sd')
  categorical <- is.finite(p1) & is.finite(p0)
  x$delta <- ifelse(categorical,p1-p0,get(arms[1],'mean')-get(arms[2],'mean'))
  x$denom <- ifelse(categorical,sqrt((p1*(1-p1)+p0*(1-p0))/2),sqrt((s1^2+s0^2)/2))
  ref <- x[x$strata_level=='none',c('variable_name','variable_level','denom')]
  names(ref)[3] <- 'reference_sd'
  x <- dplyr::left_join(x,ref,by=c('variable_name','variable_level'))
  x$smd <- ifelse(is.finite(x$reference_sd)&x$reference_sd>0,x$delta/x$reference_sd,NA_real_)
  x$weighting <- ifelse(x$strata_level=='none','none','ps_weight')
  x[,c('variable_name','variable_level','weighting','smd')]
}
plotData <- function(x, panel, comparisons = character()) {
  if (!nrow(x)) return(NULL)
  if (panel == 'ps') {
    z <- tidyr::pivot_wider(x[x$estimate_name %in% c('ps_x','ps_y'),c('cdm_name','group_level','variable_name','variable_level','estimate_name','estimate_value')],names_from='estimate_name',values_from='estimate_value',values_fn=function(z) z[1])
    if (!all(c('ps_x','ps_y') %in% names(z))) return(NULL)
    z$ps_x <- num(z$ps_x);z$ps_y <- num(z$ps_y)
    z <- z[is.finite(z$ps_x)&is.finite(z$ps_y),]
    if(!nrow(z))return(NULL)
    return(ggplot2::ggplot(z,ggplot2::aes(ps_x,ps_y,colour=variable_name,group=interaction(cdm_name,group_level,variable_name)))+ggplot2::geom_line(linewidth=1)+ggplot2::facet_wrap(~cdm_name+group_level,scales='free_y')+ggplot2::labs(x='Propensity score',y='Density',colour='Exposure')+ggplot2::theme_bw())
  }
  if (panel == 'forest') {
    z <- tidyr::pivot_wider(x[x$estimate_name %in% c('coef','ci_lower','ci_upper'),c('cdm_name','group_level','weighting','variable_level','estimate_name','estimate_value')],names_from='estimate_name',values_from='estimate_value',values_fn=function(v)v[1])
    if(!all(c('coef','ci_lower','ci_upper')%in%names(z)))return(NULL)
    for(nm in c('coef','ci_lower','ci_upper'))z[[nm]]<-num(z[[nm]])
    z<-z[is.finite(z$coef)&is.finite(z$ci_lower)&is.finite(z$ci_upper)&z$coef>0&z$ci_lower>0,]
    if(!nrow(z))return(NULL)
    return(ggplot2::ggplot(z,ggplot2::aes(coef,reorder(variable_level,coef),colour=weighting))+ggplot2::geom_vline(xintercept=1,linetype=2)+ggplot2::geom_pointrange(ggplot2::aes(xmin=ci_lower,xmax=ci_upper),position=ggplot2::position_dodge(width=.5))+ggplot2::scale_x_log10()+ggplot2::facet_grid(cdm_name~group_level,scales='free_y')+ggplot2::labs(x='Odds ratio (95% CI)',y='Outcome')+ggplot2::theme_bw())
  }
  if (panel == 'temporal') x <- x[x$variable_name %in% temporalVariables & x$estimate_name=='percentage',]
  if (panel == 'population') x <- x[x$variable_name %in% c('number records','number subjects') & x$estimate_name=='count',]
  if (panel == 'coef') x <- x[x$estimate_name=='coefficient',]
  if (panel == 'weight') x <- x[x$estimate_name %in% c('count','mean_weight','sum_weights','effective_sample_size'),]
  if (panel == 'outcome') x <- x[x$estimate_name %in% c('events_exposed','events_comparator'),]
  if (panel == 'table') x <- x[x$estimate_name=='percentage',]
  if(!nrow(x))return(NULL)
  x$value <- num(x$estimate_value); x<-x[is.finite(x$value),]
  if(!nrow(x))return(NULL)
  x$category<-ifelse(is.na(x$variable_level)|x$variable_level=='NA',x$variable_name,x$variable_level)
  x
}
plotGeneric <- function(x, panel, colour='group_level',facet='none') {
  z<-plotData(x,panel)
  if(is.null(z))return(NULL)
  if(inherits(z,'ggplot'))return(z)
  z$colour_key<-if(colour %in% names(z))as.character(z[[colour]]) else 'All'
  p<-ggplot2::ggplot(z,ggplot2::aes(category,value,fill=colour_key))+ggplot2::geom_col(position='dodge')+ggplot2::theme_bw()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1))+ggplot2::labs(x=NULL,y=if(panel=='temporal')'Percentage' else 'Estimate',fill=colour)
  if(facet %in% names(z))p<-p+ggplot2::facet_wrap(stats::reformulate(facet),scales='free_x')
  p
}
# Prefer visOmopTable when the package is available and data is a valid summarised result.
# The tidy fallback is deliberately used for custom table layouts.
makeTidyTable <- function(x, header, groups, hide) {
  if(!nrow(x))return(tibble::tibble())
  cols<-names(x);header<-intersect(header,cols);groups<-intersect(groups,cols);hide<-intersect(hide,cols)
  ids<-setdiff(cols,c('value','value_type','result_id','group_name','strata_name','additional_name','additional_level',hide,header))
  z<-x[,unique(c(ids,header,'value')),drop=FALSE]
  if(length(header)) {
    z<-tidyr::unite(z,'.column',tidyselect::all_of(header),sep=' | ',remove=TRUE)
    z<-tidyr::pivot_wider(z,names_from='.column',values_from='value',values_fn=function(v)v[1])
  } else {
    z<-tidyr::pivot_wider(z,names_from='estimate',values_from='value',values_fn=function(v)v[1])
  }
  attr(z,'group_col')<-intersect(groups,names(z))[1] %||% NULL
  z
}
