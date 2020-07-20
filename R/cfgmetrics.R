#' @export
#' @title cfgmetrics
#' @description Display, edit, create measures
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param M is the meta data connection structure
#' @param D is the data connection structure
cfgmetrics<- function(input, output, session, M, D) {
	ns<- session$ns

	output$measures<- renderDataTable(
		datatable(M$mt$measures, class='compact', filter='top',options=list(dom='<"bottom"itp><clear>', pageLength=10))
		)

	output$metricols<- renderUI({
		catg<- input$category
		if(is.null(catg)) return(NULL)
		m<- getmeas(M$cfg, catg)
		if(m[[1]]$md_id > 0) { 
			tab<- m[[1]]$md_table
			cols<- getcols(D$datadb, tab)
			tcols<- datecols(D$mydata, tab)
			fluidRow(
				column(2, textInput(ns('mname'), 'Metric Name')),
				column(2, selectizeInput(ns('tab'), 'Table', tab, multiple=F)),
				column(2, selectizeInput(ns("col"), "Column", cols, selected='', multiple=F, options=list(maxitems=1, create=T, placeholder='Select or Add'))),
				column(2, textInput(ns('wherec'), 'Where')),
				column(2, selectizeInput(ns("groupfun"), "Aggregate Function", c('count', 'sum', 'mean', 'avg', 'min', 'max'), selected='', multiple=F, options=list(maxitems=1, create=T, placeholder='Select or Add'))),
				column(2, selectizeInput(ns("timecol"), "Time Column", tcols, selected='', multiple=F, options=list(maxitems=1, create=F)))
				)
			}
		else {
			fluidRow(
				column(4, selectizeInput(ns('tabcat'), 'Table', M$dm$factabs, multiple=F)),
				column(2, tags$div(style="line-height:25px;", br()), bsButton(ns("addmetricatg"),"Add Metric Category",size='small',icon("save"),style='primary'))
			)
			}
		})

	observeEvent(input$addmetric, {
		catg<- isolate(input$category)
		name<- isolate(input$mname)
		tab<- isolate(input$tab)
		col<- isolate(input$col)
		wherec<- isolate(input$wherec)
		groupfun<- isolate(input$groupfun)
		tcol<- isolate(input$timecol)
		addmetric(M, catg, name,tab,col,wherec,groupfun,tcol)
		createAlert(session, ns("savemetric"), ns("metricsaved"), title="", content=paste("Metric saved:", strong(name)))
		}
		)
	
	observeEvent(input$addmetricatg, {
		catg<- isolate(input$category)
		tab<- isolate(input$tabcat)
		addmetricatg(M, catg,tab)
		createAlert(session, ns("savemetric"), ns("metricatgsaved"), title="Yo", content=paste("Metric Category saved:", strong(catg)))
		}
		)
	}

#' @export
#' @title cfgmetricsUI
#' @description UI for Display, edit, create measures
#' @param id is caller id
#' @param M is the meta data connection structure
cfgmetricsUI<- function(id, M) {
	ns<- NS(id)
	cats<- list()
	measgrp<- M$mt$measgrp
	for(i in 1:nrow(measgrp))
		cats[[measgrp[i,2]]]<- measgrp[i,1]
	fluidPage(
		fluidRow(
			column(2, selectizeInput(ns("category"), "Metrics Category", cats, selected='', multiple=F, options=list(maxitems=1, create=T, placeholder='Select or Add'))),
			column(offset=8, 2, tags$div(style="line-height:18px;", br(), bsButton(ns("addmetric")," Add This Metric",size='default',icon("save"),style='primary')))
			),
		uiOutput(ns('metricols')),
		fluidRow(column(10, offset=1,bsAlert(ns("savemetric")))),
		hr(),
		dataTableOutput(ns("measures"))
		)
	}
