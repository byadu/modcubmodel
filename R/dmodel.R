#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @import shinyBS
#' @import DT
#' @import chorddiag
#' @import DBI
#' @import RMySQL
#' @import libcubmeta

#' @export
#' @title dmodel
#' @description Display, edit, create data model / table joins
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param M is the meta data connection structure
#' @param D is the data connection structure
dmodel<- function(input, output, session, M, D) {
	ns<- session$ns
	output$dmodel<- renderDataTable(
		datatable(M$tjoins, class='compact', filter='top',options=list(dom='<"bottom"itp><clear>', pageLength=12))
		)
	observeEvent(input$factab, ignoreInit=T, ignoreNULL=T, {
		req(input$factab)
		dims<- filterdimtabs(input$factab, M)
		factcols<- getcols(D$datadb, input$factab)
		updateSelectizeInput(session, 'factcol', selected=factcols[1], choices=factcols)
		updateSelectizeInput(session, 'dimtab', selected=dims[1], choices=dims)
		}
		)
	observeEvent(input$dimtab, ignoreInit=T, ignoreNULL=T, {
		req(input$dimtab)
		dimcols<- getprimary(D$mydata, input$dimtab)
		updateSelectizeInput(session, 'dimcol', selected=dimcols[1], choices=dimcols)
		}
		)
	observeEvent(input$Add, ignoreInit=T, ignoreNULL=T, {
		fact<- isolate(input$factab)
		dim<- isolate(input$dimtab)
		fcol<- isolate(input$factcol)
		dcol<- isolate(input$dimcol)
		addjoin(M, fact,dim,fcol,dcol)
		createAlert(session, ns("joinsave"), ns("joinsaved"), title="", content=paste("Join saved:", strong(fact)))
		}
		)
	output$chordmodel <- renderChorddiag({
		chorddiag(M$model, type="bipartite",showTicks=F,categoryNames=c("Facts", "Dimensions"),categorynamePadding=80, categorynameFontsize=14, groupnamePadding=5,groupnameFontsize=10)
    	})
	}

#' @export
#' @title cfgattribsUI
#' @description UI for Display, edit, create attributes
#' @param id is caller id
#' @param M is the meta data connection structure
dmodelUI<- function(id, M) {
	ns<- NS(id)

	tabsetPanel(
	tabPanel("Diagram", chorddiagOutput(ns('chordmodel'), height='650px'), tags$i("Top 8 Fact and Dimension Tables (by #connects) shown.")),
	tabPanel("Table",
		fluidPage(
		strong("Add New Join"),
		fluidRow(
			column(3, selectizeInput(ns('factab'), 'Fact', M$dm$factabs, multiple=F, options=list(placeholder='Select Fact'))),
			column(3, selectizeInput(ns('dimtab'), 'Dimension', '', multiple=F, options=list(placeholder='Select Dimension'))),
			column(3, selectizeInput(ns('factcol'), 'Fact Column', '', multiple=F, options=list(placeholder='Fact Column'))),
			column(3, selectizeInput(ns('dimcol'), 'Dimension Column', '', multiple=F, options=list(placeholder='Dimension Column')))
		),
		column(3, bsAlert(ns('joinsave'))),
		column(1, offset=11, bsButton(ns('Add'), 'Add', size='large', style='primary')),
		hr(),
		dataTableOutput(ns("dmodel"))
		)
		)
	)
	}

#' @importFrom rlang .data
filterdimtabs<- function(fact, M) {
	currdims<- filter(M$tjoins, .data$Fact_Table==!!fact) %>% select(.data$Dimension_Table) 
	currdims<- currdims[,1]
	setdiff(M$dm$dimtabs, currdims)
	}

addjoin<- function(M, fact, dim, fcol, dcol) {
	tj<- as.data.frame(cbind(fact[1],dim[1],fcol[1],dcol[1]))
	colnames(tj)<- c('tj_tab1', 'tj_tab2', 'tj_col1', 'tj_col2')
	print(tj)
	dbWriteTable(M$mycfg, 'table_joins', tj, row.names=F, append=T)
	cat("addjoin", fact, dim, fcol, dcol, "\n")
	}

