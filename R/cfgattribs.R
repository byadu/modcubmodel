cfgattribs<- function(input, output, session) {
	ns<- session$ns

	output$dimensions<- renderDataTable(
		datatable(M$mt$dimensions, class='compact', filter='top',options=list(dom='<"bottom"itp><clear>', pageLength=10)),
		server=T
		)

	output$attribcols<- renderUI({
		catg<- input$category
		if(is.null(catg)) return(NULL)
		m<- getdims(M$cfg, catg)
		tab<- m[[1]]$md_table
		cols<- getcols(D$datadb, tab)
		fluidRow(
			column(2, textInput(ns('mname'), 'Attribute Name')),
			column(2, selectizeInput(ns('tab'), 'Table', tab, multiple=F)),
			column(2, selectizeInput(ns("col"), "Column", cols, multiple=F, options=list(create=T, placeholder='Select or Add'))),
			column(2, textInput(ns('wherec'), 'Where')),
			column(1, tags$div(style="line-height:25px;", br()), bsButton(ns("addattrib"),"Add Attribute",size='small',icon("save"),style='primary'))
			)
		})

	observeEvent(input$addattrib, {
		catg<- isolate(input$category)
		name<- isolate(input$mname)
		tab<- isolate(input$tab)
		col<- isolate(input$col)
		wherec<- isolate(input$wherec)
		addattrib(catg,name,tab,col,wherec)
		createAlert(session, ns("saveattrib"), ns("attribsaved"), title="", content=paste("Attribute saved:", strong(name)))
		}
		)
	}

cfgattribsUI<- function(id) {
	ns<- NS(id)
	cats<- list()
	dimgrp<- M$mt$dimgrp
	for(i in 1:nrow(dimgrp))
		cats[[dimgrp[i,2]]]<- dimgrp[i,1]
	fluidPage(
		fluidRow(
			column(2, selectizeInput(ns("category"), "Attributes Category", cats, multiple=F, options=list(create=T, placeholder='Select or Add'))),
			column(10, uiOutput(ns('attribcols')))
			),
		fluidRow(column(10, offset=1,bsAlert(ns("saveattrib")))),
		hr(),
		dataTableOutput(ns("dimensions"))
		)
	}
