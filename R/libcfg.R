getnextid<- function(catg, md) {
	catg<- as.integer(catg)
	if(md == 'm')
		mgid<- M$mt$measgrp$md_id
	else
		mgid<- M$mt$dimgrp$md_id
	mgid<- mgid[order(mgid)]
	nextcatg<- mgid[mgid>catg][1]
	if(is.na(nextcatg))
		nextcatg<- ifelse(md=='m', 50000, 10000)
	cat('\nnextcat', nextcatg, '\n')

	if(md == 'm')
		items<- M$mt$measures
	else
		items<- M$mt$dimensions
	mdids<- items$md_id
	mdids<- mdids[order(mdids)]
	mdids<- mdids[mdids>catg & mdids < nextcatg]
	if(length(mdids) > 0)
		nextid<- max(mdids)+1
	else
		nextid<- catg+1
	cat('\nnextid', nextid, '\n')
	nextid
	}


addattrib<- function(catg, name, tab, col, where) {
	cat("addattrib", catg, name, tab, col, where, '\n') 
	id<- getnextid(catg, 'd')
	q<- paste("insert into menu_trees values(", id, ",", catg, ",'common', 'DisplayF')")
	dbExecute(M$mycfg, q)
	q<- paste0("insert into menu_dtls(md_id, md_name, md_table, md_column, md_where) values(", id, ",'", name, "','",  tab, "','", col, "','", where, "')")
	print(q)
	dbExecute(M$mycfg, q)
	}

addmetric<- function(catg, name, tab, col, where, sumcnt, tcol) {
	cat("addmetric", catg, name, tab, col, where, sumcnt, tcol, '\n') 
	id<- getnextid(catg, 'm')
	q<- paste("insert into menu_trees values(", id, ",", catg, ",'common', 'DisplayF')")
	print(q)
	dbExecute(M$mycfg, q)
	q<- paste0("insert into menu_dtls(md_id, md_name, md_table, md_column, md_where, md_sumcnt, md_timecol) values(", id, ",'", name, "','",  tab, "','", col, "','", where, "','", sumcnt, "','", tcol, "')")
	print(q)
	dbExecute(M$mycfg, q)
	}
getnextmetricatg<- function() {
	M$mt$measgrp[nrow(M$mt$measgrp),1] + 100
	}
addmetricatg<- function(catg, tab) {
	id<- getnextmetricatg()
	q<- paste("insert into menu_trees values(", id, ",10000, 'common', 'DisplayF')")
	print(q)
	dbExecute(M$mycfg, q)
	q<- paste0("insert into menu_dtls(md_id, md_name, md_table, md_column) values(", id, ",'", catg, "','",  tab, "','')")
	print(q)
	dbExecute(M$mycfg, q)
	}

