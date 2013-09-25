datacache <- new.env(hash=TRUE, parent=emptyenv())

MeSH <-  function () get("dbshow", envir=datacache)()
MeSH_dbconn <- function() get("dbconn", envir=datacache)
MeSH_dbfile <- function()  get("dbfile", envir=datacache)
MeSH_dbschema <- function() get("dbschema", envir=datacache)()
MeSH_dbInfo <- function() get("dbInfo", envir=datacache)()

.onLoad <- function(libname, pkgname)
{
	require("methods", quietly=TRUE)
    dbfile <- system.file("extdata", "mesh.sqlite", package=pkgname, lib.loc=libname)
	assign("dbfile", dbfile, envir=datacache)
	
	driver <- dbDriver("SQLite")
	db <- dbfile
	dbconn <- dbConnect(driver, db)
    assign("dbconn", dbconn, envir=datacache)
	
	dbshow <- function(){
		cat("Quality control information for MeSH:\n\n\n")
		cat("This package has the following mappings:\n\n")
		print(dbGetQuery(dbconn, "SELECT * FROM MAPCOUNTS;"))
		cat("Additional Information about this package:\n")
		cat("Date for MeSH data: 20120907\n")
	}
	assign("dbshow", dbshow, envir=datacache)

	dbschema <-  function() cat(dbGetQuery(dbconn, "SELECT * FROM sqlite_master;")$sql)
	assign("dbschema", dbschema, envir=datacache)

	dbInfo <- function() dbGetQuery(dbconn, "SELECT * FROM METADATA;")
	assign("dbInfo", dbInfo, envir=datacache)


	ns <- asNamespace(pkgname)
	
	#Definition of Classes
	setClass("MeSHTERM", representation(name="character"), prototype(name="MeSHTERM"))
	setClass("MeSHSYNONYM", representation(name="character"), prototype(name="MeSHSYNONYM"))
	setClass("MeSHQUALIFIER", representation(name="character"), prototype(name="MeSHQUALIFIER"))
	setClass("MeSHMAPCOUNTS", representation(name="character"), prototype(name="MeSHMAPCOUNTS"))
	setClass("MeSHAOR", representation(name="character", objectname="character"), prototype(name="MeSHAOR", objectname="NONAME"))
	setClass("MeSHPCR", representation(name="character", objectname="character"), prototype(name="MeSHPCR", objectname="NONAME"))
	
	# Definition of Methods
	# columns
	setMethod("columns", "MeSHTERM", function(x){return(c("MESHID", "MESHTERM", "MESHCATEGORY"))})
	setMethod("columns", "MeSHSYNONYM", function(x){return(c("MESHID", "MESHSYNONYM"))})
	setMethod("columns", "MeSHQUALIFIER", function(x){return(c("QUALIFIERID", "SUBHEADING", "MESHID"))})
	setMethod("columns", "MeSHMAPCOUNTS", function(x){return(c("MAPNAME", "COUNT"))})
	setMethod("columns", "MeSHAOR", function(x){return(c("ANCESTORMESHID", "OFFSPRINGMESHID"))})
	setMethod("columns", "MeSHPCR", function(x){return(c("PARENTMESHID", "CHILDMESHID"))})

	# keytypes
	setMethod("keytypes", "MeSHTERM", function(x){return(c("MESHID", "MESHTERM", "MESHCATEGORY"))})
	setMethod("keytypes", "MeSHSYNONYM", function(x){return("MESHID")})
	setMethod("keytypes", "MeSHQUALIFIER", function(x){return(c("QUALIFIERID", "MESHID"))})
	setMethod("keytypes", "MeSHMAPCOUNTS", function(x){return("MAPNAME")})
	setMethod("keytypes", "MeSHAOR", function(x){return(c("ANCESTORMESHID", "OFFSPRINGMESHID"))})
	setMethod("keytypes", "MeSHPCR", function(x){return(c("PARENTMESHID", "CHILDMESHID"))})

	# keys
	setMethod("keys", "MeSHTERM", function(x, keytype){
		query <- paste0("SELECT ", keytype, " FROM MeSHTERM;")
		k <- dbGetQuery(MeSH_dbconn(), query)
		return(unique(k))
	})
	setMethod("keys", "MeSHSYNONYM", function(x, keytype){
		keytype <- "MESHID"
		query <- paste0("SELECT ", keytype, " FROM MeSHSYNONYM;")
		k <- dbGetQuery(MeSH_dbconn(), query)
		return(unique(k))
	})
	setMethod("keys", "MeSHQUALIFIER", function(x, keytype){
		query <- paste0("SELECT ", keytype, " FROM QUALIFIER;")
		k <- dbGetQuery(MeSH_dbconn(), query)
		return(unique(k))
	})
	setMethod("keys", "MeSHMAPCOUNTS", function(x, keytype){
		keytype <- "MAPNAME"
		query <- paste0("SELECT ", keytype, " FROM MAPCOUNTS;")
		k <- dbGetQuery(MeSH_dbconn(), query)
		return(k)
	})
	setMethod("keys", "MeSHAOR", function(x, keytype){
		query <- paste0("SELECT ", keytype, " FROM ",x@objectname,";")
		k <- dbGetQuery(MeSH_dbconn(), query)
		return(unique(k))
	})
	setMethod("keys", "MeSHPCR", function(x, keytype){
		query <- paste0("SELECT ", keytype, " FROM ",x@objectname,";")
		k <- dbGetQuery(MeSH_dbconn(), query)
		return(unique(k))
	})

	# vector dividing function
	div <- function(x,d=1){
		y <- list()
		delta <- ceiling(length(x)/d)
			for(i in 1:d){
				y[[i]] <- as.vector(na.omit(x[((i-1)*delta+1):(i*delta)]))
			}
		return(y)
	}

	# select
	setMethod("select", "MeSHTERM",
		function(x, keys, columns, keytype){
		keys <- unlist(keys)
		kee <- c()
			if(length(columns) > 1){
				c <- columns[1]
					for(i in 2:(length(columns))){
						c<- paste(c, columns[i], sep=",")
					}
			}else{
			c <- columns
			}
		keys <- paste0('"',keys,'"')
		ke <- paste(keytype, keys, sep="=")
		if(length(ke) > 1){
			if(length(ke) >= 1000){
				ke_loc <- div(1:length(ke),ceiling(length(keys)/500))
				for(j in 1:ceiling(length(keys)/500)){
					kee[j] <- paste(ke[ke_loc[[j]]], sep="",collapse=" OR ")
				}
			}else{
				kee <- paste(ke, sep="", collapse=" OR ")
			}
		}else{
			kee <- ke
		}
		kk <- c()
		for(i in 1:length(kee)){
			query <- paste0("SELECT ", c, " FROM MeSHTERM WHERE ", kee[i],";")
			k <- dbGetQuery(MeSH_dbconn(), query)
			kk <- rbind(kk,k)
		}
		return(unique(kk))
		}
	)


	
		setMethod("select", "MeSHSYNONYM",
		function(x, keys, columns, keytype){
		keys <- unlist(keys)
		keytype <- "MESHID"
		kee <- c()
			if(length(columns) > 1){
				c <- columns[1]
					for(i in 2:(length(columns))){
						c<- paste(c, columns[i], sep=",")
					}
			}else{
			c <- columns
			}
		keys <- paste0('"',keys,'"')
		ke <- paste(keytype, keys, sep="=")
		if(length(ke) > 1){
			if(length(ke) >= 1000){
				ke_loc <- div(1:length(ke),ceiling(length(keys)/500))
				for(j in 1:ceiling(length(keys)/500)){
					kee[j] <- paste(ke[ke_loc[[j]]], sep="",collapse=" OR ")
				}
			}else{
				kee <- paste(ke, sep="", collapse=" OR ")
			}
		}else{
			kee <- ke
		}
		kk <- c()
		for(i in 1:length(kee)){
			query <- paste0("SELECT ", c, " FROM MeSHSYNONYM WHERE ", kee[i],";")
			k <- dbGetQuery(MeSH_dbconn(), query)
			kk <- rbind(kk,k)
		}
		return(unique(kk))
		}
	)
	
		setMethod("select", "MeSHQUALIFIER",
		function(x, keys, columns, keytype){
		keys <- unlist(keys)
		kee <- c()
			if(length(columns) > 1){
				c <- columns[1]
					for(i in 2:(length(columns))){
						c<- paste(c, columns[i], sep=",")
					}
			}else{
			c <- columns
			}
		keys <- paste0('"',keys,'"')
		ke <- paste(keytype, keys, sep="=")
		if(length(ke) > 1){
			if(length(ke) >= 1000){
				ke_loc <- div(1:length(ke),ceiling(length(keys)/500))
				for(j in 1:ceiling(length(keys)/500)){
					kee[j] <- paste(ke[ke_loc[[j]]], sep="",collapse=" OR ")
				}
			}else{
				kee <- paste(ke, sep="", collapse=" OR ")
			}
		}else{
			kee <- ke
		}
		kk <- c()
		for(i in 1:length(kee)){
			query <- paste0("SELECT ", c, " FROM QUALIFIER WHERE ", kee[i],";")
			k <- dbGetQuery(MeSH_dbconn(), query)
			kk <- rbind(kk,k)
		}
		return(unique(kk))
		}
	)	
	
		setMethod("select","MeSHMAPCOUNTS",
		function(x, keys, columns, keytype){
		keytype <- "MAPNAME"
		keys <- unlist(keys)
			if(length(columns) > 1){
				c <- columns[1]
					for(i in 2:(length(columns))){
						c<- paste(c,columns[i], sep=",")
					}
			}else{
			c <- columns
			}
		keys <- paste0('"',keys,'"')
		ke <- paste(keytype, keys, sep="=")
		if(length(ke) > 1){
			kee <- ke[1]
			for(i in 2:(length(ke))){
				kee <- paste(kee, ke[i], sep=" OR ")
			}
		}else{
			kee <- ke
		}
		query <- paste0("SELECT ", c, " FROM MAPCOUNTS WHERE ", kee)
		k <- dbGetQuery(MeSH_dbconn(), query)
		return(unique(k))
		}
	)	

		setMethod("select", "MeSHAOR",
		function(x, keys, columns, keytype){
		keys <- unlist(keys)
		kee <- c()
			if(length(columns) > 1){
				c <- columns[1]
					for(i in 2:(length(columns))){
						c<- paste(c, columns[i], sep=",")
					}
			}else{
			c <- columns
			}
		keys <- paste0('"',keys,'"')
		ke <- paste(keytype, keys, sep="=")
		if(length(ke) > 1){
			if(length(ke) >= 1000){
				ke_loc <- div(1:length(ke),ceiling(length(keys)/500))
				for(j in 1:ceiling(length(keys)/500)){
					kee[j] <- paste(ke[ke_loc[[j]]], sep="",collapse=" OR ")
				}
			}else{
				kee <- paste(ke, sep="", collapse=" OR ")
			}
		}else{
			kee <- ke
		}
		kk <- c()
		for(i in 1:length(kee)){
			query <- paste0("SELECT ", c, " FROM ", x@objectname , " WHERE ", kee[i], ";")
			k <- dbGetQuery(MeSH_dbconn(), query)
			kk <- rbind(kk,k)
		}
		return(unique(kk))
		}
	)	


		setMethod("select","MeSHPCR",
		function(x, keys, columns, keytype){
		keys <- unlist(keys)
		kee <- c()
			if(length(columns) > 1){
				c <- columns[1]
					for(i in 2:(length(columns))){
						c<- paste(c, columns[i], sep=",")
					}
			}else{
			c <- columns
			}
		keys <- paste0('"',keys,'"')
		ke <- paste(keytype, keys, sep="=")
		if(length(ke) > 1){
			if(length(ke) >= 1000){
				ke_loc <- div(1:length(ke),ceiling(length(keys)/500))
				for(j in 1:ceiling(length(keys)/500)){
					kee[j] <- paste(ke[ke_loc[[j]]], sep="",collapse=" OR ")
				}
			}else{
				kee <- paste(ke, sep="", collapse=" OR ")
			}
		}else{
			kee <- ke
		}
		kk <- c()
		for(i in 1:length(kee)){
			query <- paste0("SELECT ", c, " FROM ", x@objectname , " WHERE ", kee[i], ";")
			k <- dbGetQuery(MeSH_dbconn(), query)
			kk <- rbind(kk,k)
		}
		return(unique(kk))
		}
	)	

	# Creation of Objects
	MeSHTERM <- new("MeSHTERM")
	assign("MeSHTERM", MeSHTERM, envir=ns)
	namespaceExport(ns, "MeSHTERM")

	MeSHSYNONYM <- new("MeSHSYNONYM")
	assign("MeSHSYNONYM", MeSHSYNONYM, envir=ns)
	namespaceExport(ns, "MeSHSYNONYM")

	MeSHQUALIFIER <- new("MeSHQUALIFIER")
	assign("MeSHQUALIFIER", MeSHQUALIFIER, envir=ns)
	namespaceExport(ns, "MeSHQUALIFIER")

	MeSHMAPCOUNTS <- new("MeSHMAPCOUNTS")
	assign("MeSHMAPCOUNTS", MeSHMAPCOUNTS, envir=ns)
	namespaceExport(ns, "MeSHMAPCOUNTS")

	meshtype <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")
	for(i in 1:length(meshtype)){
	### Create AOR ###
	objectname1 <- paste0("MeSH", meshtype[i], "AOR")
	# new
	eval(parse(text=paste0(objectname1,' <- new("MeSHAOR");')))
	# objectname
	eval(parse(text=paste0(objectname1, '@objectname <- "', objectname1, '";')))
	# assign
	eval(parse(text=paste0('assign("', objectname1, '", ', objectname1, ', envir=ns);')))
	# namespaceExport
	eval(parse(text=paste0('namespaceExport(ns, "', objectname1, '");')))
	
	### Create PCR ###
	objectname2 <- paste0("MeSH", meshtype[i], "PCR")
	# new
	eval(parse(text=paste0(objectname2,' <- new("MeSHPCR");')))
	# objectname
	eval(parse(text=paste0(objectname2, '@objectname <- "', objectname2, '";')))
	# assign
	eval(parse(text=paste0('assign("', objectname2, '", ', objectname2, ', envir=ns);')))
	# namespaceExport
	eval(parse(text=paste0('namespaceExport(ns, "', objectname2, '");')))
	}
}


.onUnload <- function(libpath)
{
    dbDisconnect(MeSH_dbconn())
}