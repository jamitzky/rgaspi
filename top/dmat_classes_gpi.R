# define class distmat
setClass(Class = "dist_mat", representation(rgaspi_seg = "ANY", rgaspi_info_obj = "ANY", 
				name = "character", nc = "integer", nr = "integer", owndata = "matrix", id = "integer", 
				owncolinds = "integer", mapping = "integer", rgaspi_buffer = "ANY"), prototype(rgaspi_seg = NULL, 
				rgaspi_info_obj = NULL, name = NULL, nc = NULL, nr = NULL, id = NULL, mapping = NULL, 
				rgaspi_buffer = NULL))


# define constructor 'distmat'
setGeneric("dist_mat", function(rgaspi_info_obj, name, initdata, initfun) standardGeneric("dist_mat"))

setMethod("dist_mat", c("ANY", "character", "matrix", "missing"), function(rgaspi_info_obj, 
				name, initdata, initfun) {
			# master id is 1
			id <- as.integer(1)
			bufferseg <- rg_segment(length = 1e+07, type = "db", mode = "master")
			
			
			nc <- ncol(initdata)
			nr <- nrow(initdata)
			map <- list()
			
			
			nprocs <- rgaspi_info_obj@nranks
			nci <- floor(nc/nprocs)
			remainder <- nc%%nprocs
			junks <- c(0, rep(nci, nprocs) + c(rep(1, remainder), rep(0, nprocs - remainder)))
			mapping <- integer(nprocs)
			worker_ncs <- c()
			for (i in 1:nprocs) {
				map[[i]] <- (sum(junks[1:i]) + 1):sum(junks[1:(i + 1)])
				mapping[i] <- sum(junks[1:i]) + 1
				worker_ncs <- c(worker_ncs, length(map[[i]]))
			}
			mapping <- as.integer(c(mapping - 1, nc))
			# print(mapping) print(map) determine maximum number of columns for a single
			# worker
			maxcol <- max(worker_ncs)
			myseg <- rg_segment(length = nr * maxcol, type = "db", mode = "master")
			# print(maxcol)
			for (i in 1:nprocs) {
				
				if (i == id) {
					owncolinds <- (mapping[id] + 1):mapping[id + 1]
					owndata <- initdata[, owncolinds]
					values <- c(owndata)
					rg_write(myseg, values, offset = 0)
				} else {
					tcolinds <- (mapping[i] + 1):mapping[i + 1]
					tdata <- initdata[, tcolinds]
					values <- c(tdata)
					rg_write(bufferseg, values, offset = 0)
					rg_remote_write(bufferseg, myseg, i - 1, length(values), 0, 0)
				}
			}
			
			
			new_dist_mat <- new("dist_mat", rgaspi_seg = myseg, rgaspi_info_obj = rgaspi_info_obj, 
					name = name, nc = nc, nr = nr, id = id, mapping = mapping, rgaspi_buffer = bufferseg)
			return(new_dist_mat)
			
		})


prepareExtraction <- function(cinds, mappingh) {
	cls <- as.integer(cut(cinds, mappingh))
	tab <- table(cls)
	reps <- rep(mappingh[as.integer(names(tab))], times = as.integer(tab))
	adj <- cinds - reps
	
	# now we need to find consecutive segments stratified by ranks
	pre <- tapply(adj, cls, FUN = function(x) x)
	# print(pre)
	consegs <- lapply(pre, FUN = split2consegs)
	lindex <- 1
	llh <- list()
	for (k in 1:length(consegs)) {
		llp <- list()
		
		for (kk in 1:length(consegs[[k]])) {
			llp$rank <- as.numeric(names(consegs)[k])
			llp$cinds <- consegs[[k]][[kk]]
			llh[[lindex]] <- llp
			lindex <- lindex + 1
		}
	}
	
	
	
	return(llh)
	
	
}

# extraction function
setGeneric("[")
setMethod("[", c(x = "dist_mat"), function(x, i, j, ..., drop = TRUE) {
			
			
			
			name <- x@name
			mappingh <- x@mapping
			mappingh[length(mappingh)] <- mappingh[length(mappingh)] + 1
			id <- x@id
			nprocs <- x@rgaspi_info_obj@nranks
			tseg <- x@rgaspi_seg
			tbuff <- x@rgaspi_buffer
			nr <- x@nr
			nc <- x@nc
			
			if (missing(i) & !missing(j)) {
				
				tinds <- 1:nr
				llh <- prepareExtraction(cinds = j, mappingh = mappingh)
				# print(llh)
				res <- list()
				
				for (k in 1:length(llh)) {
					tmp <- llh[[k]]
					crank <- tmp$rank
					if (crank == 1) {
						res[[k]] <- rg_read(tseg, length = nr * length(tmp$cinds), offset = nr * 
										(tmp$cinds[1] - 1))
						
					} else {
						
						tlen <- nr * length(tmp$cinds)
						rg_remote_read(tseg, tbuff, crank - 1, tlen, nr * (tmp$cinds - 1), 
								0)
						res[[k]] <- rg_read(tbuff, length = tlen, offset = 0)
					}
				}
				
				
			} else if (!missing(i) & missing(j)) {
				tinds <- i
				llh <- prepareExtraction(cinds = 1:nc, mappingh = mappingh)
				# print(llh)
				res <- list()
				
				for (k in 1:length(llh)) {
					tmp <- llh[[k]]
					crank <- tmp$rank
					if (crank == 1) {
						res[[k]] <- rg_read(tseg, length = nr * length(tmp$cinds), offset = nr * 
										(tmp$cinds[1] - 1))
						
					} else {
						
						tlen <- nr * length(tmp$cinds)
						rg_remote_read(tseg, tbuff, crank - 1, tlen, nr * (tmp$cinds - 1), 
								0)
						res[[k]] <- rg_read(tbuff, length = tlen, offset = 0)
					}
				}
				
				
				
			} else if (!missing(i) & !missing(j)) {
				tinds <- i
				
				llh <- prepareExtraction(cinds = j, mappingh = mappingh)
				# print(llh)
				res <- list()
				
				for (k in 1:length(llh)) {
					tmp <- llh[[k]]
					crank <- tmp$rank
					if (crank == 1) {
						res[[k]] <- rg_read(tseg, length = nr * length(tmp$cinds), offset = nr * 
										(tmp$cinds[1] - 1))
						
					} else {
						
						tlen <- nr * length(tmp$cinds)
						rg_remote_read(tseg, tbuff, crank - 1, tlen, nr * (tmp$cinds - 1), 
								0)
						res[[k]] <- rg_read(tbuff, length = tlen, offset = 0)
					}
				}
				
				
			} else {
				tinds <- 1:nr
				llh <- prepareExtraction(cinds = 1:nc, mappingh = mappingh)
				# print(llh)
				res <- list()
				
				for (k in 1:length(llh)) {
					tmp <- llh[[k]]
					crank <- tmp$rank
					if (crank == 1) {
						res[[k]] <- rg_read(tseg, length = nr * length(tmp$cinds), offset = nr * 
										(tmp$cinds[1] - 1))
						
					} else {
						
						tlen <- nr * length(tmp$cinds)
						rg_remote_read(tseg, tbuff, crank - 1, tlen, nr * (tmp$cinds - 1), 
								0)
						res[[k]] <- rg_read(tbuff, length = tlen, offset = 0)
					}
				}
				
				
			}
			
			# print(res) combine partial matrices
			
			tmat <- c()
			
			for (k in 1:length(res)) {
				tlen <- length(llh[[k]]$cinds)
				partres <- res[[k]]
				for (z in 1:tlen) {
					newcol <- partres[tinds + (z - 1) * nr]
					tmat <- cbind(tmat, newcol)
				}
			}
			return(tmat)
			
		})


prepareAssign <- function(cinds, mappingh, value) {
	cls <- as.integer(cut(cinds, mappingh))
	tab <- table(cls)
	reps <- rep(mappingh[as.integer(names(tab))], times = as.integer(tab))
	adj <- cinds - reps
	
	# now we need to find consecutive segments stratified by ranks
	pre <- tapply(adj, cls, FUN = function(x) x)
	# print(pre)
	consegs <- lapply(pre, FUN = split2consegs)
	lindex <- 1
	llh <- list()
	used <- 0
	for (k in 1:length(consegs)) {
		llp <- list()
		
		for (kk in 1:length(consegs[[k]])) {
			llp$rank <- as.numeric(names(consegs)[k])
			llp$cinds <- consegs[[k]][[kk]]
			llp$dat <- value[, (used + 1):(used + length(consegs[[k]][[kk]])), drop = F]
			llh[[lindex]] <- llp
			used <- used + length(consegs[[k]][[kk]])
			lindex <- lindex + 1
		}
	}
	
	
	return(llh)
}

prepareAssignRow <- function(datlist, inds) {
	res <- list()
	consegs <- split2consegs(inds)
	#print(consegs)
	lindex <- 1
	for (i in 1:length(datlist)) {
		tmp <- datlist[[i]]
		llp <- list()
		llp$rank <- tmp$rank
		usedc <- 0
		for (j in 1:length(tmp$cinds)) {
			usedr <- 0
			llp$cinds <- tmp$cinds[j]
			for (k in 1:length(consegs)) {
				llp$rinds <- consegs[[k]]
				# print('+++') print(usedr) print(usedc) print(tmp$dat)
				llp$dat <- tmp$dat[(usedr + 1):(usedr + length(consegs[[k]])), usedc + 
								1]
				usedr <- usedr + length(consegs[[k]])
				res[[lindex]] <- llp
				lindex <- lindex + 1
			}
			usedc <- usedc + 1
		}
		
	}
	return(res)
}


split2consegs <- function(x) {
	consegs <- list()
	last <- -5
	thisseg <- c()
	sc <- 1
	for (k in 1:length(x)) {
		cur <- x[k]
		if (cur != last + 1) {
			if (k != 1) {
				consegs[[sc]] <- thisseg
				sc <- sc + 1
			}
			thisseg <- cur
		} else {
			thisseg <- c(thisseg, cur)
		}
		last <- cur
	}
	
	# include last conseg
	consegs[[sc]] <- thisseg
	# print(consegs)
	return(consegs)
}


setGeneric("[<-")
setMethod("[<-", c(x = "dist_mat", value = "numeric"), function(x, i, j, ..., value) {
			
			name <- x@name
			mappingh <- x@mapping
			mappingh[length(mappingh)] <- mappingh[length(mappingh)] + 1
			id <- x@id
			nprocs <- x@rgaspi_info_obj@nranks
			tseg <- x@rgaspi_seg
			tbuff <- x@rgaspi_buffer
			nr <- x@nr
			nc <- x@nc
			
			
			# print('vector version')
			if (missing(i) & !missing(j)) 
				i <- 1:nr
			if (missing(j) & !missing(i)) 
				j <- 1:nc
			
			
			if (length(j) == 1) {
				value <- matrix(value, ncol = 1)
			} else if (length(i) == 1) {
				j <- 1:nc
				value <- matrix(value, nrow = 1)
			} else {
				stop("Dimensions of inputdata ans submatrix do not match.")
			}
			
			x[i, j] <- value
			
			return(x)
		})

setGeneric("[<-")
setMethod("[<-", c(x = "dist_mat", value = "matrix"), function(x, i, j, ..., value) {
			
			name <- x@name
			mappingh <- x@mapping
			mappingh[length(mappingh)] <- mappingh[length(mappingh)] + 1
			id <- x@id
			nprocs <- x@rgaspi_info_obj@nranks
			tseg <- x@rgaspi_seg
			tbuff <- x@rgaspi_buffer
			nr <- x@nr
			nc <- x@nc
			
			if (missing(i) & !missing(j)) {
				i <- 1:nr
				
				if (any(dim(value) != c(nr, length(j)))) 
					stop("dimensions of inputdata and submatrix do not match")
				llh <- prepareAssign(cinds = j, mappingh = mappingh, value = value)
				
				# print(llh)
				for (k in 1:length(llh)) {
					tmp <- llh[[k]]
					values <- c(tmp$dat)
					crank <- tmp$rank
					if (crank == 1) {
						
						rg_write(tseg, values, (tmp$cinds[1] - 1) * nr)
						
					} else {
						rg_write(tbuff, values, 0)
						rg_remote_write(tbuff, tseg, crank - 1, as.numeric(length(values)), 
								0, (tmp$cinds[1] - 1) * nr)
					}
				}
				
				
				
			} else if (!missing(i) & missing(j)) {
				j <- 1:nc
				if (any(dim(value) != c(length(i), length(j)))) 
					stop("Dimensions of inputdata ans submatrix do not match.")
				
				llhpre <- prepareAssign(cinds = j, mappingh = mappingh, value = value)
				
				# print(llhpre)
				llh <- prepareAssignRow(llhpre, i)
				# print(llh)
				
				for (k in 1:length(llh)) {
					tmp <- llh[[k]]
					values <- c(tmp$dat)
					crank <- tmp$rank
					currow <- tmp$rinds[1]
					curcol <- tmp$cinds[1]
					if (crank == 1) {
						
						rg_write(tseg, values, (curcol - 1) * nr + currow - 1)
						
					} else {
						rg_write(tbuff, values, 0)
						rg_remote_write(tbuff, tseg, crank - 1, as.numeric(length(values)), 
								0, (curcol - 1) * nr + currow - 1)
					}
				}
				
				
				
			} else if (!missing(i) & !missing(j)) {
				if (any(dim(value) != c(length(i), length(j)))) 
					stop("Dimensions of inputdata and submatrix do not match.")
				
				
				llhpre <- prepareAssign(cinds = j, mappingh = mappingh, value = value)
				
				# print(llhpre)
				llh <- prepareAssignRow(llhpre, i)
				# print(llh)
				
				for (k in 1:length(llh)) {
					tmp <- llh[[k]]
					values <- c(tmp$dat)
					crank <- tmp$rank
					currow <- tmp$rinds[1]
					curcol <- tmp$cinds[1]
					if (crank == 1) {
						
						rg_write(tseg, values, (curcol - 1) * nr + currow - 1)
						
					} else {
						rg_write(tbuff, values, 0)
						rg_remote_write(tbuff, tseg, crank - 1, as.numeric(length(values)), 
								0, (curcol - 1) * nr + currow - 1)
					}
				}
				
				
			} else {
				
				j <- 1:nc
				i <- 1:nr
				if (any(dim(value) != c(nr, nc))) 
					stop("dimensions of inputdata and submatrix do not match")
				llh <- prepareAssign(cinds = j, mappingh = mappingh, value = value)
				
				for (k in 1:length(llh)) {
					tmp <- llh[[k]]
					values <- c(tmp$dat)
					crank <- tmp$rank
					if (crank == 1) {
						
						rg_write(tseg, values, 0)
						
					} else {
						rg_write(tbuff, values, 0)
						rg_remote_write(tbuff, tseg, crank - 1, as.numeric(length(values)), 
								0, 0)
					}
				}
				
				
			}
			
			return(x)
		})




