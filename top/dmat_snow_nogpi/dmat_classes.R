# TODO: Add comment
# 
# Author: ri72fuv2
###############################################################################



# define class distmat
setClass(Class = "dist_mat", representation(cluster = "list", name = "character", 
				nc = "integer", nr = "integer", owndata = "matrix", id = "integer", owncolinds = "integer", 
				mapping = "integer"), prototype(cluster = NULL, name = NULL, nc = NULL, nr = NULL, 
				owndata = NULL, id = NULL, owncolinds = NULL, mapping = NULL))


# define constructor 'mymat'
setGeneric("dist_mat", function(cluster, name, initdata, id, initfun, nc, nr, owndata, 
				mapping) standardGeneric("dist_mat"))

setMethod("dist_mat", c("list", "character", "missing", "integer", "missing", "integer", 
				"integer", "matrix", "integer"), function(cluster, name, initdata, id, initfun, 
				nc, nr, owndata, mapping) {
			
			
			new_dist_mat <- new("dist_mat", cluster = cluster, name = name, nc = nc, nr = nr, 
					owndata = owndata, id = id, owncolinds = (mapping[id] + 1):mapping[id + 1], 
					mapping = mapping)
			return(new_dist_mat)
			
		})

# actual constructor
setMethod("dist_mat", c("list", "character", "matrix", "integer", rep("missing", 
						5)), function(cluster, name, initdata, id, initfun, nc, nr, owndata, mapping) {
			clh <- cluster[[1]]
			nc <- ncol(initdata)
			nr <- nrow(initdata)
			map <- list()
			# master is not part of the cluster object!
			nprocs <- length(clh) + 1
			nci <- floor(nc/nprocs)
			remainder <- nc%%nprocs
			junks <- c(0, rep(nci, nprocs) + c(rep(1, remainder), rep(0, nprocs - remainder)))
			mapping <- integer(nprocs)
			for (i in 1:nprocs) {
				map[[i]] <- (sum(junks[1:i]) + 1):sum(junks[1:(i + 1)])
				mapping[i] <- sum(junks[1:i]) + 1
			}
			mapping <- as.integer(c(mapping - 1, nc))
			
			for (i in 1:nprocs) {
				
				if (i == id) {
					obj <- dist_mat(cluster = cluster, name = name, id = as.integer(i), nc = as.integer(nc), 
							nr = as.integer(nr), owndata = initdata[, map[[i]], drop = F], mapping = mapping)
				} else {
					cli <- clh[i - 1]
					ll <- list()
					ll[[1]] <- dist_mat(cluster = cluster, name = name, id = as.integer(i), 
							nc = as.integer(nc), nr = as.integer(nr), owndata = initdata[, map[[i]]], 
							mapping = mapping)
					
					res <- clusterApply(cl = cli, x = ll, fun = function(val, name) {
								assign(name, val, env = globalenv())
								return(1)
							}, name = name)
				}
			}
			
			return(obj)
			
		})

prepareExtraction <- function(cinds, mappingh) {
	cls <- as.integer(cut(cinds, mappingh))
	tab <- table(cls)
	reps <- rep(mappingh[as.integer(names(tab))], times = as.integer(tab))
	adj <- cinds - reps
	ll <- tapply(adj, cls, FUN = function(x) x)
	llh <- list()
	for (k in 1:length(ll)) llh[[k]] <- ll[[k]]
	ext <- unique(cls)
	
	return(list(ext = ext, llh = llh))
	
	
}

# extraction function
setGeneric("[")
setMethod("[", c(x = "dist_mat"), function(x, i, j, ..., drop = TRUE) {
			
			
			cl <- x@cluster[[1]]
			name <- x@name
			mappingh <- x@mapping
			mappingh[length(mappingh)] <- mappingh[length(mappingh)] + 1
			id <- x@id
			nprocs <- length(cl) + 1
			
			
			if (missing(i) & !missing(j)) {
				
				tempres <- prepareExtraction(cinds = j, mappingh = mappingh)
				ext <- tempres$ext
				ext2 <- ext
				llh <- tempres$llh
				
				if (id %in% ext) {
					masterdat <- x@owndata[, llh[[id]], drop = F]
					llh[[which(ext == id)]] <- NULL
					ext2 <- ext2[-which(ext2 == id)]
				}
				
				ext2[which(ext2 > id)] <- ext2[which(ext2 > id)] - 1
				clh <- cl[ext2]
				res <- clusterApply(cl = clh, x = llh, function(inds, name) {
							get(name, envir = globalenv())@owndata[, inds, drop = F]
						}, name = name)
				
				
			} else if (!missing(i) & missing(j)) {
				llh <- list()
				ext <- 1:nprocs
				for (k in 1:length(cl)) llh[[k]] <- i
				res <- clusterApply(cl = cl, x = llh, fun = function(inds, name) {
							get(name, envir = globalenv())@owndata[inds, , drop = F]
						}, name = name)
				masterdat <- x@owndata[i, , drop = F]
				
				
			} else if (!missing(i) & !missing(j)) {
				
				tempres <- prepareExtraction(cinds = j, mappingh = mappingh)
				ext <- tempres$ext
				ext2 <- ext
				llh <- tempres$llh
				
				
				
				if (id %in% ext) {
					masterdat <- x@owndata[i, llh[[id]], drop = F]
					ext2 <- ext2[-which(ext2 == id)]
					llh[[which(ext == id)]] <- NULL
				}
				
				ext2[which(ext2 > id)] <- ext2[which(ext2 > id)] - 1
				clh <- cl[ext2]
				res <- clusterApply(cl = clh, x = llh, function(cinds, name, rinds) {
							get(name, envir = globalenv())@owndata[rinds, cinds, drop = F]
						}, name = name, rinds = i)
			} else {
				i <- 1:x@nr
				llh <- list()
				ext <- 1:nprocs
				for (k in 1:length(cl)) llh[[k]] <- i
				res <- clusterApply(cl = cl, x = llh, fun = function(inds, name) {
							get(name, envir = globalenv())@owndata[inds, , drop = F]
						}, name = name)
				masterdat <- x@owndata[i, , drop = F]
			}
			
			
			# combine partial matrices
			masterf <- 0
			mat <- c()
			lind <- 1
			for (k in ext) {
				if (k == id) {
					# print(paste('mat:',paste(dim(mat),collapse='-')))
					# print(paste('masterdat',k,paste(dim(masterdat),collapse='-')))
					mat <- cbind(mat, masterdat)
					masterf <- 1
				} else {
					# print(paste('mat:',paste(dim(mat),collapse='-')))
					# print(paste('resp',k,paste(dim(res[[lind-masterf]]),collapse='-')))
					mat <- cbind(mat, res[[lind - masterf]])
				}
				lind <- lind + 1
			}
			
			
			return(mat)
			
		})


prepareAssign <- function(cinds, mappingh, value) {
	cls <- as.integer(cut(cinds, mappingh))
	tab <- table(cls)
	reps <- rep(mappingh[as.integer(names(tab))], times = as.integer(tab))
	adj <- cinds - reps
	ll <- tapply(adj, cls, FUN = function(x) x)
	llh <- list()
	used <- 0
	for (k in 1:length(ll)) {
		llp <- list()
		llp$cinds <- ll[[k]]
		llp$dat <- value[, (used + 1):(used + length(ll[[k]]))]
		llh[[k]] <- llp
		used <- used + length(ll[[k]])
	}
	ext <- unique(cls)
	return(list(llh = llh, ext = ext))
}



setGeneric("[<-")
setMethod("[<-", c(x = "dist_mat", value = "numeric"), function(x, i, j, ..., value) {
			
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
			
			cl <- x@cluster[[1]]
			name <- x@name
			mappingh <- x@mapping
			mappingh[length(mappingh)] <- mappingh[length(mappingh)] + 1
			id <- x@id
			nprocs <- length(cl) + 1
			
			
			
			
			
			if (missing(i) & !missing(j)) {
				nr <- x@nr
				i <- 1:nr
				if (any(dim(value) != c(length(i), length(j)))) 
					stop("Dimensions of inputdata ans submatrix do not match.")
				
				tempres <- prepareAssign(cinds = j, mappingh = mappingh, value = value)
				ext <- tempres$ext
				ext2 <- ext
				llh <- tempres$llh
				
				
				if (id %in% ext) {
					tempind <- which(ext == id)
					x@owndata[i, llh[[tempind]]$cinds] <- llh[[tempind]]$dat
					llh[[tempind]] <- NULL
					ext2 <- ext2[-tempind]
				}
				
				
				ext2[which(ext2 > id)] <- ext2[which(ext2 > id)] - 1
				clh <- cl[ext2]
				res <- clusterApply(cl = clh, x = llh, function(llp, name) {
							eval(parse(text = paste(name, "@owndata[,llp$cinds]<<-llp$dat", sep = "")))
						}, name = name)
			} else if (!missing(i) & missing(j)) {
				
				nc <- x@nc
				j <- 1:nc
				if (any(dim(value) != c(length(i), length(j)))) 
					stop("Dimensions of inputdata ans submatrix do not match.")
				tempres <- prepareAssign(cinds = j, mappingh = mappingh, value = value)
				ext <- tempres$ext
				ext2 <- ext
				llh <- tempres$llh
				
				if (id %in% ext) {
					tempind <- which(ext == id)
					x@owndata[i, llh[[tempind]]$cinds] <- llh[[tempind]]$dat
					llh[[tempind]] <- NULL
					ext2 <- ext2[-tempind]
				}
				
				
				ext2[which(ext2 > id)] <- ext2[which(ext2 > id)] - 1
				clh <- cl[ext2]
				res <- clusterApply(cl = clh, x = llh, function(llp, name, rinds) {
							eval(parse(text = paste(name, "@owndata[rinds,llp$cinds]<<-llp$dat", 
													sep = "")))
						}, name = name, rinds = i)
				
			} else if (!missing(i) & !missing(j)) {
				if (any(dim(value) != c(length(i), length(j)))) 
					stop("Dimensions of inputdata ans submatrix do not match.")
				tempres <- prepareAssign(cinds = j, mappingh = mappingh, value = value)
				ext <- tempres$ext
				ext2 <- ext
				llh <- tempres$llh
				
				if (id %in% ext) {
					tempind <- which(ext == id)
					x@owndata[i, llh[[tempind]]$cinds] <- llh[[tempind]]$dat
					llh[[tempind]] <- NULL
					ext2 <- ext2[-tempind]
				}
				
				
				ext2[which(ext2 > id)] <- ext2[which(ext2 > id)] - 1
				clh <- cl[ext2]
				res <- clusterApply(cl = clh, x = llh, function(llp, name, rinds) {
							eval(parse(text = paste(name, "@owndata[rinds,llp$cinds]<<-llp$dat", 
													sep = "")))
						}, name = name, rinds = i)
				
				
			} else {
				nc <- x@nc
				nr <- x@nr
				j <- 1:nc
				i <- 1:nr
				if (any(dim(value) != c(length(i), length(j)))) 
					stop("dimensions of inputdata ans submatrix do not match")
				tempres <- prepareAssign(cinds = j, mappingh = mappingh, value = value)
				ext <- tempres$ext
				ext2 <- ext
				llh <- tempres$llh
				
				if (id %in% ext) {
					tempind <- which(ext == id)
					x@owndata[i, llh[[tempind]]$cinds] <- llh[[tempind]]$dat
					llh[[tempind]] <- NULL
					ext2 <- ext2[-tempind]
				}
				
				
				ext2[which(ext2 > id)] <- ext2[which(ext2 > id)] - 1
				clh <- cl[ext2]
				res <- clusterApply(cl = clh, x = llh, function(llp, name, rinds) {
							eval(parse(text = paste(name, "@owndata[rinds,llp$cinds]<<-llp$dat", 
													sep = "")))
						}, name = name, rinds = i)
				
				
			}
			
			return(x)
		})
