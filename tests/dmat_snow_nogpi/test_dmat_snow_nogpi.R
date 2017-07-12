# TODO: Add comment
# 
# Author: ri72fuv2
###############################################################################


# on rvs8 (prerequisites: snow and Rmpi, and RprofileDMAT copied to ~/Rprofile,
# +classes.R which is sourced in the Rprofile[adjust path]) mpiexec -n 4 R
# --no-save

# setup snow and input data we need a cluster object embedded in a list at first
# we use a normal matrix as data input


cl <- getMPIcluster()
cluster <- list()
cluster[[1]] <- cl
orgmat <- matrix(1:50, ncol = 10)

# test constructor the constructor needs a cluster object, a name for the
# distributed matrix to be created [this name is used on the other processes], a
# matrix with inputdata and an id the id indicates the process that created the
# matrix, usually this is '1' which means it was created by the master
mydmat <- dist_mat(cluster = cluster, name = "mydmat", initdata = orgmat, id = as.integer(1))

# just for testing, print out the part of the matrix of one of the remote
# processes
cli <- cl[3]
clusterApply(cl = cli, x = "mydmat", fun = function(name) {
			try(get(name, envir = globalenv()), silent = TRUE)
		})


mydmat  #this shows the local part only; one can use mydmat[,] 

### from here on evrything should be nomal R-syntax


### test extraction of submatrices
mydmat[, ]  #will show the whole matrix
mydmat[2:4, 8:10]
mydmat[5, ]
mydmat[, 2:10]
mydmat[4:5, ]
ri <- 2:4
ci <- 2:9
mydmat[ri, ci]




### test assignment of submatrices
ri <- 2:4
ci <- 2:9
mydmat[ri, ci] <- orgmat[ri, ci] + 0.3
mydmat[, ]
mydmat[ri, ci]

# and revert it
mydmat[ri, ci] <- orgmat[ri, ci]
mydmat[, ]
mydmat[ri, ci]


# rows only
mydmat[ri, ] <- orgmat[ri, ] + 0.3
mydmat[, ]
mydmat[ri, ]

# and revert it
mydmat[ri, ] <- orgmat[ri, ]
mydmat[, ]

# cols only
mydmat[, ci] <- orgmat[, ci] + 0.3
mydmat[, ]
mydmat[, ci]

# and revert it
mydmat[, ci] <- orgmat[, ci]
mydmat[, ci]

# total assign
mydmat[, ] <- orgmat[, ] + 0.3
mydmat[, ]


# and revert it
mydmat[, ] <- orgmat[, ]
mydmat[, ]


# single row (as matrix)
ri <- 3
mydmat[ri, ] <- orgmat[ri, , drop = F] + 0.3
mydmat[, ]
mydmat[ri, ]

# and revert it
mydmat[ri, ] <- orgmat[ri, , drop = F]
mydmat[, ]

# single col (as matrix)
ci <- 7
mydmat[, ci] <- orgmat[, ci, drop = F] + 0.3
mydmat[, ]
mydmat[, ci]

# and revert it
mydmat[, ci] <- orgmat[, ci, drop = F]
mydmat[, ]


# single row (as vector)
ri <- 3
mydmat[ri, ] <- orgmat[ri, ] + 0.3
mydmat[, ]
mydmat[ri, ]

# and revert it
mydmat[ri, ] <- orgmat[ri, ]
mydmat[, ]


# single col (as matrix)
ci <- 7
mydmat[, ci] <- orgmat[, ci] + 0.3
mydmat[, ]
mydmat[, ci]

# and revert it
mydmat[, ci] <- orgmat[, ci]
mydmat[, ]


# single element
mydmat[ri, ci] <- orgmat[ri, ci] + 0.3
mydmat[, ]
mydmat[, ci]

# and revert it
mydmat[ri, ci] <- orgmat[ri, ci]
mydmat[, ]

