# 
# Author: ri72fuv2
###############################################################################

# prerequisites: Rmpi, and RprofileDMATGPI copied to ~/Rprofile

###e.g. on lxlogin5 or mpp2-devel
# cp ~/GASPI/testbed/rgaspi/tests/RprofileDMATGPI ~/.Rprofile


#module load R/3.3


#we also need to add the folder containing the gpi shared lib to the lib path
#export GASPIDIR=/home/hpc/pr28fa/ri72fuv2/GASPI/MYGPIMPPD/gpi2
#export LD_LIBRARY_PATH=$GASPIDIR/lib64:$LD_LIBRARY_PATH
#mpiexec -bootstrap=ssh -n 3 R --no-save


#check rmpi
mpi.comm.size()
mpi.remote.exec(2+2)
mpi.remote.exec(2+2,ret=F)

#get number of workers
nw<-mpi.comm.size()-1

#dynload and sourcing of middle layer is performed in the Rprofile
#rgaspi intit
rgaspi("master")
info<-rgaspi_obj


#currently, matrix elements need to be reals, not integers
orgmat <- matrix(1:50, ncol = 10)+0.1

# test constructor the constructor needs a cluster object, a name for the
# distributed matrix to be created [this name is used on the other processes], a
# matrix with inputdata and an id the id indicates the process that created the
# matrix, usually this is '1' which means it was created by the master
mydmat <- dist_mat(rgaspi_info_obj= info, name = "mydmat", initdata = orgmat)

#test the contents of the matrices using the middle layer
mydmat  #this shows the local part only; one can use mydmat[,]
#we need to create a local buffer segment for this
mybuff<-rg_segment(length=1e6,type="db",mode='master')
mylen<-4*5
tseg<-mydmat@rgaspi_seg
#from rank 2
rg_remote_read(tseg,mybuff,2,mylen,0,0)
rg_read(mybuff,length=mylen,offset=0)
#from rank1
rg_remote_read(tseg,mybuff,1,mylen,0,0)
rg_read(mybuff,length=mylen,offset=0)
#and te local one
rg_read(tseg,length=mylen,offset=0)


### from here on everything should be nomal R-syntax

### test extraction of submatrices
mydmat[, ]  #will show the whole matrix

#check case where both (row and column) indices are present
mydmat[2:4, 8:10]
mydmat[2:4,2:9]

#check case where only row indices are present
mydmat[5, ]
mydmat[2:4,]

#check case where only column indices are present
mydmat[, 2:10]
mydmat[,4:5]

#case where both col and row indices are present
ri <- 2:4
ci <- 2:9
mydmat[ri, ci]

#case with split columns only
ci<-c(2,4,6,8)
mydmat[,ci]

#case with split rows only
ri<-c(1,3,5)
mydmat[ri,]

#case with split row and column indices
ci<-c(2,4,6,8)
ri<-c(1,3,5)
mydmat[ri,ci]


#test assignment of full matrix
mydmat[, ]
mydmat[, ] <- orgmat[, ] + 0.5
mydmat[, ]

#and revert it
mydmat[, ] <- orgmat[, ]
mydmat[, ]

#test assignment with columns only
ci<-2:9
mydmat[,ci]<-orgmat[,ci]+0.5
mydmat[,]

# cols only
ci <- c(2:5,7:9)
mydmat[, ci] <- orgmat[, ci] + 0.3
mydmat[, ]

#and revert it
mydmat[, ] <- orgmat[, ]
mydmat[, ]

# split cols only
ci <- c(2,4,6,8)
mydmat[, ci] <- orgmat[, ci] + 0.3
mydmat[, ]

#and revert it
mydmat[, ] <- orgmat[, ]
mydmat[, ]


#test assignment with rows only
ri<-c(1:2,4:5)
mydmat[ri,]<-orgmat[ri,]+0.5
mydmat[,]

#and revert it
mydmat[, ] <- orgmat[, ]
mydmat[, ]


#test assignment with both j and i
ci <- c(2:5,7:9)
ri<-c(1:2,4:5)
mydmat[ri,ci]<-orgmat[ri,ci]+0.5
mydmat[,]

#and revert it
mydmat[, ] <- orgmat[, ]
mydmat[, ]

#test the vector versions
#col
ci<-4
class(as.numeric(orgmat[,ci]+0.5))
mydmat[,ci]<-as.numeric(orgmat[,ci]+0.5)
mydmat[,]

#and revert it
mydmat[, ] <- orgmat[, ]
mydmat[, ]

#row
ri<-3
class(as.numeric(orgmat[ri,]+0.5))
mydmat[ri,]<-as.numeric(orgmat[ri,]+0.5)
mydmat[,]

#and revert it
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
mydmat[, ] <- orgmat[,]
mydmat[, ]


# single element
ri <- 3
ci <- 7
mydmat[ri, ci] <- orgmat[ri, ci] + 0.3
mydmat[, ]


# and revert it
mydmat[, ] <- orgmat[, ]
mydmat[, ]

