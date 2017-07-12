# TODO: Add comment
# 
# Author: ri72fuv2
###############################################################################

###e.g. on lxlogin5 or mpp2-devel
# cp ~/GASPI/testbed/rgaspi/tests/RprofileGASPIMPI ~/.Rprofile


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
rgaspi("master")

mpi.remote.exec(eval(expression(rgaspi_obj),envir=.GlobalEnv))
myseg0<-rg_segment(length=10,type="db",mode='master')

mpi.remote.exec(eval(expression(seg0obj),envir=.GlobalEnv))
myseg0

myseg1<-rg_segment(length=10,type="db",mode='master')
mpi.remote.exec(eval(expression(seg1obj),envir=.GlobalEnv))
myseg1

values<-c(1:10)+0.1
rg_write(myseg0,values,offset=0)
rg_read(myseg0,length=3, offset=2)
rg_read(myseg0,length=10,offset=0)


rg_remote_write(myseg0,myseg1,2,5,0,3)
mpi.remote.exec(eval(expression(rg_read(seg1obj,length=10,offset=0)),envir=.GlobalEnv))



rg_read(myseg1,length=10,offset=0)
rg_remote_read(myseg1,myseg1,2,10,0,0)
rg_read(myseg1,length=10,offset=0)

#gaspi wait (wf)
rg_wait()

#gaspi-barrier (wf)
rg_barrier()


#gaspi term (wf)
rg_term()


#and quit
quit()





