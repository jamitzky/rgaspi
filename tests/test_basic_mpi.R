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

#dyn.load is already perfomred in the Rprofile


#gaspi init (workers first)
#worker
mpi.remote.exec(eval(expression(rgaspiinfop<-.Call("rgaspi_init",hptr=list("a"))),envir=.GlobalEnv),ret=F)
#mpi.remote.exec(eval(expression(a<-10),envir=.GlobalEnv))
#mpi.remote.exec(eval(expression(a),envir=.GlobalEnv))
rgaspiinfop<-.Call("rgaspi_init",hptr=list("a"))
#rgaspiinfop
#check rgaspiinfo-pointer
#worker
mpi.remote.exec(eval(expression(rgaspiinfop),envir=.GlobalEnv))
#master
rgaspiinfop


#try to retrieve info
#master
.Call("rgaspi_show_info",extptr=rgaspiinfop)
#work
er
mpi.remote.exec(eval(expression(.Call("rgaspi_show_info",extptr=rgaspiinfop)),envir=.GlobalEnv))


#create segment (worker first, collective)
#worker
mpi.remote.exec(eval(expression(seg1ptr<-.Call("rgaspi_segment_create_db",rgaspi_info_ptr=rgaspiinfop,r_seg_ptrh=list("a"),r_seg_size=as.integer(5),r_seg_type=as.integer(1))),envir=.GlobalEnv),ret=F)
#master
seg1ptr<-.Call("rgaspi_segment_create_db",rgaspi_info_ptr=rgaspiinfop,r_seg_ptrh=list("a"),r_seg_size=as.integer(5),r_seg_type=as.integer(1))
seg1ptr

#fill seg with values
values<-c(2.3,2.4,2.5,2.6,2.7)
mpi.remote.exec(eval(expression(values<-c(2.3,2.4,2.5,2.6,2.7)+mpi.comm.rank()),envir=.GlobalEnv))
len<-length(values)
mpi.remote.exec(eval(expression(len<-length(values)),envir=.GlobalEnv))

.Call("rgaspi_segment_write_db",rgaspi_seginfo_ptr=seg1ptr,values=values[2:4],offset=as.integer(1),length=as.integer(3))
mpi.remote.exec(eval(expression(.Call("rgaspi_segment_write_db",rgaspi_seginfo_ptr=seg1ptr,values=values[2:4],offset=as.integer(1),length=as.integer(3))),envir=.GlobalEnv))

#now read
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))
mpi.remote.exec(eval(expression(.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))),envir=.GlobalEnv))

#overwrite values
values<-c(2.3,2.4,2.5,2.6,2.7)+3.1
mpi.remote.exec(eval(expression(values<-c(2.3,2.4,2.5,2.6,2.7)+3.1),envir=.GlobalEnv))

#and read again
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))
mpi.remote.exec(eval(expression(.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))),envir=.GlobalEnv))


#and read again
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))
mpi.remote.exec(eval(expression(.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr,offset=as.integer(0),length=as.integer(5))),envir=.GlobalEnv))

#and again
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))
mpi.remote.exec(eval(expression(.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))),envir=.GlobalEnv))



#now we do a remote read to the second local segment
#first we need to create a 2nd segment
mpi.remote.exec(eval(expression(seg2ptr<-.Call("rgaspi_segment_create_db",rgaspi_info_ptr=rgaspiinfop,r_seg_ptrh=list("a"),r_seg_size=as.integer(5),r_seg_type=as.integer(1))),envir=.GlobalEnv),ret=F)
#master
seg2ptr<-.Call("rgaspi_segment_create_db",rgaspi_info_ptr=rgaspiinfop,r_seg_ptrh=list("a"),r_seg_size=as.integer(5),r_seg_type=as.integer(1))
seg2ptr

#check gaspi_info
.Call("rgaspi_show_info",extptr=rgaspiinfop)

#remote read: we read from remote (rank 2) seg1 to local seg2
.Call("rgaspi_segment_remote_read_db",rgaspi_seginfo_extptr1=seg1ptr,rgaspi_seginfo_extptr2=seg2ptr,rgaspi_info_extptr=rgaspiinfop,length=as.integer(4),rank=as.integer(2),offset_1=as.integer(1),offset_2=as.integer(1))

mpi.remote.exec(eval(expression(.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))),envir=.GlobalEnv))
mpi.remote.exec(eval(expression(.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg2ptr, offset=as.integer(0),length=as.integer(5))),envir=.GlobalEnv))

#now we read from segment 2 and check segment 1
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg2ptr, offset=as.integer(0),length=as.integer(5))
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr,  offset=as.integer(0),length=as.integer(5))


#remote read from rank1: we read from remote (rank 1) seg1 to local seg2
.Call("rgaspi_segment_remote_read_db",rgaspi_seginfo_extptr1=seg1ptr,rgaspi_seginfo_extptr2=seg2ptr,rgaspi_info_extptr=rgaspiinfop,length=as.integer(3),rank=as.integer(1),offset_1=as.integer(1),offset_2=as.integer(0))

#now we read from segment 2 and check segment 1
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg2ptr, offset=as.integer(0),length=as.integer(5))
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg1ptr, offset=as.integer(0),length=as.integer(5))


#now we do remote write from the local seg1 to the remote seg2 (rank1 first, then rank2)
new_val<-c(5.4,5.3,5.2,5.1,5.0)
.Call("rgaspi_segment_write_db",rgaspi_seginfo_ptr=seg1ptr,values=new_val,length=len,offset=as.integer(0))
.Call("rgaspi_segment_remote_write_db",rgaspi_seginfo_extptr1=seg1ptr,rgaspi_seginfo_extptr2=seg2ptr,rgaspi_info_extptr=rgaspiinfop,length=as.integer(3),rank=as.integer(1),offset_1=as.integer(2),offset_2=as.integer(1))
.Call("rgaspi_segment_remote_write_db",rgaspi_seginfo_extptr1=seg1ptr,rgaspi_seginfo_extptr2=seg2ptr,rgaspi_info_extptr=rgaspiinfop,length=as.integer(3),rank=as.integer(2),offset_1=as.integer(1),offset_2=as.integer(2))
#then we check whether everything arrived in place
.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg2ptr, offset=as.integer(0),length=as.integer(5))
mpi.remote.exec(eval(expression(.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=seg2ptr, offset=as.integer(0),length=as.integer(5))),envir=.GlobalEnv))



#gaspi wait (wf)
mpi.remote.exec(eval(expression(.Call("rgaspi_wait")),envir=.GlobalEnv),ret=F)
.Call("rgaspi_wait")

#gaspi-barrier (wf)
mpi.remote.exec(eval(expression(.Call("rgaspi_barrier")),envir=.GlobalEnv),ret=F)
.Call("rgaspi_barrier")


#gaspi term (wf)
mpi.remote.exec(eval(expression(.Call("rgaspi_term")),envir=.GlobalEnv),ret=F)
.Call("rgaspi_term")


#and quit
quit()




