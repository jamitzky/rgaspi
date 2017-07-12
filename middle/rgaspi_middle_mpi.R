#define class rgaspi
setClass(Class="rgaspi",representation(rank="integer",nranks="integer",extp="ANY"),prototype(rank=NULL, nranks=NULL,extp=NULL))


#define constructor rgaspi_init
setGeneric("rgaspi", function(mode) standardGeneric("rgaspi"))

setMethod('rgaspi',c('character'),function(mode){
			
			if(mode=='master'){
				mpi.remote.exec(eval(expression(rgaspi_obj<-rgaspi('worker')),envir=.GlobalEnv),ret=F)
				eval(expression(rgaspi_obj<-rgaspi('worker')),envir=.GlobalEnv)
				return(TRUE)
			} else{
				rgaspiinfop<-.Call("rgaspi_init",hptr=list("a"))
				info<-.Call("rgaspi_show_info",extptr=rgaspiinfop)
				rank<-info[2]
				nranks<-info[3]
				
				new_rgaspi<-new('rgaspi',rank=rank, nranks=nranks,extp=rgaspiinfop)
				return(new_rgaspi)
			}
		})

#define class rgaspi_segment
setClass(Class="rg_segment",representation(id="integer",length="integer",type="character",extp = "ANY" ),prototype(id=NULL,length=NULL,type=NULL,extp=NULL))


#define constructor 'rgaspi_segment'
setGeneric("rg_segment", function(length,type,mode) standardGeneric("rg_segment"))

setMethod('rg_segment',c('numeric','character','character'),
		function(length,type='db',mode='master'){
			len<-as.integer(length)
			id<-.Call("rgaspi_show_info",extptr=rgaspi_obj@extp)[1]
			
			
			if(mode=='master'){
				
				ctxt<-paste("mpi.remote.exec(eval(expression(seg",id,"obj<-rg_segment(length=",len,",type='db',mode='worker')),envir=.GlobalEnv),ret=F)",sep='')
				eval(parse(text=ctxt))
				#print(ctxt)
			}
			
			segp<-.Call("rgaspi_segment_create_db",rgaspi_info_ptr=rgaspi_obj@extp,r_seg_ptrh=list("a"),r_seg_size=len,r_seg_type=as.integer(1))
			
			new_seg<-new('rg_segment',id=id,length=len,type=type,extp=segp)
			return(new_seg)
			
		})



rg_term<-function(){
	mpi.remote.exec(eval(expression(.Call("rgaspi_term")),envir=.GlobalEnv),ret=F)
	.Call("rgaspi_term")
	
}

rg_barrier<-function(){
	mpi.remote.exec(eval(expression(.Call("rgaspi_barrier")),envir=.GlobalEnv),ret=F)
	.Call("rgaspi_barrier")
}


rg_wait<-function(){
	
	mpi.remote.exec(eval(expression(.Call("rgaspi_wait")),envir=.GlobalEnv),ret=F)
	.Call("rgaspi_wait")
}


#local write

setGeneric("rg_write", function(segment,values,offset) standardGeneric("rg_write"))
setMethod('rg_write',c('rg_segment','numeric', 'numeric'),function(segment,values,offset){
			len<-as.integer(length(values))
			
			
			
			res<-.Call("rgaspi_segment_write_db",rgaspi_seginfo_ptr=segment@extp,values=values,offset=as.integer(offset),length=len)
			return(res) 
		})

setGeneric("rg_remote_write", function(segment_from,segment_to,rank_to,length, offset_from, offset_to) standardGeneric("rg_remote_write"))
setMethod('rg_remote_write',c('rg_segment','rg_segment','numeric','numeric', 'numeric','numeric'),function(segment_from,segment_to,rank_to,length, offset_from, offset_to){
			len<-as.integer(length)
			
			res<-.Call("rgaspi_segment_remote_write_db",rgaspi_seginfo_extptr1=segment_from@extp,rgaspi_seginfo_extptr2=segment_to@extp,
					rgaspi_info_extptr=rgaspi_obj@extp,length=len,rank=as.integer(rank_to), offset_1=as.integer(offset_from), offset_2=as.integer(offset_to))
			
			return(res) 
		})

#rg_read
setGeneric("rg_read", function(segment, offset, length) standardGeneric("rg_read"))
setMethod('rg_read',c('rg_segment', 'numeric', 'numeric'),function(segment,offset,length){
			res<-.Call("rgaspi_segment_read_db",rgaspi_seginfo_ptr=segment@extp,offset=as.integer(offset),length=as.integer(length))
			return(res)
			
		})

#rg_remote_read
setGeneric("rg_remote_read", function(segment_to,segment_from,rank_from,length,offset_to,offset_from) standardGeneric("rg_remote_read"))
setMethod('rg_remote_read',c('rg_segment','rg_segment','numeric','numeric', 'numeric', 'numeric'),function(segment_to,segment_from,rank_from,length,offset_to,offset_from){
			len<-as.integer(length)
			
			res<-.Call("rgaspi_segment_remote_read_db",rgaspi_seginfo_extptr1=segment_to@extp,rgaspi_seginfo_extptr2=segment_from@extp,
						rgaspi_info_extptr=rgaspi_obj@extp,len=len,rank=as.integer(rank_from),offset_1=as.integer(offset_to),offset_2=as.integer(offset_from))
			
			return(res) 
		})



rg_info<-function(){
	res<-.Call("rgaspi_show_info",extptr=rgaspi_obj@extp)
	return(res)
}
