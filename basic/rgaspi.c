#include <Rdefines.h>
#include <stdio.h>
#include<stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include "GASPI.h"



//////////////////////////////////////////////////////////////////////
#define ASSERT(ec) success_or_exit (__FILE__, __LINE__, ec);


/* helper function 1  (from gaspi hello world example) */
void success_or_exit ( const char* file, const int line, const int ec)
{
  if (ec != GASPI_SUCCESS)
    {
      gaspi_printf ("Assertion failed in %s[%i]:%d\n", file, line, ec);
      exit (EXIT_FAILURE);
    }
}


/* helper function 1  (from gaspi hello world example) */


void wait_if_queue_full ( const gaspi_queue_id_t queue_id

                        , const gaspi_number_t request_size

                        )

{

  gaspi_number_t queue_size_max;

  gaspi_number_t queue_size;



  ASSERT (gaspi_queue_size_max (&queue_size_max));

  ASSERT (gaspi_queue_size (queue_id, &queue_size));



  if (queue_size + request_size >= queue_size_max)

    {

      ASSERT (gaspi_wait (queue_id, GASPI_BLOCK));

    }

}


//////////////////////////////////////////////////////////////





//structure with general gaspi information
struct rgaspi_info{
  gaspi_segment_id_t next_seg_name; //pointer to next segment name
  gaspi_rank_t gaspi_rank;
  gaspi_rank_t gaspi_nranks;
};

//structure for segment specific information
struct rgaspi_seginfo{
  gaspi_segment_id_t seg_name;
  gaspi_size_t seg_size;
  gaspi_pointer_t seg_ptr;
  int seg_type; //currently only double is implemented so it is always set to 1
};

//finalizer for external R-pointers
static void _finalizer_rgaspi_info(SEXP ext)
{
    if (NULL == R_ExternalPtrAddr(ext))
        return;
    //Rprintf("finalizing\n");
    struct rgaspi_info *ptr = (struct rgaspi_info *) R_ExternalPtrAddr(ext);
    Free(ptr);
    R_ClearExternalPtr(ext);
}


static void _finalizer_rgaspi_seginfo(SEXP ext)
{
    if (NULL == R_ExternalPtrAddr(ext))
        return;
    //Rprintf("finalizing\n");
    struct rgaspi_seginfo *ptr = (struct rgaspi_seginfo *) R_ExternalPtrAddr(ext);
    Free(ptr);
    R_ClearExternalPtr(ext);
}


/////////////////////////////////////////////////////////////////////





SEXP copyraw(SEXP raw, SEXP len){
  int len2=INTEGER(len)[0];
int i;
SEXP out =PROTECT(allocVector(RAWSXP,len2));

 for(i=0;i<len2;i++){
   RAW(out)[i] =RAW(raw)[i];
   }

 UNPROTECT(1);
 return out;
}


SEXP rgaspi_init(SEXP ptrh){
  // run gaspi init
  ASSERT (gaspi_proc_init (GASPI_BLOCK));

  //create gaspi_info structure and set its values
  struct rgaspi_info *rgaspi_info_ptr = (struct rgaspi_info *) malloc(sizeof(struct rgaspi_info));
  //gaspi_segment_id_t * nsn = (gaspi_segment_id_t *) malloc(sizeof(gaspi_segment_id_t));
  //*nsn=0;

  ASSERT (gaspi_proc_rank (&rgaspi_info_ptr->gaspi_rank));

  ASSERT (gaspi_proc_num (&rgaspi_info_ptr->gaspi_nranks));

  rgaspi_info_ptr->next_seg_name=0;

  //use hptr provided by R to create external pointer
  SEXP rgaspi_info_extptr = PROTECT(R_MakeExternalPtr(rgaspi_info_ptr, R_NilValue, ptrh));
  R_RegisterCFinalizerEx(rgaspi_info_extptr, _finalizer_rgaspi_info, TRUE);
    UNPROTECT(1);

  return rgaspi_info_extptr;
}


/*
rgaspi_show_info:
creates an R-integer vector containing general gaspi_info and returns it to R
 */
SEXP rgaspi_show_info(SEXP rgaspi_info_extptr){


  struct rgaspi_info * rgaspi_info_ptr= (struct rgaspi_info *) R_ExternalPtrAddr(rgaspi_info_extptr);
  SEXP out =PROTECT(allocVector(INTSXP,3));
  INTEGER(out)[0] = (int) rgaspi_info_ptr->next_seg_name;
  INTEGER(out)[1] = (int) rgaspi_info_ptr->gaspi_rank;
  INTEGER(out)[2] = (int)rgaspi_info_ptr->gaspi_nranks;
 UNPROTECT(1);

  return out;
}

/*
rgaspi_segment_create:
creates a gaspi segment and a corresponding gaspi_seg_info structure and returns a external pointer to this structure to R
 */




/*
rgaspi_segment_info:
uses an external pointer provided by R to create an R-integer-vector conatining segment information
 */

SEXP rgaspi_segment_info(SEXP rgaspi_seginfo_extptr){
  //process external pointer provided by R
  struct rgaspi_seginfo * rgaspi_seginfo_ptr= (struct rgaspi_seginfo *) R_ExternalPtrAddr(rgaspi_seginfo_extptr);

  //read segment info into R-integer vector
  SEXP out =PROTECT(allocVector(INTSXP,3));
  INTEGER(out)[0] = (int)rgaspi_seginfo_ptr->seg_name;
  INTEGER(out)[1] = rgaspi_seginfo_ptr->seg_size;
  INTEGER(out)[2] = rgaspi_seginfo_ptr->seg_type;
  UNPROTECT(1);

  //return R-interger vector to R
  return out;

}


////////////////////////////////////
///???///
////////////////////////////////////


SEXP rgaspi_segment_create_db(SEXP rgaspi_info_extptr, SEXP r_seg_ptrh, SEXP r_seg_size, SEXP r_seg_type){

  //process arguments passed by R
  struct rgaspi_info * rgaspi_info_ptr= (struct rgaspi_info *) R_ExternalPtrAddr(rgaspi_info_extptr);
  gaspi_size_t segment_size =(gaspi_size_t)INTEGER(r_seg_size)[0];

  // create gaspi_seginfo structure
  struct rgaspi_seginfo *rgaspi_seginfo_ptr = (struct rgaspi_seginfo *) malloc(sizeof(struct rgaspi_seginfo));
  gaspi_segment_id_t segment_id_new= rgaspi_info_ptr->next_seg_name;


  //create gaspi_segment
  ASSERT (gaspi_segment_create ( segment_id_new, segment_size*sizeof(double)

                               , GASPI_GROUP_ALL, GASPI_BLOCK

                               , GASPI_ALLOC_DEFAULT

                               )

         );


  //set values in  gaspi_segment structure
  rgaspi_seginfo_ptr->seg_name=rgaspi_info_ptr->next_seg_name;
  rgaspi_seginfo_ptr->seg_size=segment_size;

  //set segment pointer in gaspi_segment structure
  ASSERT (gaspi_segment_ptr (rgaspi_seginfo_ptr->seg_name, &rgaspi_seginfo_ptr->seg_ptr));
  rgaspi_seginfo_ptr->seg_type=(int)INTEGER(r_seg_type)[0];

  //increase next_seg_name
  ++(rgaspi_info_ptr->next_seg_name);

  //now return pointer to the seginfo structure to R
  SEXP rgaspi_seginfo_extptr = PROTECT(R_MakeExternalPtr(rgaspi_seginfo_ptr, R_NilValue, r_seg_ptrh));
  R_RegisterCFinalizerEx(rgaspi_seginfo_extptr, _finalizer_rgaspi_seginfo, TRUE);
  UNPROTECT(1);

  return rgaspi_seginfo_extptr;
}




/*
rgaspi_segment_write:
uses an external pointer provided by R to write specified values to an previously created gaspi_segment
in this small test case we write to the local part only
 */


SEXP rgaspi_segment_write_db(SEXP rgaspi_seginfo_extptr, SEXP values , SEXP offset, SEXP length){

  //get segstruct pointer and process args
 struct rgaspi_seginfo * rgaspi_seginfo_ptr= (struct rgaspi_seginfo *) R_ExternalPtrAddr(rgaspi_seginfo_extptr);
 double *segptrh=(double*)rgaspi_seginfo_ptr->seg_ptr;

 int sizeh=INTEGER(length)[0];
int offh=INTEGER(offset)[0];

 int i;
 segptrh=segptrh+offh;
 for(i=0;i<sizeh;i++){
   *segptrh=REAL(values)[i];
   segptrh++;
   }


 return ScalarLogical(TRUE);

}

/*
rgaspi_segment_read:
uses an external pointer provided by R to read values from a previously created gaspi_segment to an R-raw-vector
in this small test case we read from the local part of the segment only
 */


SEXP rgaspi_segment_read_db(SEXP rgaspi_seginfo_extptr, SEXP offset, SEXP length){
int sizeh=INTEGER(length)[0];
int offh=INTEGER(offset)[0];
  //get segstruct pointer
 struct rgaspi_seginfo * rgaspi_seginfo_ptr= (struct rgaspi_seginfo *) R_ExternalPtrAddr(rgaspi_seginfo_extptr);
 //gaspi_size_t sizeh=rgaspi_seginfo_ptr->seg_size;
 double *segptrh=(double *)rgaspi_seginfo_ptr->seg_ptr;
 SEXP out =PROTECT(allocVector(REALSXP,(int)sizeh));
 int i;
 segptrh=segptrh+offh;
 for(i=0;i<sizeh;i++){
   REAL(out)[i] =*segptrh;
   segptrh++;
   }

 UNPROTECT(1);
 return out;

}


////////////////////////////////////

/*
rgaspi_segment_remote_read
uses an external pointer provided by R to read values from a previously created gaspi_segment to another previously created
gaspi segment
 */


SEXP rgaspi_segment_remote_read_db(SEXP rgaspi_seginfo_extptr1, SEXP rgaspi_seginfo_extptr2, SEXP rgaspi_info_extptr, SEXP length, SEXP rank, SEXP offset_1, SEXP offset_2){
int lenC=INTEGER(length)[0];
 int offh1=INTEGER(offset_1)[0];
 int offh2=INTEGER(offset_2)[0];


 gaspi_rank_t rankC=INTEGER(rank)[0];

  //process external pointers provided by R
 struct rgaspi_seginfo * rgaspi_seginfo_ptr1= (struct rgaspi_seginfo *) R_ExternalPtrAddr(rgaspi_seginfo_extptr1);
 struct rgaspi_seginfo * rgaspi_seginfo_ptr2= (struct rgaspi_seginfo *) R_ExternalPtrAddr(rgaspi_seginfo_extptr2);
 struct rgaspi_info * rgaspi_info_ptr= (struct rgaspi_info *) R_ExternalPtrAddr(rgaspi_info_extptr);

 //retrieve necessary members from the gaspi_seginfo-structure
 gaspi_size_t sizeh=rgaspi_seginfo_ptr1->seg_size;

 //TODO: include checks whether segments sizes are too short for length of read operation

 double *segptrh=(double *)rgaspi_seginfo_ptr1->seg_ptr;

 const gaspi_segment_id_t seg_id1=rgaspi_seginfo_ptr1->seg_name;
 const gaspi_segment_id_t seg_id2=rgaspi_seginfo_ptr2->seg_name;


 //sizes and types are checked in R: size of segments needs to coincide with number of procs
 const gaspi_queue_id_t queue_id = 0; //currently we always use queue 0




  //in our case the offset is always zero
 const gaspi_offset_t offset1 = (gaspi_offset_t) offh1*sizeof (double);
 const gaspi_offset_t offset2 = (gaspi_offset_t) offh2*sizeof (double);


      wait_if_queue_full (queue_id, 1);



      ASSERT (gaspi_read ( seg_id2, offset2

                         , rankC, seg_id1, offset1

                         , lenC*sizeof (double), queue_id, GASPI_BLOCK

                         )

             );




 return ScalarLogical(TRUE);

}

///////////////////////////////
////remote_write
///////////////////////////////


SEXP rgaspi_segment_remote_write_db(SEXP rgaspi_seginfo_extptr1, SEXP rgaspi_seginfo_extptr2, SEXP rgaspi_info_extptr, SEXP length, SEXP rank, SEXP offset_1, SEXP offset_2){
int lenC=INTEGER(length)[0];
 int offh1=INTEGER(offset_1)[0];
 int offh2=INTEGER(offset_2)[0];

 gaspi_rank_t rankC=INTEGER(rank)[0];

  //process external pointers provided by R
 struct rgaspi_seginfo * rgaspi_seginfo_ptr1= (struct rgaspi_seginfo *) R_ExternalPtrAddr(rgaspi_seginfo_extptr1);
 struct rgaspi_seginfo * rgaspi_seginfo_ptr2= (struct rgaspi_seginfo *) R_ExternalPtrAddr(rgaspi_seginfo_extptr2);
 struct rgaspi_info * rgaspi_info_ptr= (struct rgaspi_info *) R_ExternalPtrAddr(rgaspi_info_extptr);

 //retrieve necessary members from the gaspi_seginfo-structure
 gaspi_size_t sizeh=rgaspi_seginfo_ptr1->seg_size;

 //TODO: include checks whether segments sizes are too short for length of write operation

 double *segptrh=(double *)rgaspi_seginfo_ptr1->seg_ptr;

 const gaspi_segment_id_t seg_id1=rgaspi_seginfo_ptr1->seg_name;
 const gaspi_segment_id_t seg_id2=rgaspi_seginfo_ptr2->seg_name;


 //sizes and types are checked in R: size of segments needs to coincide with number of procs
 const gaspi_queue_id_t queue_id = 0; //currently we always use queue 0




  //in our case the offset is always zero
 const gaspi_offset_t offset1 = (gaspi_offset_t) offh1*sizeof (double);
 const gaspi_offset_t offset2 = (gaspi_offset_t) offh2*sizeof (double);


      wait_if_queue_full (queue_id, 1);



      ASSERT (gaspi_write ( seg_id1, offset1

                         , rankC, seg_id2, offset2

                         , lenC*sizeof (double), queue_id, GASPI_BLOCK

                         )

             );




 return ScalarLogical(TRUE);

}



///////////////////////
///???///
////////////////////////


/*
rgaspi_wait:
runs gaspi_wait
 */
SEXP rgaspi_wait(){
  /* Terminate */
  gaspi_queue_id_t queue_id = 0;
  ASSERT (gaspi_wait (queue_id, GASPI_BLOCK));

  return ScalarLogical(TRUE);
}




/*
rgaspi_wait:
runs gaspi_barrier
 */
SEXP rgaspi_barrier(){
  /* Terminate */
  ASSERT (gaspi_barrier (GASPI_GROUP_ALL, GASPI_BLOCK));

  return ScalarLogical(TRUE);
}




/*
rgaspi_wait:
runs gaspi_term
 */
SEXP rgaspi_term(){
  /* Terminate */
  ASSERT( gaspi_proc_term(GASPI_BLOCK) );

  return ScalarLogical(TRUE);
}
