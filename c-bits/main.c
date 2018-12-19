#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/queue.h>
#include "types.h"
#include "gc.h"

#define USE_GC  1    // switch this to 1 to use your gc(...)

extern int  our_code_starts_here()    asm("our_code_starts_here");
extern void error() 		      asm("error");
extern int  print_val(int val);
extern int  print(int val) 	      asm("print");
extern int  equal(int val1, int val2) asm("equal");
extern int* try_gc(int* alloc_ptr, int amount_needed, int* first_frame, int* stack_top) asm("try_gc");

extern int* HEAP         asm("HEAP");
extern int* HEAP_END     asm("HEAP_END");
extern int* STACK_BOTTOM asm("STACK_BOTTOM");

size_t HEAP_SIZE;
int* HEAP_PTR;
int* HEAP;
int* HEAP_END;
int* STACK_BOTTOM;

/******************************************************************************/
/** ERROR handling ************************************************************/
/******************************************************************************/

void error(int code, int v){
   if (code == 0) {
     fprintf(stderr, "Error: expected a number but got %#010x\n", v);
   }
   else if (code == 1) {
     fprintf(stderr, "Error: expected a boolean but got %#010x\n", v);
   }
   else if (code == 2) {
     fprintf(stderr, "Error: arithmetic overflow.");
   }
   else if (code == 3){
     fprintf(stderr, "Error: expected a tuple but got %#010x\n", v);
   }
   else if (code == 4){
     fprintf(stderr, "Error: tuple index too small.");
   }
   else if (code == 5){
     fprintf(stderr, "Error: tuple index too large.");
   }
   exit(1);
 }



/******************************************************************************/
/** GARBAGE COLLECTION ********************************************************/
/******************************************************************************/

/* Try to clean up space in memory by calling gc.

  You do not need to edit this function.

  Arguments:

    - alloc_ptr: The current value of ESI (where the next value would be
      allocated without GC)
    - bytes_needed: The number of bytes that the runtime is trying to allocate
    - first_frame: The current value of EBP (for tracking stack information)
    - stack_top: The current value of ESP (for tracking stack information)

  Returns:

    The new value for ESI, for the runtime to start using as the allocation
    point.  Must be set to a location that provides enough room to fit
    bytes_allocated more bytes in the given heap space

*/
int* try_gc(  int* alloc_ptr
            , int bytes_needed
            , int* first_frame
            , int* stack_top )
{

  int* new_esi;

  if(HEAP == alloc_ptr) {
    fprintf(stderr, "Allocation of %d words too large for %d-word heap\n", bytes_needed / 4, (int)(HEAP_END - HEAP));
    if (DEBUG) fprintf(stderr, "FREE at %p \n", HEAP);
    free(HEAP_PTR);
    exit(10);
  }
  if (USE_GC) {
    // This uses your GC; toggle USE_GC when confident!
    new_esi = gc(STACK_BOTTOM, stack_top, first_frame, HEAP, HEAP_END);
  } else {
    // This skips GC; just keeps ESI where it is.
    new_esi = alloc_ptr;
  }

  if((new_esi + (bytes_needed / 4)) > HEAP_END) {
    fprintf(stderr, "\nOut of memory: needed %d words, but only %d remain after collection!\n\n", bytes_needed / 4, (HEAP_END - new_esi));
    if (DEBUG) fprintf(stderr, "FREE at %p \n", HEAP);
    free(HEAP_PTR);
    exit(9);
  }
  else {
    return new_esi;
  }
}

/******************************************************************************/
/** Top-level "main" **********************************************************/
/******************************************************************************/

int main(int argc, char** argv) {
  if(argc > 1) {
    HEAP_SIZE = atoi(argv[1]);
  }
  else {
    HEAP_SIZE = 100000;
  }
  HEAP = calloc(HEAP_SIZE, sizeof (int));
  if (DEBUG) fprintf(stderr, "ALLOC at %p \n", HEAP);
  HEAP_END = HEAP + HEAP_SIZE;

  // store original to properly 'free'
  HEAP_PTR = HEAP;

  int result = our_code_starts_here(HEAP);

  print(result);
  return 0;
}
