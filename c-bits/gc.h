#include <stddef.h>

#define DEBUG     0

/*******************************************************************************
  Mark, sweep, and compact memory, starting from the root set on the stack.

  Arguments:

    - stack_bottom: A pointer to the bottom of the stack (highest address).  In
      our runtime, this is set by getting the address of a variable from main
    - stack_top: A pointer to the top of the stack (lowest address).  During
      execution, this will be the current value of ESP, so it points to the
      most recent frame's variables
    - first_frame: A pointer to the topmost (lowest address) base pointer, for
      starting stack traversal
    - heap_start, heap_end: Pointers to the start and end of the heap;
      typically the global values HEAP, HEAP_END but provided
      as a parameter for testing

  Returns:

    The address immediately following the compacted data, to use as the new
    allocation index stored in ESI.

*******************************************************************************/
int* gc( int* stack_bottom
       , int* stack_top
       , int* first_frame
       , int* heap_start
       , int* heap_end );


/*******************************************************************************

  mark()

  Traverse the heap, starting from the data on the stack, and mark all
  reachable data as live, by setting the second word, aka the GCWord to
  0x00000001

  Arguments:

    - stack_top, first_frame, stack_bottom: as in gc()
    - heap_start: as in compact()

  Returns:

    The *largest* starting address (i.e. the largest tuple-address)
    that was marked.

*******************************************************************************/
int* mark( int* stack_bottom
         , int* stack_top
         , int* first_frame
         , int* heap_start );

/*******************************************************************************

  forward()

  Set up forwarding pointers to marked data on the heap:
  i.e. set the second word (i.e. the GC-WORD) of each
  live value on the heap to its new, compacted address
  (with a 1 at the end, to track liveness)

  Arguments:

    - heap_start, max_address: as in compact()

  Returns:

   The 'Addr' from which *new* blocks can be allocated post-compaction.

*******************************************************************************/
int* forward(int* heap_start, int* max_address);

/*******************************************************************************

  redirect()

  Set each address value on the stack and on the heap to point at the
  new corresponding forwarded value, as computed in the forward() phase.

  Arguments:

    - stack_top, first_frame, stack_bottom: As in gc()
    - heap_start, max_address: as in compact()

  Returns:

    nothing

*******************************************************************************/
void redirect( int* stack_bottom
             , int* stack_top
             , int* first_frame
             , int* heap_start
             , int* max_address );

/*******************************************************************************

  compact()

  Compact memory, assuming that the data has been marked and set up for
  forwarding.  Copy the memory for each value onto the heap into its new
  location.

  Arguments:

    - heap_start: A pointer to the start of the heap; typically the global
      value HEAP, but provided as a parameter for testing
    - heap_end: A pointer to the end of the heap; typically the global
      value HEAP_END, but provided as a parameter for testing
    - max_address: A pointer to the _start_ of the latest value marked (e.g.
      the return value of the mark call), for knowing how far in the heap to
      traverse

  Returns:

    Nothing
*******************************************************************************/
void compact( int* heap_start
            , int* max_address
            , int* heap_end );

/*******************************************************************************

   print_heap ()

   Prints out the first 'size' bytes of the memory starting at 'heap'

*******************************************************************************/
void print_heap( int* heap
               , int size);
