#include <stdio.h>
#include <stdlib.h>
#include "gc.h"

#define HEAPPRINT 30

#define OFF       1
#define VInit     0x00000000
#define VMark     0x00000001
#define VFwd(A)   (A | VMark)

////////////////////////////////////////////////////////////////////////////////
// Imported from types.c
////////////////////////////////////////////////////////////////////////////////

extern int  is_number(int);
extern int  is_tuple(int);
extern int  tuple_at(int* base, int i);
extern int  tuple_size(int* base);
extern int* int_addr(int);
extern int  addr_int(int*);

////////////////////////////////////////////////////////////////////////////////

typedef struct Frame_ {
  int *sp;
  int *bp;
} Frame;

typedef enum Tag_
  { VAddr
  , VStackAddr
  , VNumber
  , VBoolean
  } Tag;

union Data
  { int* addr;
    int  value;
    int  gcvalue;
  };

typedef struct Value_
  { Tag        tag;
    union Data data;
  } Value;

////////////////////////////////////////////////////////////////////////////////
// Low-level API
////////////////////////////////////////////////////////////////////////////////

int valueInt(Value v){
  if (v.tag == VAddr || v.tag == VStackAddr) {
    return addr_int(v.data.addr);
  } else if (v.tag == VNumber){
    return (v.data.value << 1);
  } else { // v.tag == VBoolean
    return v.data.value;
  }
}

Value intValue(int v){
  Value res;
  if (is_tuple(v)) {
    res.tag       = VAddr;
    res.data.addr = int_addr(v);
  } else if (is_number(v)) {
    res.tag   = VNumber;
    res.data.value = v >> 1;
  } else {  // is_boolean(v)
    res.tag   = VBoolean;
    res.data.value = v;
  }
  return res;
}

Value getElem(int *addr, int i){
  int vi = tuple_at(addr, i);
  return intValue(vi);
}

void  setElem(int *addr, int i, Value v){
  addr[i+2] = valueInt(v);
}

void  setStack(int *addr, Value v){
  *addr = valueInt(v);
}

Value getStack(int* addr){
  return intValue(*addr);
}

int* extStackAddr(Value v){
  if (v.tag == VStackAddr)
    return v.data.addr;
  printf("GC-PANIC: extStackAddr");
  exit(1);
}

int* extHeapAddr(Value v){
  if (v.tag == VAddr)
    return v.data.addr;
  printf("GC-PANIC: extHeapAddr");
  exit(1);
}

void setSize(int *addr, int n){
  addr[0] = (n << 1);
}

int isLive(int *addr){
  return (addr[1] == VInit ? 0 : 1);
}

void  setGCWord(int* addr, int gv){
  if (DEBUG) fprintf(stderr, "\nsetGCWord: addr = %p, gv = %d\n", addr, gv);
  addr[1] = gv;
}

int*  forwardAddr(int* addr){
  return int_addr(addr[1]);
}

Value vHeapAddr(int* addr){
  return intValue(addr_int(addr));
}

int round_to_even(int n){
  return (n % 2 == 0) ? n : n + 1;
}

int blockSize(int *addr){
  int n = tuple_size(addr);
  return (round_to_even(n+2));

}
////////////////////////////////////////////////////////////////////////////////

Frame caller(int* stack_bottom, Frame frame){
  Frame callerFrame;
  int *bptr = frame.bp;
  if (bptr == stack_bottom){
    return frame;
  } else {
    callerFrame.sp = bptr + 1;
    callerFrame.bp = (int *) *bptr;
    return callerFrame;
  }
}

void print_stack(int* stack_top, int* first_frame, int* stack_bottom){
  Frame frame = {stack_top, first_frame };
  if (DEBUG) fprintf(stderr, "***** STACK: START sp=%p, bp=%p,bottom=%p *****\n", stack_top, first_frame, stack_bottom);
  do {
    if (DEBUG) fprintf(stderr, "***** FRAME: START *****\n");
    for (int *p = frame.sp; p < frame.bp; p++){
      if (DEBUG) fprintf(stderr, "  %p: %p\n", p, (int*)*p);
    }
    if (DEBUG) fprintf(stderr, "***** FRAME: END *****\n");
    frame    = caller(stack_bottom, frame);
  } while (frame.sp != stack_bottom);
  if (DEBUG) fprintf(stderr, "***** STACK: END *****\n");
}

void print_heap(int* heap, int size) {
  fprintf(stderr, "\n");
  for(int i = 0; i < size; i += 1) {
    fprintf(stderr
          , "  %d/%p: %p (%d)\n"
          , i
          , (heap + i)
          , (int*)(heap[i])
          , *(heap + i));
  }
}


////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
int* mark( int* stack_top
         , int* first_frame
         , int* stack_bottom
         , int* heap_start)
{
  return heap_start;
}

////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
int* forward( int* heap_start
            , int* max_address)
{
  return heap_start;
}

////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void redirect( int* stack_bottom
             , int* stack_top
             , int* first_frame
             , int* heap_start
             , int* max_address )
{
  return; 
}

////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void compact( int* heap_start
            , int* max_address
            , int* heap_end )
{ 
  return;
}

////////////////////////////////////////////////////////////////////////////////
// Top-level GC function (you can leave this as is!) ///////////////////////////
////////////////////////////////////////////////////////////////////////////////

int* gc( int* stack_bottom
       , int* stack_top
       , int* first_frame
       , int* heap_start
       , int* heap_end )
{

  int* max_address = mark( stack_top
                         , first_frame
                         , stack_bottom
                         , heap_start );

  int* new_address = forward( heap_start
                            , max_address );

                     redirect( stack_bottom
                             , stack_top
                             , first_frame
                             , heap_start
                             , max_address );

                     compact( heap_start
                            , max_address
                            , heap_end );

  return new_address;
}
