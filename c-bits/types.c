#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "types.h"

extern int  print_val(int val);
extern int  print(int val)            asm("print");
extern int  equal(int val1, int val2) asm("equal");
extern int  is_number(int v);
extern int  is_boolean(int v);
extern int  is_tuple(int v);

/******************************************************************************/
/** Convert a raw 'int' into a pointer ****************************************/
/******************************************************************************/

// take an int 'val' ending with '001' and return it as a pointer
int* int_addr(int val){
  return (int *) (val - 1);
}

// assumes 8 byte aligned 'addr' (last 3 bits are 0)
int addr_int(int* addr){
  int v = (int) addr;
  return v + 1; // set the last bit to 1
}

/******************************************************************************/
/** EQUALITY test *************************************************************/
/******************************************************************************/

int equal(int val1, int val2) {
  if(val1 == val2) { return CONST_TRUE; }
  else { return CONST_FALSE; }
}

/******************************************************************************/
/** PRINTING ******************************************************************/
/******************************************************************************/

int is_number(int v){
  return ((v & 1) == 0);
}

int is_boolean(int v){
  return ((v & CONST_FALSE) == CONST_FALSE);
}

int is_tuple(int v){
  return ((v & 7) == 1);
}

void print_number(int val){
  printf("%d", val >> 1);
}

void print_boolean(int val){
  if (val == CONST_TRUE)
    printf("true");
  else // if (val == CONST_FALSE)
    printf("false");
}

int tuple_at(int* base, int i){
  return base[i + 2];
}

int tuple_size(int* base){
  return (*base) >> 1;
}

void print_tuple(int val){
  // printf("TBD");
}

int print_val(int val) {
  if (is_number(val))
    print_number(val);
  else if (is_boolean(val))
    print_boolean(val);
  else if (is_tuple(val))
    print_tuple(val);
  else
    printf("Unknown value: %#010x", val);
  return val;
}

int print(int val){
  print_val(val);
  printf("\n");
  return val;
}
