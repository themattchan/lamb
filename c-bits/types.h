#include <stddef.h>

#define ZEROBIT       0x00000001
#define CONST_TRUE    0xFFFFFFFF
#define CONST_FALSE   0x7FFFFFFF

int  tuple_at(int* base, int i);
int  print_val(int val);
int  print(int val);
int  equal(int val1, int val2);
int  is_number(int v);
int  is_boolean(int v);
int  is_tuple(int v);
int* int_addr(int);
int  addr_int(int*);
