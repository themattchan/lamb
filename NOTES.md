
## surface syntax

```
<module>
  ::= module <name> where <imports> <sc-decl>*

<imports>
  ::=
  import <mod-name> \(as <mod-name>\)? \(add <names>\)?

<sc-decl>
  ::= let <name> <args>* (: <monotype>)? = <expr0>
   | type <name> <args> = <named-sum> \(| <named-sum>\)+

<expr0> ::= <expr> | <expr> : <type>

-- TODO how to add statements???

<expr>
  ::= <cprim>
   | let <name> <args>* (: <monotype>)? = <expr0> ; <expr0>
   | <expr0> <expr0>
   | fun <arg>* (: <monotype>)? => <expr0>
--   | <stmts> <expr0>
   | (<expr0>)

<named-sum> = <Cap-name> <monotype>*

<monotype>
  ::= <prim-type-name>
   | <var-name>
   | <type> <type>
   | <type> => <type>

<polytype>
  ::= forall <var-name>. <type>
  | <monotype>

<type>
  ::= <monotype>
   | <polytype>

-- Names

<var-name> ::= [a-z]([a-zA-Z0-9_]*)
<Cap-name> ::= [A-Z]([a-zA-Z0-9_]*)
<mod-name> ::= <Cap-name>\(\.<Cap-name\)*

<name> ::= <var-name> | <cap-name>

<args> ::= <var-name> | (<var-name> : <monotype>)
```

--------------------------------------------------------------------------------

## desugaring

let-bound functions desugar like haskell

```
let <name> <args>* = <expr> ;

====>

let <name> = fun <args>* => expr ;
```

sequence of lets are collapsed by parser.

sc-decls are sorted to bring all types in scope first. this will just be a
partition, the declaration order is retained (not that it matters).

all supercombinators are in scope to each other (i.e. sc-decls will be compiled to a c header).

--------------------------------------------------------------------------------
## implementation details

`lambc` is a whole-program compiler. All `lamb` compilation units must be input at
once. C code can be separately compiled and linked in with the C output of `lambc`.

### data representation for user-defined types.

All C primitive types are primitive types in the language.

They compile to stack allocated variables.

These include: `(u?)int{8,16,32,64}_t, float, long, double` and arrays of these.
?? TODO how to handle arrays??

heap allocated things will be tagged structs.

```c
struct lamb_hdr {
  uint8 flags;   -- need an extra-wide flag
  uint8 module;  -- unique module id
  uint8 type;
  uint8 ctor;
};
```

A header is a 32-bit machine word (half-word for 64-bit machines).

Because all types are known statically, there is no need for storing sizes or
tagging or nonsense like that. If the size needs to be known later for whatever
reason, the compiler will generate a `sizeofDT` lookup table that gives the size
of a `lamb` dt given the `ijk`-value (see below).

```c
// TYPE_DATA_OFFSET(i,j) is statically known to compiler

size_t *LAMB_DATATYPE_SIZES = {... statically known sizes ... };

int some_code(char* x)
{
    ...
    // lookup size
    lamb_hdr *h = (lamb_hdr*)x;
    size_t siz = LAMB_DATATYPE_SIZES[MODULE_DATA_OFFSET(h->module,h->type)+h->ctor];
    ...
}
```

After the header will come the data.

Something more clever can be done with the header bit packing but this simple
thing should work for now.

These will be as unpacked as possible. all mallocs will be `char*`s. things must
be aligned on 8-bit word boundary.

### compiling data types, part 1

```
-- Internal repr of names
data Name
  = DT !Word8 !Word8 !Word8   -- unpack
  | ...
```

The namespace for data type constructors and other binders will be separated
after parsing.

suppose you have the following compilation units with Dt defns

```
M1 = type T1 = T1C1 ... T1CN ; type T2 = T2C1... T2CN
M1 = type T1 = T1C1 ... T1CN ; ...
...
MK = ...
```

let there be:

1 <= i <= 255 modules, each with
0 <= j <= 255 types, each type with
0 <= k <= 255 constructors.

The renamer will rename the actual data type names with their (i,j,k) values.

How numbers j and k are picked is irrelevant as long as they are unique.
However, k should be numbered in order of definition.

### modules and imports

haskell style module system.

the filenames are irrelevant.
the module name defined in the module will be the name other modules reference it by.

qualification:

- the full set of binders defined in a module is always exported.
- imports are never reexported.
- the full set of binders is always imported qualified.
- when two or more modules introduce the same binding, there is no unqualified
  import of that binding and a warning is issued (if the module does not define
  that function itself.
- when a module shadows an imported binding, the defn in the module is preferred.

Module numbering is by the topological sort order.

### closures and heap allocated crap

closures:

```c
struct lamb_clos {
  void * fun_ptr;
  ... lots of data;
};
```

I think because we have a whole program compiler, the code generated for
function calls can just do the right thing wrt how it unpacks the closure.

arrays will be special cased so we can match on them.

Modulo escaping, we know when to insert mallocs and frees (when stuff goes out
of scope). But if we don't have escaping, everything can be stack allocated.
Even if we have escaping, and everything is consumed by `main`, things can be
stack allocated!


```ml
module A where

let f = fun x => fun y => x + y;

let f2 = f 2;

let main _ = do
  print (f2 3);
```

should compile to....

```
lamb_clos * f

```

In the following case with multiple modules, we can just compile everything to a
header file and include it, and get gcc to inline. However, assuming that we
don't want to do that... when do you free the closures? In general you CANNOT do dumb ref
counting because of self references... and this will mess things up when
recursive lets are introduced.

```ml
module A where

let f = fun x => fun y => x + y;

let f2 = f 2;

module B where
module A

let main _ = do
  print (f2 3);
```

### compiling data types, part 2

pattern matching. (see augustsson paper for ideas)

the following program

```
-- module 0

data T1 = C1 T2 | C2 T1 T1 | C3 Int

data T2 = D1 String | D2 Int Int

let f (x : T1) : Int = case x of
  C1 t2 -> g t2
  C2 t1 t2 -> f t1 + f t2
  C3 i -> i

let g (y : T2) : Int = case y of
  D1 _ -> 0
  D2 i j -> i + j
```

should compile to:

```c
#define MATCH_TYPE(x, i, j) (x->module == i && x->type==j)
#define MATCH_CTOR(x, k) (x->ctor == k)


// this exhaustive, as guaranteed by type checking!!
int f(char* x)
{
  // this is not necessary because typechecking
  if (MATCH_TYPE((lamb_hdr*)x,0,0) {
    if (MATCH_CTOR((lamb_hdr*)x, 0)) {
      return g(x+1);
    } else if (MATCH_CTOR((lamb_hdr*)x, 1)) {
      return f(x+1) + f(x+2);
    } else if (MATCH_CTOR((lamb_hdr*)x, 2)) {
      return f( *((int*)(x+1)) );
    }
    pattern_match_fail(x, 0, 0); // takes i and j
  }
  runtime_exception("type error");
}

int g(char* x)
{
  // this is not necessary because typechecking
  if (MATCH_TYPE((lamb_hdr*)x,0,1) {
    if (MATCH_CTOR((lamb_hdr*)x, 0)) {
      return 0;
    } else if (MATCH_CTOR((lamb_hdr*)x, 1)) {
      return *((int*)(x+1)) + *((int*)(x+2))
    }
    pattern_match_fail(x, 0, 1); // takes i and j
  }
  runtime_exception("type error");
}

```


--------------------------------------------------------------------------------



example program

let map xs =

let main =
