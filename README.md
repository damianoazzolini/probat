# PROBAT
Property based testing Prolog programs (currently only SWI Prolog is supported).

Installation (SWI Prolog only, by now)
```
pack_install('https://github.com/damianoazzolini/probat.git').
```

## What is it for?
Property based testing: given a property that should always hold, the library generates a number of random input values to check whether the specified property holds.

The library exposes the predicates `property_test/0` and `property_test/1`.
The former can be used to run the tests with the default parameters which are:
- `trials`: 100, (Number of test)
- `depth`: 8, (Max shrink depth)
- `max_len_list`: 32, (Max list length)
- `verbosity`: 1, (Verbosity)
- `min_val`: -2147483648, (Min val to generate)
- `max_val`: 2147483648, (Max val to generate)

If you want to change the default parameters, the predicate `property_test/1` accepts a list where each atom has as name the argument to set (among the specified in the above list) and as argument its value.
For instance, with `property_test([trials(10)])` the number of trials is set to 10.

Futhermore, the library tries to perform shrinking, i.e., when an example that violates the property is found, the library tries to found a smaller one, which is possibly more helpful for the programmer.

### How to Define a Test?
Add to your source code a set of facts `property/1` where the argument is the predicate that describe the property that must be checked and as its argument one of the following types (`[x]` means implemented, `[ ]` not yet implemented):
- [x] `int`
- [x] `pos_int` (int >= 0)
- [x] `pos_int_no_zero` (int > 0)
- [x] `neg_int` (int =< 0)
- [x] `neg_int_no_zero` (int < 0)
- [x] `float`
- [x] `pos_float` (float >= 0)
- [x] `neg_float` (float =< 0)
- [x] `number` (int or float)
- [x] all the above with two arguments, representing the lower and the upper bound (for example, `int(2,3)`)
- [x] `var`: variable
- [x] `any`: anything
- [x] `list`: list of arbitrary length of arbitrary types
- [x] `list(N)`: list of length `N` of arbitrary types
- [x] `list(any,[type])` -> list of arbitrary length of type type
- [x] `list(N,[type0,type1,...])`: list of length `N` of only types `type0`, `type1`, ...
- [x] `list([type0,type1,...])`: list of length of the input list where the first element is of `type0`, the second of `type1`, and so on
- [X] `atom`
- [X] `atom(L,U)`: atom of length between `L` and `U`.
- [X] `string`
- [X] `string(L,U)`: string of length between `L` and `U`.

Some examples of specifiers for lists:
- `list(2)`: list of length 2 of arbitrary types
- `list(any,[int])`: list of arbitrary length of int
- `list(3,[int,float])`: list of length 3 of only int and float
- `list([int,float])`: list of length 2 where the first element is an int and the second a float
- `list([list,float])`: list of length 2 where the first element is an arbitrary list and the second a float

## Example
Suppose you have this program in a file called `a.pl`.
```Prolog
:- use_module(library(probat)).

% this is the predicate defining the property you want to check
always_hold(A,B):-
    A > 0,
    B > 0.

% with this fact you say that you want to test the property described by always_hold(A,B) when
% A and B are both arbitrary integers.
property(always_hold(int,int)).
```
Load it into SWI 
```
$ swipl a.pl 
```
and test the property with `property_test/0` (or `property_test/1` if you don't like the default values):
```
?- property_test.
Found 1 test.
Executing test: always_hold(int,int) ..... Run 100 attempts, 70 failures (70.0 %) ..... FAILED
Smallest failing always_hold(-1,-1)
Greatest failing always_hold(0,0)
--- Summary ---
Executed 1 test in 0.00545954704284668 seconds
Failed 1 over 1 (100 %)
.
true.
```

Further examples can be found into the `examples` folder.

## Caveat
Currently, if your predicate use assert and does not retract values upon failures, these are will be kept in the database, so possible subsequent calls that depend on these values may be not fully valid. 

## Related Tools
This library is based on the [quickcheck](https://hackage.haskell.org/package/QuickCheck) haskell' library.

Another Prolog library that performs property based testing is:
- [quickcheck](https://github.com/nicoabie/quickcheck)