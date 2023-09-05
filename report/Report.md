# Microc

### Extensions added:
 - Pre and postfix, and assignment shorthand operators (++, --, +=, -+, *=, /=, %=)
 - Separate compilation (interfaces and extern, see below)
 - Deadcode detection
 - Strings and floats

## 1 - Scanner and Parser

### Scanner
The scanner has been structured in 4 different tokenizers:
 1. `next_token`: the main tokenizer that supplies tokens to the parser;
 2. `single_line_comment`: ignores a single line comment
 3. `block_comment`: ignores a multi-line comment
 4. `string_literal`: used to parse a string literal, by inserting characters met into a buffer (`string_buffer`) which then is used by `next_token` to initialize the `STRING` token.

A hash table has been utilized for keywords (as seen in class), but not for single character tokens, like operators.
The following modifications have been made:
 - Another escape sequence has been added: `'\0'`, to help with string (null-terminated) manipulation.
 - Floats have been added, of the format\
 `([0-9]* \. [0-9]+ f?) | ([0-9]+ f) | (0x [0-9 a-f A-F] f)`.

### Parser
Every production of the grammar has been assigned its own production in the menhir parser, the translation is almost 1:1 with the EBNF grammar.\
A new type `vardesc` has been used to help translate the variable descriptors to the correct types (arrays and pointers).

AST translations have been preferred to modifying the structure of the AST itself. This means for example that the `for` construct has been implemented by translating it into an equivalent `While` node, instead of creating a representation in the AST.
This also applies to shorthand assignment operators, as for example with `x += y`, which is rewritten as `x = x + y`.\
Empty statements are compiled as an empty block (which is a statement), instead of adding an empty statement node.

<br/>
<br/>
<br/>

### AST Modifications
Despite rewriting most constructs with existing AST nodes, the following modifications have been made:
 - Added `TypF` representing `float`s;
 - Added `SLiteral` to encapsulate string literals;
 - Added `Nullptr` to represent the `NULL` literal;
 - Added `Prepost` to represent pre/post increments/decrements;
 - Separated `fun_dec` from `fun_def`, to allow declaring functions without defining them (see later in [Separate Compilation](#sepcomp))



## 2 - Semantic Analysis

### Symbol Table
A block of the symbol table is implemented with a mutable `Hashtbl`, while the symbol table itself has been implemented using a mutable `Stack`. Because of this, the signature of the function to create a new table had to be changed: 
```diff
- val empty_table : 'a symbol_table`
+ val empty_table : unit -> 'a symbol_table
```
This is because of the [value restriction](http://ocamlverse.net/content/weak_type_variables.html) and because using a mutable data structure means we need multiple instances to handle multiple tables.

Other modifications made to the interface are:
 - Added `lookup_opt` to handle lookups with `option`s instead of `exception`s. The original function was left so that the interface is backwards-compatible;
 - Added `append_block`, which allows to push an already existing block on the stack. This was convenient together with `of_alist` to implement the creation of local blocks for functions starting from the formal parameters;
 - Added `print_entries` to recursively print symbols from the top of the stack (innermost block) all the way down to the bottom (outermost/global block) for debug purposes;
 - Added `lookup_local_block` to perform lookup only on the local (top) block. This is useful to check for name clashing, as variables are allowed to shadow variables from outer scopes, but not from the local one.


### Semantic Analysis
The following checks are in place:
 - Arrays cannot be of size < 1. Moreover, arrays that are declared as variables, either local or global, need to have a size defined. Arrays declared as parameters for functions need not satisfy this rule;
 - Multidimensional arrays are not allowed;
 - Functions cannot return non-scalar values. This is checked during the semantic analysis step, despite already being enforced by the grammar implemented by the parser;
 - Variables cannot be of type `void`;
 - Variables and functions cannot be declared twice in the same block;
 - Deadcode detection: see more in [Deadcode Detection](#deadcode);
 - Assignments can only happen with compatible types. The compatibility relation is a relaxation of the type equality relation, where arrays with a known size can be assigned to arrays with undeclared size;
 - Arrays can be assigned with an assignment expression, but only if they are `char` arrays (i.e. strings);
 - Type checking on functions ensures that return statements are present when needed and that they are of the appropriate types. Implemented [Rule CERTC MSC37-C with EX1](https://wiki.sei.cmu.edu/confluence/pages/viewpage.action?pageId=87152283);
 - Function calls are checked so that the types of actual parameters and formal parameters match;
 - Integers are checked so that literals fit into 32 bits;
 - Floats are not checked. See later [Floats](#floats);
 - String literals are regarded as null-terminated `char` arrays;
 - The `NULL` pointer is regarded for the purposes of semantic analysis as a `void` pointer. This comes from the [C Standard](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf) (Section 6.3.2.3/3), where it is defined as "An integer constant expression with the value `0`, or such an expression cast to type `void *`";
 - Check the return type of the `main` function. The check for the presence of `main` check has been implemented, but later disabled because of [Separate Compilation](#sepcomp);
 - Names in the global scope (only global, not in local scopes) are **hoisted**, to allow a more convenient formatting of the code without needing function declarations. This is accomplished by first adding all symbols (variables and functions) to the global scope, and only then performing the static analysis on the function bodies;
 - A new step has been implemented to check for multiple assignments of the same variables in the same statement. In regular C this is allowed, but in _some cases_ (depending on sequence points) leads to undefined behaviour. Because UB is ugly, this over-approximating step has been added to avoid these cases (while also rendering some legal and well-defined C statements illegal in microc, but these statements can be rewritten into sequences of microc-legal statements). Double assignment is still possible in the case of array elements and pointers, as those are not easy to check;

## 3 - Codegen
[LLVM performance tips](https://llvm.org/docs/Frontend/PerformanceTips.html) have been taken as a reference, but preferring simplicity of implementation to performance, where the tradeoff was not worth investing the effort to optimize further.
A key design decision is to use `store`s and `load`s whenever a variable is accessed, instead of keeping track of values in registers and using `phi` instructions in merge blocks. This simplifies immensely the implementation, and is _partly_ optimized by the `mem2reg` LLVM optimization step (although [mem2reg "only attempt[s] to eliminate alloca instructions that are in the entry basic block"](https://llvm.org/docs/Frontend/PerformanceTips.html#use-of-allocas), which means depending on the code it might only help marginally with this issue).

Another key decision is to convert arrays into pointers to array data for the purposes of function calls. Because functions allow parameters to be arrays of undefined size, this was a good solution to implement those, and it's part of a possible implementation of the C-like pointer/array duality, although this hasn't been implemented fully. This decision means that (like in C) arrays are passed by reference to functions, and can be modified. This is really useful in implementing functions that need to return array data, which is otherwise forbidden by the language.

Global variables are initialised to the default value of their respective type (using `Llvm.const_null`), while local ones are not. This is mainly due to the LLVM IR instruction set. Local variables could have been initialised to default values as well, but because a 'declare and initialize' statement has not been implemented, this would have caused many double initializations (which might be optimized out by an analysis checking value usage, but it has been decided to leave less work to optimizers in this case).

To perform arithmetic operations, the `nsw` variants have been used, as they more closely represent C semantics, and can have a good impact on performance. This also applies to `inbounds gep`s.

Importantly, it was not possible for some reason to define a target data layout and triplet, as an error occurred with LLVM stating there were no target architectures available.

## 4 - Extensions

### Pre-post
Pre/post increments/decrements have been added, together with shorthand assignment operators.

Increments and decrements have been implemented as a new type of expression node in the AST, and compilation is rather straightforward:
 1. Access the variable;
 2. Increment or decrement;
 3. Return either the old value or the new one.

These have been implemented both for `int`s and for `float`s.

Assignment shorthands, on the other hand, have been implemented via a translation in the AST, passing from the expression `a += b` to `a = a + b`. This requires two accesses to the variable being assigned, but again is easier to implement, and can be optimized out.\
Because this translation is carried out before the semantic analysis step, overloading `+` for use with ints and floats automatically overloads `+=`.

### Separate Compilation <a name="sepcomp"></a>

Separate compilation has been implemented:

An `extern` keyword has been defined, to allow to declare, but not define, external names (implemented with `Llvm.declare_global` or `Llvm.declare_function`).

`.mc` source files can `#include` (or `#import`) `.mci` **interface files**. These do not work like C includes, but more like OCaml `.mli` interfaces.

Functions can be declared without being defined, but only in interface files.

The `extern` keyword can only appear in `.mc` source files, and not in `.mci` interface files.

Interface files can only contain global variable declarations and function declarations. When an interface file is included, all the declarations inside it are copied into the global scope, marking all of them as `extern` symbols.

Files included will only be checked once: if an included file is included again, it will simply be ignored. This means that cyclic inclusions are not possible.

The usual workflow to compile a program with this mechanism is:
 1. Write a library in a `.mc` source file;
 2. Write a `.mci` interface file for that library;
 3. Write a `.mc` program including the interface file;
 4. Compile each `.mc` file separately into bitcode using `microcc.exe`
 5. Link them together (e.g. with `llvm-link`)
 6. Compile the bitcode to an executable format

This workflow technically allows for interop with other languages with LLVM frontends, and the runtime support implementing print functions (and other ones like typecasts) is an example, being written in C and compiled to LLVM bytecode by clang.

### Deadcode Detection <a name="deadcode"></a>

A deadcode detection pass has been added to the semantic analysis of the programs. Initially, deadcode was accepted during this step, but was then rejected by LLVM, because it entailed generating code after a block terminator. This was initially solved at the codegen step, by not inserting statements if the block contained a terminator (such as a return).

Later, a proper detection was added at the semantic analysis phase, rejecting programs that have unreachable code. This step is an under-approximation that is not very involved, and does not perform checks on the guard to consider whether a `while` will be executed or not. This means that, although a check is performed _inside_ the body of the `while`, the result will not be considered outside of its body. I.e.:
```C
while(true){
    return;
}
// Valid: We do not check if the body is executed, so we cannot assume it returns
int i;
```
```C
while(false){
    return;
    // Invalid: We are sure the following code will not be executed
    int i;
}
```

Same goes for `if`s, where we consider an `if` statement to return only if its two branches both return.

### Strings and Floats <a name="floats"></a>

#### Strings
Strings have been implemented as array of characters.\
A node `SLiteral` has been added to the AST to represent literals.\
The presence of strings means that arrays (at least `char` ones) need to be assignable, or the utility of string literals would be severely compromised.\
To generate IR code for string literals `Llvm.const_stringz` has been used. This however created a problem with how arrays in function parameters have been implemented, as a string constant cannot be bitcasted to a pointer. To solve this, if a function is called with a string literal as parameter, this literal is first allocated on the stack.\
Strings, being `char` arrays, are mutable, like in C, unlike in Java.\
The only support added in the language to strings is the presence of the new `'\0'` escape character (as strings are null-terminated). String manipulation functions are expected to be possible to code on top of the primitive language support, examples are the `strzero` and `strcat` functions implemented in microc in `test/extensions/stringfloat/strcat.mc`.

#### Floats
`float`s have been implemented by adding a new type to the AST, and a new `FLiteral` to represent `float` literals.\
Unlike `int`s, checks on `float` values are left to the functions used to generate them (`Float.of_string` in the lexer and `Llvm.const_float` in the codegen phase). An attempt to explicitly check them has been made, but OCaml using 64 bit floats does not help, and the only references found on the issue proposed [this approach](https://stackoverflow.com/questions/45362323/ieee-64-and-32-bit-float-validation-in-ocaml), which unfortunately created more issues than it solved, rejecting programs that should have been valid. Attempts have been made at checking validity allowing a margin of error, but said margin should have been too big to allow valid programs, so as said, it has been left to runtime functions to raise exceptions in case of illegal values.

## 5 -  Tests

All tests have been run and compared (when available) to the expected output. All tests supposed to succeed do so, and those expected to fail do so as well, with the following exceptions:
 - `test-func2.mc` now fails because multiple assignments in the same statement are not allowed anymore;
 - `test-ops2.mc` now fails because `--42` is parsed as `--(42)`, and not `-(-(42))`, and `42` is not an lvalue, so it cannot be decremented;
 - `test-return1.mc` now fails because it contains dead code;
 - `fail-nomain.mc` now succeeds because to allow separate compilation the main check has been relaxed.

 Furthermore, a few more tests have been added to check the new features implemented. They are contained in `test/extensions`, categorised by the extension they were made for, although many tests include features implemented in other extensions. Scripts to compile them and run them are provided in each folder.