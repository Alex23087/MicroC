# Microc
Alessandro Scala, 600372

05/09/2023

### Extensions added:
 - Pre and postfix, and assignment shorthand operators (++, --, +=, -+, *=, /=, %=)
 - Separate compilation (interfaces and extern, see below)
 - Deadcode detection
 - Strings and floats

## 1 - Scanner and Parser

### Scanner
### Parser
#### Notes
 - Another escape sequence has been added: `'\0'`, to help with string manipulation.
 - Floats are of the format `[0-9]* . [0-9]+ f? | [0-9]+ f | 0x [0-9 a-f A-F] f`
 - Keywords are handled with a hash table, as seen in class. Operators and single character tokens are not
 - Three auxiliary scanners, one for single line comments, one for block comments, one for string literals

 - Empty statement is compiled as an empty block instead of creating a new node in the ast
 - For is translated as while
 - Fundecl and Fundef
 - Assignment shorthands are compiled as a sequence of operations
 - Binops inlined to solve shift/reduce conflicts