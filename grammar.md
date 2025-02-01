START -> declaration
declaration -> var | 
var -> "let" ident "=" expr ";" 
expr -> for | while | operation
ident -> \some string\
for -> unimplemented!()
while -> unimplemented!()
operation -> \any mathematical operation??\

program = block
block = ["let" ident "=" expression ";"]+
        | 
