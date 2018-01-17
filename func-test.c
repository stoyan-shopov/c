int x, y;
/*
int foo()()()()()();
int foo(a,b,c)(a,b,c)(a,b,c)(a,b,c)(a,b,c)(a,b,c);
// */
int (*fpfi1(struct { int x; } *, struct x { int y; }*[], struct z { int y; }(*)[]))(struct foo *), (*fpfi(int (*)(long), int))(int, ...), (*foo(x))(z), a, b, c, *((****t)[2][3])[4];

int bar(a,b,c);
/*
id" x" id" y" >>int define-variables declaration-end
id" foo" >function{ id" x" } >pointer >function{ id" z" } id" a" id" b" id" c" id" t" >pointer >pointer >pointer >pointer >array{ -2 }array-end >array{ -2 }array-end >array{ -2 }array-end >pointer >>int define-variables declaration-end
id" bar" >function{ id" a" id" b" id" c" } >>int define-variables declaration-end
>>int define-variables declaration-end
 */

int foo1(void bar());
