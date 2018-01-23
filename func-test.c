int x, y;
/*
int foo()()()()()();
int foo(a,b,c)(a,b,c)(a,b,c)(a,b,c)(a,b,c)(a,b,c);
// */
int (*fpfi1(struct { int x; } *, struct x { int y; }*[], struct z { int y; }(*xxx)[]))(struct foo *), (*fpfi(int (*)(long), int))(int, ...), (*foo(x))(z), a, b, c, *((****t)[2][3])[4];

int bar(a,b,c);
/*
id" x" id" y" >>int define-variables declaration-end
id" foo" >function{ id" x" } >pointer >function{ id" z" } id" a" id" b" id" c" id" t" >pointer >pointer >pointer >pointer >array{ -2 }array-end >array{ -2 }array-end >array{ -2 }array-end >pointer >>int define-variables declaration-end
id" bar" >function{ id" a" id" b" id" c" } >>int define-variables declaration-end
>>int define-variables declaration-end
 */

int foo1(void bar());

int * (*xfoo())();

int (*fpfi1(struct { int x; } *, struct x { int y; }*[], struct z { int y; }(*xxx)[]))(struct foo *);

/*
x" id" y" >>int define-variables declaration-end
fpfi1" >function-param-type-list{ id" xxx" >pointer >array[] aggregate{ >>int struct-declarator-list{ id" y" }struct-declarator-list-end }aggregate-end id" z" >struct |param-list-boundary| >abstract-declarator-array[] >pointer aggregate{ >>int struct-declarator-list{ id" y" }struct-declarator-list-end }aggregate-end id" x" >struct |param-list-boundary| >pointer aggregate{ >>int struct-declarator-list{ id" x" }struct-declarator-list-end }aggregate-end >struct }function-param-type-list-end >pointer >function-param-type-list{ >pointer id" foo" >struct }function-param-type-list-end id" fpfi" >function-param-type-list{ >>int |param-list-boundary| >pointer >abstract-declarator-function-id-list{ >>long }abstract-declarator-function-id-list-end >>int }function-param-type-list-end >pointer >function-param-type-list{  }function-param-type-list-end id" foo" >function-id-list{ id" x" }function-id-list-end >pointer >function-id-list{ id" z" }function-id-list-end id" a" id" b" id" c" id" t" >pointer >pointer >pointer >pointer >array{ -2 }array-end >array{ -2 }array-end >array{ -2 }array-end >pointer >>int define-variables declaration-end
bar" >function-id-list{ id" a" id" b" id" c" }function-id-list-end >>int define-variables declaration-end
foo1" >function-param-type-list{ id" bar" >function() >>void }function-param-type-list-end >>int define-variables declaration-end
xfoo" >function() >pointer >function() >pointer >>int define-variables declaration-end
*/

int (*fpfi1(struct { int x; } *, void * (*(*[]))(int, int *), struct x { int y; }*[], struct z { int y; }(*xxx)[]))(struct foo *);

/* !!! the next two cases have an interesting parse !!! */
int (*fpfi1(struct { int x; } *, void * (**[])(int, int *), struct x { int y; }*[], struct z { int y; }(*xxx)[]))(struct foo *);
int (*fpfi1(struct { int x; } *, void * (**asd[])(int, int *), struct x { int y; }*[], struct z { int y; }(*xxx)[]))(struct foo *);

