/*
struct ta;
struct tag s;
struct { int x; };
struct { int x; } s;
struct tag { int x; };
struct tag { int x; } s;
// */
/*
struct tag_innermost { int x; };
struct tag_ext { struct tag_inner { struct tag_innermost; }; } ********** s[][10][20][30];


int (*p)[10];
int *p[10];

struct tag_innermost { struct x ****** y; };
*/

/*struct str { struct { int x; } s1; struct sss {struct {int y;};}s2;};*/

/*struct str { struct { int x; }; struct {struct {int y;};};};*/
struct { struct { int x; }; struct {struct {int y;};};};

struct { int x; } y;

void foo();


struct str
{
	struct
	{
		int x;
	} s1;
	struct sss
	{
		struct
	       	{
			int y;
		};
	}s2;
};

struct { int; };

struct str
{
	struct
	{
		int r, (*(***x)[10])[20], * a, b[10], *c[10], z;
	};
	struct
	{
		struct
	       	{
			int y;
		};
	};
};

