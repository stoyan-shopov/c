/*
int ** x;
int ** y[];
int (** z)[];
int *(* a)[], b, c;
*/

struct s { struct {int x;};};

struct str
{
	struct s *(*(* p)[10][10])[12], z;
	struct { int x, y, z; } **(*x)[10], d;
};
/*
struct str1
{
	struct s *(*(* p)[10][10])[12], z;
	struct { int x, y, z; } **(*x)[10], d;
} ***(**p[5])[10];
*/
