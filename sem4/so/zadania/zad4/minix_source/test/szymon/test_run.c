#include <unistd.h>
#include <stdio.h>

int fib(int n)
{
	if(n<2) return n;
	return fib(n-1) + fib(n-2);
}
//ma sie tylko policzyc
int main(int argc, char** argv)
{
        printf("%d\n",fib(45));
}
