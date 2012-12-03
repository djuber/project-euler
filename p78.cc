
#include <iostream>

int* p = new int [100000];

int part(int n)
{
	int sum = 0;
	int k = 4;
	int a = 2;
	int b = 1;
	int sgn = 1;

	while (n >= a)
	{
		sum += sgn*(p[n-a] + p[n-b]);
		sum = sum;
		a += k + 1;
		b += k;
		sgn *= -1;
		k+=3;
	}

	if (n >= b)
	{
		sum += sgn*p[n-b];
	}
	return sum % 1000000;
}

int main()
{
	p[0] = 1;
	p[1] = 1;
	int n = 1;
	while (!(p[n] == 0))
	{
		n++;
		p[n] = part(n);
	}
	std::cout << n << "\n";
	return 1;
}
