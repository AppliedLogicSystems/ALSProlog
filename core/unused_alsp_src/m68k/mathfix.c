/* Dummy functions for missing math routines. - ceh */

double yn(int n, double x);
double yn(int n, double x)
{
	return 0.0;
}

double jn(int n, double x);
double jn(int n, double x)
{
	return 0.0;
}

double y1(double x);
double y1(double x)
{
	return 0.0;
}

double y0(double x);
double y0(double x)
{
	return 0.0;
}

double j1(double x);
double j1(double x)
{
	return 0.0;
}

double j0(double x);
double j0(double x)
{
	return 0.0;
}

#ifndef __MWERKS__
double gamma(double x);
double gamma(double x)
{
	return 0.0;
}

double erfc(double x);
double erfc(double x)
{
	return 0.0;
}

double erf(double x);
double erf(double x)
{
	return 0.0;
}
#endif

#ifdef THINK_C
double hypot(double x);
double hypot(double x)
{
	return 0.0;
}
#endif