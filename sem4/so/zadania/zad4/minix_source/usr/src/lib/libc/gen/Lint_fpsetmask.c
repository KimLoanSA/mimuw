/* $NetBSD: Lint_fpsetmask.c,v 1.2 2000/06/14 06:49:05 cgd Exp $ */

/*
 * This file placed in the public domain.
 * Chris Demetriou, November 5, 1997.
 */

#include <ieeefp.h>

/*ARGSUSED*/
fp_except
fpsetmask(m)
	fp_except m;
{
	fp_except rv = { 0 };

	return (rv);
}
