/* $NetBSD: Lint_fpsetsticky.c,v 1.2 2000/06/14 06:49:05 cgd Exp $ */

/*
 * This file placed in the public domain.
 * Chris Demetriou, November 5, 1997.
 */

#include <ieeefp.h>

/*ARGSUSED*/
fp_except
fpsetsticky(s)
	fp_except s;
{
	fp_except rv = { 0 };

	return (rv);
}
