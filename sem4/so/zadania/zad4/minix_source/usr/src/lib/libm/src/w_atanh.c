/* @(#)w_atanh.c 5.1 93/09/24 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

#include <sys/cdefs.h>
#if defined(LIBM_SCCS) && !defined(lint)
__RCSID("$NetBSD: w_atanh.c,v 1.9 2002/05/26 22:02:00 wiz Exp $");
#endif

/*
 * wrapper atanh(x)
 */

#include "math.h"
#include "math_private.h"


double
atanh(double x)		/* wrapper atanh */
{
#ifdef _IEEE_LIBM
	return __ieee754_atanh(x);
#else
	double z,y;
	z = __ieee754_atanh(x);
	if(_LIB_VERSION == _IEEE_ || isnan(x)) return z;
	y = fabs(x);
	if(y>=1.0) {
	    if(y>1.0)
	        return __kernel_standard(x,x,30); /* atanh(|x|>1) */
	    else
	        return __kernel_standard(x,x,31); /* atanh(|x|==1) */
	} else
	    return z;
#endif
}
