/**
 * Copyright (c) 2006-2013 LOVE Development Team
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 **/

#ifndef __triangulate_h_
#define __triangulate_h_

#include <list>
#include <vector>
#include <cstdio>

struct Vector
{
	Vector()
		: x(0), y(0) { }
	Vector(float x, float y)
		: x(x), y(y) { }
	float x, y;
};

struct Triangle
{
	Triangle(const Vector& a, const Vector& b, const Vector& c)
		: a(a)
		, b(b)
		, c(c) { }
	Vector a, b, c;
};

extern std::vector<Triangle> triangulate(const std::vector<Vector>& polygon);

extern bool isConvex(Vector* polygon, unsigned size);

#endif //__triangulate_h_
