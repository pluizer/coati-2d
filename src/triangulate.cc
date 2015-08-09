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

#include "triangulate.h"

// check if an angle is oriented counter clockwise
inline bool isOrientedCcw(const Vector& a, const Vector& b, const Vector& c)
{
	// return det(b-a, c-a) >= 0
	return ((b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)) >= 0;
}

// check if a and b are on the same side of the line c->d
bool onSameSide(const Vector& a, const Vector& b, const Vector& c, const Vector& d)
{
	float px = d.x - c.x, py = d.y - c.y;
	// return det(p, a-c) * det(p, b-c) >= 0
	float l = px * (a.y - c.y) - py * (a.x - c.x);
	float m = px * (b.y - c.y) - py * (b.x - c.x);
	return l * m >= 0;
}

// checks is p is contained in the triangle abc
inline bool pointInTriangle(const Vector& p, const Vector& a, const Vector& b, const Vector& c)
{
	return onSameSide(p,a, b,c) && onSameSide(p,b, a,c) && onSameSide(p,c, a,b);
}

// checks if any Vector in `vertices' is in the triangle abc.
bool isAnyPointInTriangle(const std::list<const Vector *> &vertices, const Vector& a, const Vector& b, const Vector& c)
{
	std::list<const Vector *>::const_iterator it, end = vertices.end();
	for (it = vertices.begin(); it != end; ++it)
	{
		const Vector *p = *it;
		if ((p != &a) && (p != &b) && (p != &c) && pointInTriangle(*p, a,b,c)) // oh god...
			return true;
	}

	return false;
}

inline bool isEar(const Vector& a, const Vector& b, const Vector& c, const std::list<const Vector*>& vertices)
{
	return isOrientedCcw(a,b,c) && !isAnyPointInTriangle(vertices, a,b,c);
}

std::vector<Triangle> triangulate(const std::vector<Vector>& polygon)
{
	if (polygon.size() < 3)
		throw "Not a polygon.";
	else if (polygon.size() == 3)
		return std::vector<Triangle>(1, Triangle(polygon[0], polygon[1], polygon[2]));

	// collect list of connections and record leftmost item to check if the polygon
	// has the expected winding
	std::vector<size_t> next_idx(polygon.size()), prev_idx(polygon.size());
	size_t idx_lm = 0;
	for (size_t i = 0; i < polygon.size(); ++i)
	{
		const Vector &lm = polygon[idx_lm], &p = polygon[i];
		if (p.x < lm.x || (p.x == lm.x && p.y < lm.y))
			idx_lm = i;
		next_idx[i] = i+1;
		prev_idx[i] = i-1;
	}
	next_idx[next_idx.size()-1] = 0;
	prev_idx[0] = prev_idx.size()-1;

	// check if the polygon has the expected winding and reverse polygon if needed
	if (!isOrientedCcw(polygon[prev_idx[idx_lm]], polygon[idx_lm], polygon[next_idx[idx_lm]]))
		next_idx.swap(prev_idx);

	// collect list of concave polygons
	std::list<const Vector *> concave_vertices;
	for (size_t i = 0; i < polygon.size(); ++i)
	{
		if (!isOrientedCcw(polygon[prev_idx[i]], polygon[i], polygon[next_idx[i]]))
			concave_vertices.push_back(&polygon[i]);
	}

	// triangulation according to kong
	std::vector<Triangle> triangles;
	size_t n_vertices = polygon.size();
	size_t current = 1, skipped = 0, next, prev;
	while (n_vertices > 3)
	{
		next = next_idx[current];
		prev = prev_idx[current];
		const Vector &a = polygon[prev], &b = polygon[current], &c = polygon[next];
		if (isEar(a,b,c, concave_vertices))
		{
			triangles.push_back(Triangle(a,b,c));
			next_idx[prev] = next;
			prev_idx[next] = prev;
			concave_vertices.remove(&b);
			--n_vertices;
			skipped = 0;
		}
		else if (++skipped > n_vertices)
		{
			throw "Cannot triangulate polygon.";
		}
		current = next;
	}
	next = next_idx[current];
	prev = prev_idx[current];
	triangles.push_back(Triangle(polygon[prev], polygon[current], polygon[next]));

	return triangles;
}

bool isConvex(Vector* polygon, unsigned size)
{
	if (size < 3)
		return false;

	// a polygon is convex if all corners turn in the same direction
	// turning direction can be determined using the cross-product of
	// the forward difference vectors
	size_t i = size - 2, j = size - 1, k = 0;
	Vector p(polygon[j].x - polygon[i].x, polygon[j].y - polygon[i].y);
	Vector q(polygon[k].x - polygon[j].x, polygon[k].y - polygon[j].y);
	float winding = p.x*q.y - p.y*q.x;

	while (k+1 < size)
	{
		i = j; j = k; k++;
		p.x = polygon[j].x - polygon[i].x;
		p.y = polygon[j].y - polygon[i].y;
		q.x = polygon[k].x - polygon[j].x;
		q.y = polygon[k].y - polygon[j].y;

		if ((p.x*q.x - p.y*q.x) * winding < 0)
			return false;
	}
	return true;
}
