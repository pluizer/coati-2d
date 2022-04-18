// Query functions use cpVect as value in callback function...

typedef void (*_SpaceNearestPointQueryFunc)(cpShape* shape, cpFloat distance, 
 	cpVect* point, void* data);

typedef struct
{
	_SpaceNearestPointQueryFunc func;
	void* data;
} _SpaceNearestPointTuple;

void _SpaceNearestPointBridge(cpShape* shape, cpFloat distance, cpVect point, void* data)
{
	_SpaceNearestPointTuple* tuple = (_SpaceNearestPointTuple*)data;
	tuple->func(shape, distance, &point, tuple->data);
}

void _SpaceNearestPointQuery(
	cpSpace* space, cpVect point, cpFloat maxDistance,
	cpLayers layers, cpGroup group,
	_SpaceNearestPointQueryFunc func, void* data)
{
	_SpaceNearestPointTuple tuple = { func, data };
	 cpSpaceNearestPointQuery(space, point, maxDistance, layers, group,
					    _SpaceNearestPointBridge, &tuple);
}

typedef void (*_SpaceSegmentQueryFunc)(cpShape* shape, cpFloat t, cpVect* n, void* data);

typedef struct
{
	_SpaceSegmentQueryFunc func;
	void* data;
} _SpaceSegmentTuple;

void _SpaceSegmentBridge(cpShape* shape, cpFloat t, cpVect n, void* data)
{
	_SpaceSegmentTuple* tuple = (_SpaceSegmentTuple*)data;
	tuple->func(shape, t, &n, tuple->data);
}

void _SpaceSegmentQuery(
	cpSpace* space, cpVect start, cpVect end,
	cpLayers layers, cpGroup group,
	_SpaceSegmentQueryFunc func, void* data)
{
	_SpaceSegmentTuple tuple = { func, data };
	cpSpaceSegmentQuery(space, start, end, layers, group,
			    _SpaceSegmentBridge, &tuple);
}
