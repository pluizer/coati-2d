(declare (unit chipmunk)
         (uses primitives))

(import scheme
	(chicken foreign)
	srfi-1
	srfi-4)

(import-for-syntax (chicken format))

#>
#include "deps/chipmunk/include/chipmunk.h"
#include "vect_hacks.c"
<#

(include "src/binding-helpers.scm")

;-------------------------------------------------------
; %Helper
;-------------------------------------------------------

; Overwrite this to automatically convert units
(define cp-scale 1)




(define-foreign-type c-shape (c-pointer "cpShape"))
(define-foreign-type c-body (c-pointer "cpBody"))
(define-foreign-type c-body-shape-iter-func (function void (c-body c-shape c-pointer)))
(define-foreign-type c-body-constraint-iter-func (function void (c-body c-constraint c-pointer)))
(define-foreign-type c-body-arbiter-iter-func (function void (c-body c-arbiter c-pointer)))
(define-foreign-type c-collision-type unsigned-integer)
(define-foreign-type c-group unsigned-integer)
(define-foreign-type c-layers unsigned-integer)
(define-foreign-type c-circle-shape (c-pointer "cpCircleShape"))
(define-foreign-type c-segment-shape (c-pointer "cpSegmentShape"))
(define-foreign-type c-polygon-shape (c-pointer "cpPolygonShape"))
(define-foreign-type c-space (c-pointer "cpSpace"))
(define-foreign-type space-body-iter-func (function void (c-body c-pointer)))
(define-foreign-type space-shape-iter-func (function void (c-shape c-pointer)))
(define-foreign-type space-constraint-iter-func (function void (c-constraint c-pointer)))
(define-foreign-type c-collision-begin-func     (function bool (c-arbiter c-space c-pointer)))
(define-foreign-type c-collision-presolve-func  (function bool (c-arbiter c-space c-pointer)))
(define-foreign-type c-collision-postsolve-func (function void (c-arbiter c-space c-pointer)))
(define-foreign-type c-collision-seperate-func  (function void (c-arbiter c-space c-pointer)))
(define-foreign-type c-post-step-func (function void (c-space c-pointer c-pointer)))
(define-foreign-type c-constraint (c-pointer "cpConstraint"))
(define-foreign-type c-arbiter (c-pointer "cpArbiter"))
(define-foreign-type c-nearest-point-query-info (c-pointer "cpNearestPointQueryInfo"))
(define-foreign-type c-contact-point-set (c-pointer "cpContactPointSet"))
(define-foreign-type c-nearest-point-query-func
  (function void (c-shape double (c-pointer "cpVect") c-pointer)))
(define-foreign-type c-segment-query-info (c-pointer "cpSegmentQueryInfo"))
(define-foreign-type c-space-segment-query-func
  (function void (c-shape double (c-pointer "cpVect") c-pointer)))
(define-foreign-type c-space-bb-query-func
  (function void (c-shape c-pointer)))
(define-foreign-type c-space-shape-query-func
  (function void (c-shape c-contact-point-set c-pointer)))

;-------------------------------------------------------
; cpBody
;-------------------------------------------------------

; Memory Management Functions


(define (create-body m i)
  (print m ", " i)
  ((foreign-lambda c-body "cpBodyNew" double double) m i))

(define (body-free body)
  ((foreign-lambda void "cpBodyFree" c-body) body))

; Creating Additional Static Bodies

(define create-static-body
  (foreign-lambda c-body "cpBodyNewStatic"))

; Properties

(%define-chipmunk-foreign-properties (body c-body)
  (mass (double "cpBodyGetMass" "cpBodySetMass"))
  (moment (double "cpBodyGetMoment" "cpBodySetMoment"))
  (position (vect "cpBodyGetPos" "cpBodySetPos"))
  (velocity (vect "cpBodyGetVel" "cpBodySetVel"))
  (force (vect "cpBodyGetForce" "cpBodySetForce"))
  (angle (double "cpBodyGetAngle" "cpBodySetAngle"))
  (angle-velocity (double "cpBodyGetAngVel" "cpBodySetAngle"))
  (torque (double "cpBodyGetTorque" "cpBodySetTorque"))
  (rotation (vect "cpBodyGetRot"))
  (velocity-limit (double "cpBodyGetVelLimit" "cpBodySetVelLimit"))
  (angular-velocity-limit (double "cpBodyGetAngVelLimit" "cpBodySetAngVelLimit"))

  (space (c-space "cpBodyGetSpace"))) 

(%define-chipmunk-foreign-methods (body c-body)
  ; Applying Forces and Torques
  (reset-forces ("cpBodyResetForces" void))
  (apply-force  ("cpBodyApplyForce" void vect vect))
  (apply-impulse ("cpBodyApplyImpulse" void vect vect))
  ; Sleeping Functions
  (sleeping? ("cpBodyIsSleeping" bool))
  (activate ("cpBodyActivate" void))
  (sleep ("cpBodySleep" void))
  ; Misc Functions
  (static? ("cpBodyIsStatic" bool))
  (roque? ("cpBodyIsRogue" bool)))

(define (body-activate-static body #!optional filter-shape)
  ((foreign-lambda void "cpBodyActivateStatic" c-body c-shape) body filter-shape))

(define (body-sleep-with-group body #!optional group)
  ((foreign-lambda void "cpBodySleepWithGroup" c-body c-body) body group))

; Iterators


(define body-each-shape
  (foreign-safe-lambda void "cpBodyEachShape" c-body c-body-shape-iter-func c-pointer))


(define body-each-constraint
  (foreign-safe-lambda void "cpBodyEachConstraint" c-body c-body-constraint-iter-func c-pointer))


(define body-each-arbiter
  (foreign-safe-lambda void "cpBodyEachArbiter" c-body c-body-arbiter-iter-func c-pointer))

; Moment of Inertia and Area Helper Functions

(define moment-for-circle (%cm-lambda double "cpMomentForCircle" double double double vect))
(define moment-for-seqment (%cm-lambda double "cpMomentForSegment" double vect vect))
(define moment-for-box (%cm-lambda double "cpMomentForBox" double double double))

(define (moment-for-polygon mass vertices offset)
  ((foreign-lambda* double ((double m) (int numVerts) (f32vector vertices) (f32vector offset)) "
	C_return(cpMomentForPoly(m, numVerts, (cpVect*)vertices, *(cpVect*)offset));")
   mass (length vertices) (vect-list->f32vector vertices) offset))

(define area-for-circle (foreign-lambda double "cpAreaForCircle" double double))
(define area-for-seqment (%cm-lambda double "cpAreaForSegment" vect vect double))

(define (area-for-polygon vertices)
  ((foreign-lambda* double ((int numVerts) (f32vector vertices)) "
	C_return(cpAreaForPoly(numVerts, (cpVect*)vertices));")
   (length vertices) (vect-list->f32vector vertices)))

; Coordinate Conversion Functions

(define body-local->world (%cm-lambda vect "cpBodyLocal2World" c-body vect))
(define body-world->local (%cm-lambda vect "cpBodyWorld2Local" c-body vect))

;-------------------------------------------------------
; cpCollisionType
;-------------------------------------------------------


;-------------------------------------------------------
; cpGroup
;-------------------------------------------------------

(define no-group (foreign-value "CP_NO_GROUP" integer))

;-------------------------------------------------------
; cpLayers
;-------------------------------------------------------

(define all-layers (foreign-value "CP_ALL_LAYERS" integer))

;-------------------------------------------------------
; cpShape
;-------------------------------------------------------

; Properties

(%define-chipmunk-foreign-properties (shape c-shape)
  (body (c-body "cpShapeGetBody" "cpShapeSetBody"))
  (bb (bb "cpShapeGetBB"))
  (sensor (bool "cpShapeGetSensor" "cpShapeSetSensor"))
  (elasticity (double "cpShapeGetElasticity" "cpShapeSetElasticity"))
  (friction (double "cpShapeGetFriction" "cpShapeSetFriction"))
  (surface-velocity (vect "cpShapeGetSurfaceVelocity" "cpShapeSetSurfaceVelocity"))
  (collision-type (c-collision-type "cpShapeGetCollisionType" "cpShapeSetCollisionType"))
  (group (c-group "cpShapeGetGroup" "cpShapeSetGroup"))
  (layers (c-layers "cpShapeGetLayers" "cpShapeSetLayers"))
  (space (c-space "cpShapeGetSpace")))
  
; Memory Management Functions

(define shape-free
  (foreign-lambda void "cpShapeFree" c-shape))

; Misc Functions
(%define-chipmunk-foreign-methods (shape c-shape)
  (cache-bb ("cpShapeCacheBB" bb))
  (update ("cpShapeUpdate" bb vect vect)))

(define reset-id-counter (%cm-lambda void "cpResetShapeIdCounter"))

;-------------------------------------------------------
; cpCircleShape
;-------------------------------------------------------


(define create-circle-shape (%cm-lambda c-shape "cpCircleShapeNew" c-body double vect))

(%define-chipmunk-foreign-methods (cicle-shape c-shape)
  (offset ("cpCircleShapeGetOffset" vect))
  (radius ("cpCircleShapeGetRadius" double)))

;-------------------------------------------------------
; cpSegmentShapea
;-------------------------------------------------------


(define create-segment-shape (%cm-lambda c-shape "cpSegmentShapeNew" c-body vect vect double))

(%define-chipmunk-foreign-methods (segment-shape c-shape)
  (a ("cpSegmentShapeGetA" vect))
  (b ("cpSegmentShapeGetB" vect))
  (normal ("cpSegmentShapeGetNormal" vect))
  (radius ("cpSegmentShapeGetRadius" double)))

(define segment-shape-set-neighbors! (%cm-lambda void "cpSegmentShapeSetNeighbors" c-shape vect vect))

;-------------------------------------------------------
; cpPolygonShape
;-------------------------------------------------------


;; (define %create-polygon-shape
;;   (%cm-lambda c-pointer "cpPolygonShapeNew" c-body integer vects vect))

(define (create-polygon-shape body vertices offset #!optional radius)
  (let ((vertices-vector (vect-list->f32vector vertices)))
    ((if radius
         (foreign-lambda* c-shape ((c-body body) (integer numVerts)
                                   (f32vector verts) (f32vector offset) (double radius)) "
	C_return(cpPolyShapeNew2(body, numVerts/2, (cpVect*)verts, *(cpVect*)offset, radius));")
         (foreign-lambda* c-shape ((c-body body) (integer numVerts)
                                   (f32vector verts) (f32vector offset) (c-pointer radius)) "
	C_return(cpPolyShapeNew(body, numVerts/2, (cpVect*)verts, *(cpVect*)offset));"))
     body (f32vector-length vertices-vector) vertices-vector offset radius)))

(%define-chipmunk-foreign-methods (polygon-shape c-shape)
  (vertex-count ("cpPolyShapeGetNumVerts" integer))
  (vertex-ref ("cpPolyShapeGetVert" vect integer))
  (radius ("cpPolyShapeGetRadius" double)))

; Boxes


(define %create-box-shape-with-radius (%cm-lambda c-shape "cpBoxShapeNew3" c-body bb double))
(define %create-box-shape (%cm-lambda c-shape "cpBoxShapeNew2" c-body bb))

(define (create-box-shape body bb #!optional radius)
  (if radius
      (%create-box-shape-with-radius body bb radius)
      (%create-box-shape body bb)))

; Poly Shape Helper Functions

(define (valid-polygon? vertices)
  ((foreign-lambda* bool ((f32vector verts) (integer length)) "
	C_return(cpPolyValidate((cpVect*)verts, length));")
   (vect-list->f32vector vertices) (length vertices)))

(define (centroid-for-polygon vertices)
  (let ((ret (make-f32vector 2)))
   ((foreign-lambda* void ((f32vector verts) (integer length) (f32vector ret)) "
	cpVect r = cpCentroidForPoly(length, (cpVect*)verts);
	memcpy(ret, &r, sizeof(cpVect));")
    (vect-list->f32vector vertices) (length vertices) ret)
   ret))

(define (centroid-for-polygon vertices)
  ((foreign-lambda* void ((f32vector verts) (integer length)) "
	cpRecenterPoly(length, (cpVect*)verts);")
   (vect-list->f32vector vertices) (length vertices)))


;-------------------------------------------------------
; cpSpace
;-------------------------------------------------------


(define create-space
  (foreign-lambda c-space "cpSpaceNew"))

(define space-free
  (foreign-lambda void "cpSpaceFree" c-space))

; Properties

(%define-chipmunk-foreign-properties (space c-space)
  (iterations (integer "cpSpaceGetIterations" "cpSpaceSetIterations"))
  (gravity (vect "cpSpaceGetGravity" "cpSpaceSetGravity"))
  (damping (double "cpSpaceGetDamping" "cpSpaceSetDamping"))
  (idle-speed-treshold (double "cpSpaceGetIdleSpeedThreshold" "cpSpaceSetIdleSpeedThreshold"))
  (sleep-time-treshold (double "cpSpaceGetSleepTimeThreshold" "cpSpaceSetSleepTimeThreshold"))
  (collision-slop (double "cpSpaceGetCollisionSlop" "cpSpaceSetCollisionSlop"))
  (collision-bias (double "cpSpaceGetCollisionBias" "cpSpaceSetCollisionBias"))
  (collision-persistence (double "cpSpaceGetCollisionPersistence" "cpSpaceSetCollisionPersistence"))
  (current-time-step (double "cpSpaceGetCurrentTimeStep"))
  (locked? (bool "cpSpaceIsLocked"))
  (static-body (c-body "cpSpaceGetStaticBody")))

(%define-chipmunk-foreign-methods (space c-space)
  (add-shape ("cpSpaceAddShape" c-shape c-shape))
  (add-body ("cpSpaceAddBody" c-body c-body))
  (add-static-shape ("cpSpaceAddStaticShape" c-shape c-shape))
  (add-constraint ("cpSpaceAddConstraint" c-constraint c-constraint))
  (remove-shape ("cpSpaceRemoveShape" void c-shape))
  (remove-body ("cpSpaceRemoveBody" void c-body))
  (remove-constraint ("cpSpaceRemoveConstraint" void c-constraint))
  (has-shape? ("cpSpaceContainsShape" bool c-shape))
  (has-body? ("cpSpaceContainsBody" bool c-body))
  (has-constraint? ("cpSpaceContainsConstraint" bool c-constraint))
  (body->static ("cpSpaceConvertBodyToStatic" void c-body))
  (body->dynamic ("cpSpaceConvertBodyToDynamic" void c-body double double))
  (reindex-shape ("cpSpaceReindexShape" void c-shape))
  (reindex-shapes-for-body ("cpSpaceReindexShapesForBody" void c-body))
  (reindex-static ("cpSpaceReindexStatic" void)))
  
; Iterators


(define space-each-body
  (foreign-safe-lambda void "cpSpaceEachBody" c-space space-body-iter-func c-pointer))


(define space-each-shape
  (foreign-safe-lambda void "cpSpaceEachShape" c-space space-shape-iter-func c-pointer))


(define space-each-constraint
  (foreign-safe-lambda void "cpSpaceEachConstraint" c-space space-constraint-iter-func c-pointer))

; Simulating the Space

(define space-step
  (foreign-safe-lambda void "cpSpaceStep" c-space double))

; Enabling and Tuning the Spatial Hash

(define space-use-spatital-hash
  (foreign-lambda void "cpSpaceUseSpatialHash" c-space double integer))

; Collision Handler API


(define %space-add-collision-handler
  (foreign-lambda void "cpSpaceAddCollisionHandler"
                  c-space
                  c-collision-type c-collision-type
                  c-collision-begin-func
                  c-collision-presolve-func
                  c-collision-postsolve-func
                  c-collision-seperate-func
                  c-pointer))

(define %space-remove-collision-handler
  (foreign-lambda void "cpSpaceRemoveCollisionHandler"
                  c-space
                  c-collision-type c-collision-type))

(define %space-default-collision-handler-set!
  (foreign-lambda void "cpSpaceSetDefaultCollisionHandler"
                  c-space
                  c-collision-begin-func
                  c-collision-presolve-func
                  c-collision-postsolve-func
                  c-collision-seperate-func
                  c-pointer))

; Post-Step Callbacks


(define space-add-poststep-callback
  (foreign-lambda bool "cpSpaceAddPostStepCallback"
                  c-space
                  c-post-step-func
                  c-pointer c-pointer))

;-------------------------------------------------------
; cpConstaint
;-------------------------------------------------------


(define constraint-free
  (foreign-lambda void "cpConstraintFree" c-constraint))

(%define-chipmunk-foreign-properties (constraint c-constraint)
  (a (c-body "cpConstraintGetA"))                                     
  (b (c-body "cpConstraintGetB"))
  (max-force (double "cpConstraintGetMaxForce" "cpConstraintSetMaxForce"))
  (error-bias (double "cpConstraintGetErrorBias" "cpConstraintSetErrorBias"))
  (max-bias (double "cpConstraintGetMaxBias" "cpConstraintSetMaxBias"))
  (space (c-space "cpConstraintGetSpace"))
  (impulse (double "cpConstraintGetImpulse")))

;-------------------------------------------------------
; cpPinJoint
;-------------------------------------------------------

(define create-pin-joint
  (%cm-lambda c-constraint "cpPinJointNew" c-body c-body vect vect))

(%define-chipmunk-foreign-properties (pin-joint c-constraint)
  (anchor-a (vect "cpPinJointGetAnchr1" "cpPinJointSetAnchr1"))
  (anchor-b (vect "cpPinJointGetAnchr2" "cpPinJointSetAnchr2"))
  (distance (double "cpPinJointGetDist" "cpPinJointSetDist")))

;-------------------------------------------------------
; cpSlideJoint
;-------------------------------------------------------

(define create-slide-joint
  (%cm-lambda c-constraint "cpSlideJointNew" c-body c-body vect vect double double))

(%define-chipmunk-foreign-properties (slide-joint c-constraint)
  (anchor-a (vect "cpSlideJointGetAnchr1" "cpSlideJointSetAnchr1"))
  (anchor-b (vect "cpSlideJointGetAnchr2" "cpSlideJointSetAnchr2"))
  (min (double "cpSlideJointGetMin" "cpSlideJointSetMin"))
  (max (double "cpSlideJointGetMax" "cpSlideJointSetMax")))

;-------------------------------------------------------
; cpPivotJoint
;-------------------------------------------------------

(define create-pivot-joint-with-pivot
  (%cm-lambda c-constraint "cpPivotJointNew" c-body c-body vect))

(define create-pivot-joint-with-anchors
  (%cm-lambda c-constraint "cpPivotJointNew2" c-body c-body vect vect))

(%define-chipmunk-foreign-properties (pivot-joint c-constraint)
  (anchor-a (vect "cpPivotJointGetAnchr1" "cpPivotJointSetAnchr1"))
  (anchor-b (vect "cpPivotJointGetAnchr2" "cpPivotJointSetAnchr2")))

;-------------------------------------------------------
; cpGrooveJoint
;-------------------------------------------------------

(define create-groove-joint
  (%cm-lambda c-constraint "cpGrooveJointNew" c-body c-body vect vect vect))

(%define-chipmunk-foreign-properties (groove-joint c-constraint)
  (anchor (vect "cpGrooveJointGetAnchr2" "cpGrooveJointSetAnchr2"))
  (groove-a (vect "cpGrooveJointGetGrooveA" "cpGrooveJointSetGrooveA"))
  (groove-b (vect "cpGrooveJointGetGrooveB" "cpGrooveJointSetGrooveB")))

;-------------------------------------------------------
; cpDampedSpring
;-------------------------------------------------------

(define create-damped-spring
  (%cm-lambda c-constraint "cpDampedSpringNew"
              c-body c-body
              vect vect
              double double double))

(%define-chipmunk-foreign-properties (damped-sping-joint c-constraint)
  (anchor-a (vect "cpDampedSpringGetAnchr1" "cpDampedSpringSetAnchr1"))
  (anchor-b (vect "cpDampedSpringGetAnchr2" "cpDampedSpringSetAnchr2"))
  (rest-length (double "cpDampedSpringGetRestLength" "cpDampedSpringSetRestLength"))
  (stiffness (double "cpDampedSpringGetStiffness" "cpDampedSpringSetStiffness"))
  (damping (double "cpDampedSpringGetDamping" "cpDampedSpringSetDamping")))

;-------------------------------------------------------
; cpDampedRotarySpring
;-------------------------------------------------------

(define create-damped-rotary-spring
  (%cm-lambda c-constraint "cpDampedRotarySpringNew"
              c-body c-body
              double double double))

(%define-chipmunk-foreign-properties (damped-sping-joint c-constraint)
  (rest-length (double "cpDampedRotarySpringGetRestAngle" "cpDampedRotarySpringSetRestAngle"))
  (stiffness (double "cpDampedRotarySpringGetStiffness" "cpDampedRotarySpringSetStiffness"))
  (damping (double "cpDampedRotarySpringGetDamping" "cpDampedRotarySpringSetDamping")))

;-------------------------------------------------------
; cpRotaryLimitJoint
;-------------------------------------------------------

(define create-rotary-limit-joint
  (%cm-lambda c-constraint "cpRotaryLimitJointNew" c-body c-body double double))

(%define-chipmunk-foreign-properties (rotary-limit-joint c-constraint)
  (min (double "cpRotaryLimitJointGetMin" "cpRotaryLimitJointSetMin"))
  (max (double "cpRotaryLimitJointGetMax" "cpRotaryLimitJointSetMax")))

;-------------------------------------------------------
; cpRatchetJoint
;-------------------------------------------------------

(define create-ratchet-joint
  (%cm-lambda c-constraint "cpRatchetJointNew" c-body c-body double double))

(%define-chipmunk-foreign-properties (ratchet-joint c-constraint)
  (angle (double "cpRatchetJointGetAngle" "cpRatchetJointSetAngle"))
  (phase (double "cpRatchetJointGetPhase" "cpRatchetJointSetPhase"))
  (ratchet (double "cpRatchetJointGetRatchet" "cpRatchetJointSetRatchet")))

;-------------------------------------------------------
; cpGearJoint
;-------------------------------------------------------

(define create-gear-joint
  (%cm-lambda c-constraint "cpGearJointNew" c-body c-body double double))

(%define-chipmunk-foreign-properties (gear-joint c-constraint)
  (phase (double "cpGearJointGetPhase" "cpGearJointSetPhase"))
  (ratio (double "cpGearJointGetRatio" "cpGearJointSetRatio")))

;-------------------------------------------------------
; cpSimpleMotor
;-------------------------------------------------------

(define create-simple-motor
  (%cm-lambda c-constraint "cpSimpleMotorNew" c-body c-body double))

(%define-chipmunk-foreign-properties (simple-motor c-constraint)
  (rate (double "cpSimpleMotorGetRate" "cpSimpleMotorSetRate")))

;-------------------------------------------------------
; cpArbiter
;-------------------------------------------------------


(%define-chipmunk-foreign-properties (arbiter c-arbiter)
  (elasticity (double "cpArbiterGetElasticity" "cpArbiterSetElasticity"))
  (friction (double "cpArbiterGetFriction" "cpArbiterSetFriction"))
  (surface-velocity (vect "cpArbiterGetSurfaceVelocity" "cpArbiterSetSurfaceVelocity"))
  (count (integer "cpArbiterGetCount")))

(%define-chipmunk-foreign-methods (arbiter c-arbiter)
  (normal ("cpArbiterGetNormal" vect integer))
  (point ("cpArbiterGetPoint" vect integer))
  (depth ("cpArbiterGetDepth" double integer))
  (first-contact? ("cpArbiterIsFirstContact" bool)))

(define (arbiter-get-shapes arbiter)
  (let ((ret (make-pointer-vector 2)))
    ((foreign-lambda* void ((c-arbiter arbiter) (pointer-vector ret)) "
	cpArbiterGetShapes(arbiter, &((cpShape**)ret)[0], &((cpShape**)ret)[1]);")
     arbiter ret)
    (list (pointer-vector-ref ret 0)
          (pointer-vector-ref ret 1))))

(define (arbiter-get-bodies arbiter)
  (let ((ret (make-pointer-vector 2)))
    ((foreign-lambda* void ((c-arbiter arbiter) (pointer-vector ret)) "
	cpArbiterGetBodies(arbiter, &((cpBody**)ret)[0], &((cpBody**)ret)[1]);")
     arbiter ret)
    (list (pointer-vector-ref ret 0)
          (pointer-vector-ref ret 1))))

; Contact Point Sets

;; TODO: setting points, and cpArbiterContactPointSet
;; I don't know any clean way of doing this yet.

(define (contact-point-set-length set)
  ((foreign-lambda* integer ((c-contact-point-set set)) "
	C_return(set->count);") set))

(define (contact-point-set-point-ref set index)
  (let ((ret (make-f32vector 2)))
    ((foreign-lambda* void ((c-contact-point-set set) (integer index) (f32vector ret)) "
	cpVect r = set->points[index].point;
	memcpy(ret, &r, sizeof(cpVect));") set index ret)
    ret))

(define (contact-point-set-normal-ref set index)
  (let ((ret (make-f32vector 2)))
    ((foreign-lambda* void ((c-contact-point-set set) (integer index) (f32vector ret)) "
	cpVect r = set->points[index].normal;
	memcpy(ret, &r, sizeof(cpVect));") set index ret)
    ret))

(define (contact-point-set-distance-ref set index)
  ((foreign-lambda* double ((c-contact-point-set set) (integer index)) "
	C_return(set->points[index].dist);") set index))

; Helper functions

(define arbiter-total-impuse-with-friction
  (%cm-lambda vect "cpArbiterTotalImpulseWithFriction" c-arbiter))

(define arbiter-total-impuse
  (%cm-lambda vect "cpArbiterTotalImpulse" c-arbiter))

(define arbiter-total-kinetic-energy
  (foreign-lambda double "cpArbiterTotalKE" c-arbiter))

;-------------------------------------------------------
; Queries
;-------------------------------------------------------

; Nearest Point Queries



(define nearest-point-query-info-shape
  (foreign-lambda* c-shape ((c-nearest-point-query-info info)) "
	C_return(info->shape);"))

(define (nearest-point-query-info-closest-point info)
  (let ((ret (make-f32vector 2)))
   ((foreign-lambda* void ((c-nearest-point-query-info info) (f32vector ret)) "
	memcpy(ret, &info->p, sizeof(cpVect));") info ret)
   ret))

(define (nearest-point-query-info-distance info)
  ((foreign-lambda* double ((c-nearest-point-query-info info)) "
	C_return(info->d);") info))

(define (nearest-point-query-info-closest-gradient-distance info)
  (let ((ret (make-f32vector 2)))
   ((foreign-lambda* void ((c-nearest-point-query-info info) (f32vector ret)) "
	memcpy(ret, &info->g, sizeof(cpVect));") info ret)
   ret))

(define (shape-nearest-point-query shape point)
  (let ((ret ((set-finalizer!
               (allocate (foreign-type-size c-nearest-point-query-info))
               free))))
    ((foreign-lambda* void ((c-shape shape) (f32vector point)
                            (c-nearest-point-query-info ret)) "
	cpShapeNearestPointQuery(shape, *(cpVect*)point, ret);") shape point ret)
    ret))

(define (space-nearest-point-query space
				   point max-distance
				   layers group
				   func data)
  ((foreign-safe-lambda* void ((c-space space)
			  (f32vector point) (double md)
			  (c-layers layers) (c-group group)
			  (c-nearest-point-query-func func) (c-pointer data)) "
	_SpaceNearestPointQuery(space, *(cpVect*)point, md, layers, group, func, data);")
   space point max-distance layers group func data))

(define (space-nearest-point-query-nearest space point
                                           max-distance
                                           layers group)
  (let ((ret (set-finalizer!
              (allocate (foreign-type-size "cpNearestPointQueryInfo"))
              free)))
   ((foreign-safe-lambda* void ((c-space space)
                                (f32vector point) (double md)
                                (c-layers layers) (c-group group)
                                (c-nearest-point-query-info ret)) "
	cpSpaceNearestPointQueryNearest(space, *(cpVect*)point, md, layers, group, ret);")
    space point max-distance layers group ret)
   ret))

; Segment Queries



(define segment-query-info-shape
  (foreign-lambda* c-shape ((c-segment-query-info info)) "
	C_return(info->shape);"))

(define (segment-query-info-normalized-distance info)
  ((foreign-lambda* double ((c-segment-query-info info)) "
	C_return(info->t);") info))

(define (seqment-query-info-normal info)
  (let ((ret (make-f32vector 2)))
   ((foreign-lambda* void ((c-segment-query-info info) (f32vector ret)) "
	memcpy(ret, &info->n, sizeof(cpVect));") info ret)
   ret))

(define (shape-segment-query shape vect-a vect-b)
  (let ((ret (set-finalizer!
              (allocate (foreign-type-size c-segment-query-info))
              free)))
    ((foreign-safe-lambda* void ((c-shape shape)
                                 (f32vector a) (f32vector b)
                                 (c-segment-query-info ret)) "
	cpShapeSegmentQuery(shape, *(cpVect*)a, *(cpVect*)b, ret);")
     shape vect-a vect-b ret)
    ret))

(define (space-segment-query space
                             vect-start vect-end
                             layers group
                             func data)
  ((foreign-safe-lambda* void ((c-space space)
                               (f32vector start) (f32vector end)
                               (c-layers layers) (c-group group)
                               (c-space-segment-query-func func)
                               (c-pointer data)) "
	_SpaceSegmentQuery(space, *(cpVect*)start, *(cpVect*)end, layers, group, func, data);")
   space vect-start vect-end layers group func data))

(define (segment-query-hit-point start end info)
  (let ((ret (make-f32vector 2)))
   ((foreign-lambda* void ((f32vector start) (f32vector end)
                           (c-segment-query-info info)
                           (f32vector ret)) "
	cpVect r = cpSegmentQueryHitPoint(*(cpVect*)start, *(cpVect*)end, *info);
	memcpy(ret, &r, sizeof(cpVect));") start end info ret)
   ret))

; AABB Queries


(define (space-bb-query space bb
                        layers group
                        func data)
  ((foreign-lambda* void ((c-space space) (f32vector bb)
                          (c-layers layers) (c-group group)
                          (c-space-bb-query-func func) (c-pointer data)) "
	cpSpaceBBQuery(space, *(cpBB*)bb, layers, group, func, data);")
   space bb layers group func data))

; Shape Queries


(define space-shape-query
  (foreign-safe-lambda bool cpSpaceShapeQuery
                       c-space c-shape c-space-shape-query-func c-pointer))

;-------------------------------------------------------
; Misc
;-------------------------------------------------------

(define infinity (foreign-value "INFINITY" double))
