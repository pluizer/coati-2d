(declare (unit chunk))

#>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct _DV_IndexStack;

typedef struct _DV_Vector
{
	void* data;
	unsigned* indices;
	struct _DV_IndexStack* available_stack;
	struct _DV_IndexStack* last_stack;
	unsigned size;
	unsigned size_hint;
	unsigned chunk_size;
} DV_Vector;

static void* smalloc(size_t size)
{
	void* ptr = malloc(size);
	if (!ptr)
	{
		fprintf(stderr, "chunk.scm: smalloc(%d): Out of memory!", size);
		exit(-1);
	}
	return ptr;
}

static void* srealloc(void* old, size_t size)
{
	void* ptr = realloc(old, size);
	if (!ptr)
	{
		fprintf(stderr, "chunk.scm: srealloc(%d, %d): Out of memory!", old, size);
		exit(-1);
	}
	return ptr;
}

typedef struct _DV_IndexStack
{
	unsigned* data;
	unsigned size_hint;
	unsigned size;
} DV_IndexStack;

DV_IndexStack* new_index_stack(unsigned size_hint)
{
	DV_IndexStack* is = smalloc(sizeof(DV_IndexStack));
	is->data = smalloc(sizeof(unsigned) * size_hint);
	is->size_hint = size_hint;
	is->size = 0;
	return is;
}

void free_index_stack(DV_IndexStack* is)
{
	free(is->data);
	free(is);
}

void index_stack_grow(DV_IndexStack* is)
{
	is->size_hint = (is->size_hint*2)+1;
	is->data = srealloc(is->data, sizeof(unsigned) * is->size_hint);
}

void index_stack_push(DV_IndexStack* is, unsigned index)
{
	if (is->size >= is->size_hint)
	{
		index_stack_grow(is);
	}
	is->data[is->size++] = index;
}

int index_stack_empty(DV_IndexStack* is)
{
	return !is->size;
}

unsigned index_stack_pop(DV_IndexStack* is)
{
	assert(!index_stack_empty(is));
	return is->data[--is->size];
}

DV_Vector* dv_vector_new(unsigned chunk_size, unsigned size_hint)
{
	DV_Vector* dv = smalloc(sizeof(DV_Vector));
	assert(dv);
	dv->indices = smalloc(sizeof(unsigned)*size_hint);
	memset(dv->indices, 0, sizeof(unsigned)*size_hint);
	dv->available_stack = new_index_stack(size_hint);
	dv->last_stack = new_index_stack(size_hint);
	dv->size = 0;
	dv->size_hint = size_hint;
	dv->chunk_size = chunk_size;
	dv->data = smalloc(dv->chunk_size  * size_hint);
	memset(dv->data, 0, dv->chunk_size * size_hint);
	return dv;
}

void dv_vector_free(DV_Vector* dv)
{
	free(dv->data);
	free(dv->indices);
	free_index_stack(dv->available_stack);
	free_index_stack(dv->last_stack);
	free(dv);
}

unsigned vector_grow(DV_Vector* dv)
{
	unsigned old_max = dv->size_hint;
	dv->size_hint = (dv->size_hint*2)+1;
	dv->data = srealloc(dv->data, 
			    dv->chunk_size * dv->size_hint);
	dv->indices = srealloc(dv->indices, sizeof(unsigned) * dv->size_hint);
	memset(dv->indices+old_max, 0, sizeof(unsigned)*old_max);
	return dv->size_hint - old_max;
}


unsigned dv_vector_push(DV_Vector* dv, void* chunk, unsigned* grown_by)
{
	unsigned index = index_stack_empty(dv->available_stack) 
		? dv->size : index_stack_pop(dv->available_stack);
	
	*grown_by = 0;

	/* vector full */
	if (dv->size >= dv->size_hint)
	{
		*grown_by = vector_grow(dv);
	}

	memcpy(dv->data+(dv->size*dv->chunk_size), chunk, dv->chunk_size);
	dv->indices[index] = dv->size;
	dv->size++;
	index_stack_push(dv->last_stack, index);
	return index;
}

void dv_vector_remove(DV_Vector* dv, unsigned index)
{
	dv->size--;
	if (dv->indices[index] != dv->size)
	{
		memcpy(dv->data+(dv->chunk_size * dv->indices[index]),
		       dv->data+(dv->chunk_size * (dv->size)),
		       dv->chunk_size);
		unsigned last = index_stack_pop(dv->last_stack);
		dv->indices[last] = dv->indices[index];
	} else {
		index_stack_pop(dv->last_stack);
	}
	index_stack_push(dv->available_stack, index);
}

void dv_vector_change(DV_Vector* dv, unsigned index, void* chunk)
{
	memcpy(dv->data+(dv->chunk_size * dv->indices[index]),
	       chunk,
	       dv->chunk_size);
}

void* dv_vector_ref(DV_Vector* dv, unsigned index)
{
	return dv->data+(dv->chunk_size * dv->indices[index]);
}

void dv_vector_clear(DV_Vector* dv)
{
	free(dv->data);
	free(dv->indices);
	free_index_stack(dv->available_stack);
	free_index_stack(dv->last_stack);
	dv->size = 0;
	/* it is likely that the vector will be around the same size
	   again, but we'll shrink it a little so it doesn't grow
	   out of bounds */
	dv->size_hint *= .8;
	dv->indices = smalloc(sizeof(unsigned)*dv->size_hint);
	memset(dv->indices, 0, sizeof(unsigned)*dv->size_hint);
	dv->available_stack = new_index_stack(dv->size_hint);
	dv->last_stack = new_index_stack(dv->size_hint);
	dv->data = smalloc(dv->chunk_size  * dv->size_hint);
	memset(dv->data, 0, dv->chunk_size * dv->size_hint);
}

unsigned dv_vector_chunk_size(DV_Vector* dv)
{
	return dv->chunk_size;
}

unsigned dv_vector_size(DV_Vector* dv)
{
	return dv->size;
}

unsigned dv_vector_current_capacity(DV_Vector* dv)
{
	return dv->size_hint;
}

void* dv_vector_data(DV_Vector* dv)
{
	return dv->data;
}
<#

(define-foreign-type chunk-vector (c-pointer "DV_Vector"))

(define-syntax %define-chunk-vector
  (ir-macro-transformer
   (lambda (exp inj cmp)
     (apply
      (lambda (_ <prefix> <type-string> <make-vector> <vector-type>)

	(define %chunk-size
	  `(foreign-lambda unsigned-integer "dv_vector_chunk_size" chunk-vector))

	`(begin

	   ;; (make-<type>chunk-vector chunk-size [size-hint])
	   ;; Create a new chunk vector with a chunk-size of /size/.
	   (define (,(symbol-append 'make- (inj <prefix>) 'chunk-vector)
		    chunk-size #!optional (size-hint 64))
	     (set-finalizer!
	      ((foreign-lambda chunk-vector "dv_vector_new"
			       unsigned-integer unsigned-integer)
	       (* chunk-size (foreign-type-size ,(inj <type-string>)))
	       size-hint)
	      (foreign-lambda void "dv_vector_free" chunk-vector)))
	   
	   ;; (<type>vector-remove! vector index)
	   ;; Removes a chunk from the vector using its /index/.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector-remove!)
	     (foreign-lambda void "dv_vector_remove" chunk-vector unsigned-integer))

	   ;; (<type>vector-set! vectror index value)
	   ;; Changed the value of a chunk using its /index/.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector-set!)
	     (foreign-lambda void "dv_vector_change" chunk-vector 
			     unsigned-integer ,(inj <vector-type>)))

	   ;; (<type>vector-push! vector value)
	   ;; Pushes a new chunk to the vector.
	   (define (,(symbol-append (inj <prefix>) 'chunk-vector-push!)
		    chunk-vector data)
	     (let ((grown (make-u32vector 1)))
	       ((foreign-lambda unsigned-integer "dv_vector_push" 
				chunk-vector ,(inj <vector-type>) u32vector)
		chunk-vector data grown)))

	   ;; (<type>vector-ref vector index)
	   ;; Returns the data at /index/.
	   (define (,(symbol-append (inj <prefix>) 'chunk-vector-ref)
		    chunk-vector index)
	     (let* ((chunk-size (,%chunk-size chunk-vector))
		    (size (/ chunk-size (foreign-type-size ,(inj <type-string>))))
		    (r (,(inj <make-vector>) size)))
	       ((foreign-lambda* void ((,(inj <vector-type>) r)
				       (chunk-vector v)
				       (unsigned-integer i)
				       (unsigned-integer s))  "
	   		void* t = dv_vector_ref(v, i);
	   		memcpy(r, t, s);")  
		r chunk-vector index chunk-size) r))

	   ;; (<type>vector-length vector)
	   ;; Returns the number of chunks in the vector.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector-length)
	     (foreign-lambda unsigned-integer "dv_vector_size" chunk-vector))

	   ;; <type>vector->pointer
	   ;; Returns a pointer to the dense foreign array where the data
	   ;; is stored.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector->pointer)
	     (foreign-lambda c-pointer "dv_vector_data" chunk-vector))

	   ;; <type>vector-chunk-size
	   ;; Returns a pointer to the dense foreign array where the data
	   ;; is stored.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector-chunk-size)
	     (foreign-lambda c-pointer "dv_vector_chunk_size" chunk-vector))

	   ;; <type>vector-clear!
	   ;; Returns a pointer to the dense foreign array where the data
	   ;; is stored.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector-clear!)
	     (foreign-lambda void "dv_vector_clear" chunk-vector))

	   )) exp))))

(%define-chunk-vector f32 "float"    make-f32vector f32vector)
(%define-chunk-vector f64 "double"   make-f64vector f64vector)
(%define-chunk-vector s8  "int8_t"   make-s8vector  s8vector)
(%define-chunk-vector s16 "int16_t"  make-s16vector s16vector)
(%define-chunk-vector s32 "int32_t"  make-s32vector s32vector)
(%define-chunk-vector u8  "uint8_t"  make-u8vector  u8vector)
(%define-chunk-vector u16 "uint16_t" make-u16vector u16vector)
(%define-chunk-vector u32 "uint32_t" make-u32vector u32vector)

;; General
(define (make-chunk-vector type chunk-length #!optional (size-hint 64))
  (let* ((funcs
	  (case type
	    ((char: int8: byte:) 
	     (list make-s8chunk-vector s8chunk-vector-push!
		   s8chunk-vector-remove! s8chunk-vector-set! 
		   s8chunk-vector-ref s8chunk-vector-length
		   s8chunk-vector->pointer s8chunk-vector-chunk-size
		   s8chunk-vector-clear!))
	    ((uchar: uint8: unsigned-byte:)
	     (list make-u8chunk-vector u8chunk-vector-push!
		   u8chunk-vector-remove! u8chunk-vector-set! 
		   u8chunk-vector-ref u8chunk-vector-length
		   u8chunk-vector->pointer u8chunk-vector-chunk-size
		   u8chunk-vector-clear!))
	    ((short: int16:)
	     (list make-s16chunk-vector s16chunk-vector-push!
		   s16chunk-vector-remove! s16chunk-vector-set! 
		   s16chunk-vector-ref s16chunk-vector-length
		   s16chunk-vector->pointer s16chunk-vector-chunk-size
		   s16chunk-vector-clear!))
	    ((ushort: uint16: unsigned-short:)
	     (list make-u16chunk-vector u16chunk-vector-push!
		   u16chunk-vector-remove! u16chunk-vector-set! 
		   u16chunk-vector-ref u16chunk-vector-length
		   u16chunk-vector->pointer u16chunk-vector-chunk-size
		   u16chunk-vector-clear!))
	    ((int: int32: integer: integer32:)
	     (list make-s32chunk-vector s32chunk-vector-push!
		   s32chunk-vector-remove! s32chunk-vector-set! 
		   s32chunk-vector-ref s32chunk-vector-length
		   s32chunk-vector->pointer s32chunk-vector-chunk-size
		   s32chunk-vector-clear!))
	    ((uint: uint32: unsigned-int: unsigned-int32:
		    unsigned-make-integer: unsigned
		    integer: unsigned-integer32:)
	     (list make-u32chunk-vector u32chunk-vector-push!
		   u32chunk-vector-remove! u32chunk-vector-set!
		   u32chunk-vector-ref u32chunk-vector-length
		   u32chunk-vector->pointer u32chunk-vector-chunk-size
		   u32chunk-vector-clear!))
	    ((float: float32:)
	     (list make-f32chunk-vector f32chunk-vector-push!
		   f32chunk-vector-remove! f32chunk-vector-set! 
		   f32chunk-vector-ref f32chunk-vector-length
		   f32chunk-vector->pointer f32chunk-vector-chunk-size
		   f32chunk-vector-clear!))
	    ((double: float64:)
	     (list make-f64chunk-vector f64chunk-vector-push!
		   f64chunk-vector-remove! f64chunk-vector-set! 
		   f64chunk-vector-ref f64chunk-vector-length
		   f64chunk-vector->pointer f64chunk-vector-chunk-size
		   f64chunk-vector-chunk-size
		   f64chunk-vector-clear!))))
	 (vector ((car funcs) chunk-length size-hint)))
    (apply (lambda (_ push! remove! set! ref length pointer chunk-size clear)
	     (lambda (com #!rest args)
	       (apply (case com
			((push!) push!)
			((remove!) remove!)
			((set!) set!)
			((ref) ref)
			((length) length)
			((pointer) pointer)
			((chunk-size) chunk-size)
			((type) (lambda (_) type))
			((clear!) clear)
			(else (assert #f)))
		      (cons vector args))))
	   funcs)))

(define (chunk-vector:remove! vector index)
  (vector 'remove! index))

(define (chunk-vector:push! vector value)
  (vector 'push! value))

(define (chunk-vector:set! vector index value)
  (vector 'set! index value))

(define (chunk-vector:ref vector index)
  (vector 'ref index))

(define (chunk-vector:length vector)
  (vector 'length))

(define (chunk-vector:>pointer vector)
  (vector 'pointer))

(define (chunk-vector:chunk-size vector)
  (vector 'chunk-size))

(define (chunk-vector:type vector)
  (vector 'type))

(define (chunk-vector:clear! vector)
  (vector 'clear!))
