(declare (unit sound)
         (uses primitives))

(import scheme
        (chicken foreign)
        (chicken gc)
        (chicken format))

#>
#include <SDL2/SDL_mixer.h>
<#

(define-foreign-type mix-chunk (c-pointer "Mix_Chunk"))
(define-foreign-type mix-music (c-pointer "Mix_Music"))

(define %mix-default-frequency (foreign-value "MIX_DEFAULT_FREQUENCY" int))
(define %mix-default-format    (foreign-value "MIX_DEFAULT_FORMAT"    unsigned-short))
(define %mix-default-channels  (foreign-value "MIX_DEFAULT_CHANNELS"  int))

(define %mix-open-audio   (foreign-lambda int  "Mix_OpenAudio"   int unsigned-short int int))
(define %mix-close-audio  (foreign-lambda void "Mix_CloseAudio"))
(define %mix-load-wav     (foreign-lambda mix-chunk "Mix_LoadWAV"  c-string))
(define %mix-load-mus     (foreign-lambda mix-music "Mix_LoadMUS"  c-string))
(define %mix-free-chunk   (foreign-lambda void "Mix_FreeChunk"    mix-chunk))
(define %mix-free-music   (foreign-lambda void "Mix_FreeMusic"    mix-music))
(define %mix-play-channel (foreign-lambda int  "Mix_PlayChannel"  int mix-chunk int))
(define %mix-play-music   (foreign-lambda int  "Mix_PlayMusic"    mix-music int))
(define %mix-halt-channel (foreign-lambda int  "Mix_HaltChannel"  int))
(define %mix-halt-music   (foreign-lambda void "Mix_HaltMusic"))
(define %mix-pause-music  (foreign-lambda void "Mix_PauseMusic"))
(define %mix-resume-music (foreign-lambda void "Mix_ResumeMusic"))
(define %mix-paused-music (foreign-lambda int  "Mix_PausedMusic"))
(define %mix-playing-music(foreign-lambda int  "Mix_PlayingMusic"))
(define %mix-volume       (foreign-lambda int  "Mix_Volume"       int int))
(define %mix-volume-chunk (foreign-lambda int  "Mix_VolumeChunk"  mix-chunk int))
(define %mix-volume-music (foreign-lambda int  "Mix_VolumeMusic"  int))
(define %mix-set-position (foreign-lambda int  "Mix_SetPosition"  int short unsigned-char))

(define-record sound chunk)
(define-record music  ptr)

(define (%vol->sdl v)
  (inexact->exact (round (* (max 0.0 (min 1.0 v)) 128))))

(define (%sound:init)
  (unless (= (%mix-open-audio %mix-default-frequency
                               %mix-default-format
                               %mix-default-channels
                               2048)
             0)
    (error "Could not initialise SDL_mixer.")))

(define (sound:load filename)
  (let ((chunk (%mix-load-wav filename)))
    (unless chunk (error (sprintf "Could not load sound: ~a" filename)))
    (set-finalizer! (make-sound chunk)
                    (lambda (s) (%mix-free-chunk (sound-chunk s))))))

(define (music:load filename)
  (let ((ptr (%mix-load-mus filename)))
    (unless ptr (error (sprintf "Could not load music: ~a" filename)))
    (set-finalizer! (make-music ptr)
                    (lambda (m) (%mix-free-music (music-ptr m))))))

(define (sound:play! sound #!key (loops 0) (volume 1.0))
  (let ((ch (%mix-play-channel -1 (sound-chunk sound) loops)))
    (%mix-volume ch (%vol->sdl volume))
    ch))

(define (sound:play-at! sound position listener
                        #!key (loops 0) (volume 1.0) (max-distance 10.0))
  (let* ((dx       (- (vect:x position) (vect:x listener)))
         (dy       (- (vect:y position) (vect:y listener)))
         (rad      (atan dx (- dy)))
         (deg      (* rad 57.29577951))
         (angle    (modulo (inexact->exact (round deg)) 360))
         (dist     (sqrt (+ (* dx dx) (* dy dy))))
         (sdl-dist (inexact->exact (round (min 255.0 (* 255.0 (/ dist max-distance))))))
         (ch       (%mix-play-channel -1 (sound-chunk sound) loops)))
    (%mix-volume ch (%vol->sdl volume))
    (%mix-set-position ch angle sdl-dist)
    ch))

(define (sound:stop! channel)
  (%mix-halt-channel channel))

(define (sound:stop-all!)
  (%mix-halt-channel -1))

(define (sound:volume! sound vol)
  (%mix-volume-chunk (sound-chunk sound) (%vol->sdl vol)))

(define (music:play! music #!key (loops -1) (volume 1.0))
  (%mix-volume-music (%vol->sdl volume))
  (%mix-play-music (music-ptr music) loops))

(define (music:stop!)
  (%mix-halt-music))

(define (music:pause!)
  (%mix-pause-music))

(define (music:resume!)
  (%mix-resume-music))

(define (music:volume! vol)
  (%mix-volume-music (%vol->sdl vol)))

(define (music:playing?)
  (= (%mix-playing-music) 1))

(define (music:paused?)
  (= (%mix-paused-music) 1))
