;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A dragable rectangle.  
;; The rectangle can be dragged to any position using the mouse.
;; The rectangle should be displayed initially as a solid green rectangle
;; and as a green outline rectangle when selected.
;; The mouse position should be visible as a red solid circle
;; and should not change inside the rectangle.

;; run with (run 0)

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide run)
(provide initial-world)
(provide world-to-center)
(provide world-selected?)
(provide world-after-mouse-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define scene0 (empty-scene 400 300))
(define CIRCLE-RADIUS 5)
(define RECT-CENTER-X 200)
(define RECT-CENTER-Y 150)
(define ZERO 0)

;; DIMENSIONS OF THE RECTANGLE

(define RECT-WIDTH 100)
(define RECT-HEIGHT 60)
(define HALF-RECTANGLE-WIDTH 50)
(define HALF-RECTANGLE-HEIGHT 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct world (x-pos y-pos is-selected? in-x in-y dist-x dist-y))
; A World is a (make-world Number Number Boolean Number Number Number Number)
; Interpretation:  
; x-pos, y-pos give the position of the center of the rectangle. 
; is-selected? is a boolean that describes whether or not the
; rectangle is selected.
; in-x and in-y specify the position of the mouse within the rectangle.
; dist-x and dist-y specify the distance of the mouse 
; from the center of the rectangle.

; Template:
; world-fn : World -> ??
; (define (world-fn w)
; (...(world-x-pos w)
; (world-y-pos w)
; (world-is-selected? w)
; (world-in-x w)
; (world-in-y w)
; (world-dist-x w)
; (world-dist-y w)))
 
; A RectangleMouseEvent is a partition of 
; MouseEvent into the following categories:
; -- "button-down"   (interpretation: select the rectangle)
; -- "drag"          (interpretation: drag the ractangle)
; -- "button-up"     (interpretation: unselect the rectangle)
; -- any other mouse event (interpretation: ignored)

; Template:
; rme-fn : RectangleMouseEvent -> ??
; (define (rme-fn rme)
; (cond
; [(mouse=? rme "button-down") ...]
; [(mouse=? rme "drag") ...]
; [(mouse=? rme "button-up") ...]
; [else ...]))

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RUN FUNCTION

; run : Any -> World
; GIVEN: any value
; EFFECT: Ignores its argument and runs the world.
; RETURNS: the final state of the world.

(define (run num)
(big-bang (initial-world num)
          (on-mouse world-after-mouse-event)
          (on-draw world-to-scene)))

; initial-world : Any -> World
; GIVEN: any value
; RETURNS: the initial world by ignoring the value.
; EXAMPLES: 
; (initial-world 10) -> (make-world 200 150 false ZERO ZERO ZERO ZERO)
; (initial-world 20) -> (make-world 200 150 false ZERO ZERO ZERO ZERO)
; DESIGN STRATEGY: Domain Knowledge
; DEFINITION: 

(define (initial-world num)
 (make-world RECT-CENTER-X RECT-CENTER-Y false ZERO ZERO ZERO ZERO))

; world-to-center : World -> Posn
; GIVEN: a world
; RETURNS: the coordinates of the center of the rectangle as a Position(posn)
; EXAMPLES: 
; (world-to-center (make-world 10 20 "true" ZERO ZERO ZERO ZERO)) -> (make-posn 10 20)
; (world-to-center (make-world 20 40 "false" ZERO ZERO ZERO ZERO)) -> (make-posn 20 40)
; DESIGN STRATEGY: Structural Decomposition on w : World
; DEFINITION:

(define (world-to-center world)
  (make-posn (world-x-pos world)
               (world-y-pos world)))
             
; world-to-scene : World -> Scene
; GIVEN: a World 
; RETURNS: a Scene that portrays the given world.
; EXAMPLE: 
; (world-to-scene (initial-world 10)) -> (make-world 200 150 false ZERO ZERO ZERO ZERO)
; DESIGN STRATEGY: Structural Decomposition on w : World
; DEFINITON:

(define (world-to-scene w)
  (if (equal? (world-is-selected? w) true)
  (place-image(circle CIRCLE-RADIUS "solid" "Red") 
              (world-in-x w) (world-in-y w)
  (place-image(rectangle RECT-WIDTH RECT-HEIGHT "outline" "green")
              (world-x-pos w)(world-y-pos w) scene0))
  (place-image (rectangle RECT-WIDTH RECT-HEIGHT "solid" "green") 
              (world-x-pos w)(world-y-pos w) scene0)))
  
; world-selected? : World -> Boolean
; GIVEN: a world
; RETURNS: true iff the rectangle is selected.
; EXAMPLES: 
; (world-selected? (make-world 10 20 true 0 0 0 0)) -> true
; (world-selected? (make-world 25 50 false 0 0 0 0)) -> false
; DESIGN STRATEGY: Structural Decomposition on w : World
; DEFINITION:

(define (world-selected? world)
  (world-is-selected? world))
                 
; world-after-mouse-event : World Number Number MouseEvent -> World
; GIVEN: The world along with the x and y coordinates 
; of the new mouse position and a mouse event.
; RETURNS: the world that follows the given mouse event.
; EXAMPLES: 
; (world-after-mouse-event (make-world 200 150 true 20 30 40 50) 50 50
; "button-down") -> (make-world 200 150 true 20 30 40 50)
; (world-after-mouse-event (make-world 200 150 true 0 0 0 0) 50 50
; "button-up") -> (make-world 200 150 false 0 0 0 0)
; (world-after-mouse-event (make-world 200 150 true 10 0 30 4) 50 50 
; "drag") -> (make-world 50 50 true 10 0 30 4)
; DESIGN STRATEGY: Structural Decomposition on rme : RectangleMouseEevent
; DEFINITION:

(define (world-after-mouse-event w mouse-x mouse-y rme)
   (cond
      [(mouse=? rme "button-down")(select-rectangle w mouse-x mouse-y)]
      [(mouse=? rme "drag")(drag-rectangle w mouse-x mouse-y)]
      [(mouse=? rme "button-up")(release-rectangle w mouse-x mouse-y)] 
      [else w]))

; select-rectangle : World Number Number -> World
; GIVEN: The world along with the x and y coordinates of the 
; new position of the mouse.
; RETURNS: A World
; EXAMPLES:
; (select-rectangle (make-world 200 150 true 0 0 0 0) 50 50)->
; (make-world 200 150 true 0 0 0 0)
; (select-rectangle (initial-world 10) 50 50)->
; (make-world 200 150 false 0 0 0 0)
; DESIGN STRATEGY: Structural Decomposition on w : World 
; DEFINITION:

(define (select-rectangle w mouse-x mouse-y)
  (if (in-rectangle? w mouse-x mouse-y)
  (make-world (world-x-pos w)(world-y-pos w) true mouse-x mouse-y 
  (- mouse-x (world-x-pos w))(- mouse-y (world-y-pos w)))w)) 
         
; drag-rectangle : World Number Number -> World
; GIVEN: The world along with the x and y coordinates of the 
; new position of the mouse.
; RETURNS: A World
; EXAMPLES:
; (drag-rectangle (make-world 200 150 true 10 20 30 40) 50 50)->
; (make-world 20 10 true 50 50 30 40)
; (drag-rectangle (initial-world 10) 50 50)->
; (make-world 50 50 true 50 50 0 0)
; DESIGN STRATEGY: Structural Decomposition on w : World
; DEFINITION:

(define (drag-rectangle w mouse-x mouse-y)
 (make-world (- mouse-x (world-dist-x w))(- mouse-y (world-dist-y w))
   true mouse-x mouse-y (world-dist-x w)(world-dist-y w)))
         
; release-rectangle : World Number Number -> World
; GIVEN: The world along with the x and y coordinates of the
; new position of the mouse.
; RETURNS: A World
; EXAMPLES:
; (release-rectangle (make-world 200 150 true) 50 50)->
; (make-world 200 150 false)
; (release-rectangle (initial-world 10) 50 50)->
; (make-world 200 150 false)
; DESIGN STRATEGY: Structural Decomposition on w : World
; DEFINITION:

(define (release-rectangle w mouse-x mouse-y)
  (make-world (world-x-pos w) (world-y-pos w) false
     mouse-x mouse-y (world-dist-x w)(world-dist-y w)))

; in-rectangle? : World Number Number -> World
; GIVEN: A World along with x and y coordinates of the mouse
; RETURNS: true iff the given coordinate is inside the rectangle.
; EXAMPLES:
; (in-rectangle? (initial-world 10) 50 50) -> false
; (in-rectangle? (make-world 20 30 true 0 0 0 0) 50 50) -> true
; strategy: structural decomposition on w : World

(define (in-rectangle? w x y)
  (and
    (<= 
      (- (world-x-pos w) HALF-RECTANGLE-WIDTH)
      x
      (+ (world-x-pos w) HALF-RECTANGLE-WIDTH))
    (<= 
      (- (world-y-pos w) HALF-RECTANGLE-HEIGHT)
      y
      (+ (world-y-pos w) HALF-RECTANGLE-HEIGHT))))

;; TESTS

(define-test-suite rectangle-tests
  
;; TESTS for creating initial world
(check-equal?(initial-world 10)
(make-world 200 150 false 0 0 0 0)"failed to create initial world")
(check-equal?(initial-world 20)
(make-world 200 150 false 0 0 0 0)"failed to create initial world")
  
;; TESTS for returning position of the center of the world
(check-equal?(world-to-center (make-world 10 20 "true" 0 0 0 0))
(make-posn 10 20)"failed to return the center of the world")
(check-equal?(world-to-center (make-world 20 40 "false" 0 0 0 0))
(make-posn 20 40)"failed to return the center of the world")
  
;; TESTS for world to scene function
(check-equal?(world-to-scene (initial-world 10))
(place-image (rectangle 100 60 "solid" "green") 200 150 scene0)
"failed to create a scene")
(check-equal?(world-to-scene (make-world 200 100 true 10 20 30 40))
(place-image(circle 5 "solid" "Red")10 20
(place-image(rectangle 100 60 "outline" "green")200 100 scene0))
"failed to create a scene")
  
;; TESTS to check if the rectangle is selected
(check-equal?(world-selected? 
(make-world 10 20 true 0 0 0 0))true 
"failed to check if world is selected")
(check-equal?(world-selected?
(make-world 25 50 false 0 0 0 0))false
"failed to check if world is selected")

;; TESTS to check world after mouse event
(check-equal?(world-after-mouse-event 
(make-world 200 150 true 20 30 40 50) 50 50 "button-down")
(make-world 200 150 true 20 30 40 50)
"failed to check world after mouse event")
(check-equal?(world-after-mouse-event
(make-world 200 150 true 0 0 0 0) 50 50 "button-up")
(make-world 200 150 false 50 50 0 0)
"failed to check world after mouse event")
(check-equal?(world-after-mouse-event 
(make-world 200 150 true 10 0 30 4) 50 50 "drag")
(make-world 20 46 true 50 50 30 4)
"failed to check world after mouse event")
(check-equal?(world-after-mouse-event 
(make-world 200 150 true 10 0 30 4) 50 50 "leave")
(make-world 200 150 true 10 0 30 4)
"failed to check world after mouse event")
  
;; TESTS for "button down" mouse event
(check-equal?(select-rectangle 
(make-world 200 150 true 0 0 0 0) 50 50)
(make-world 200 150 true 0 0 0 0)"failed to select rectangle")
(check-equal?(select-rectangle 
(initial-world 10) 50 50)
(make-world 200 150 false 0 0 0 0)"failed to select rectangle")
(check-equal?(select-rectangle 
(make-world 0 0 true 0 0 0 0)0 0)
(make-world 0 0 true 0 0 0 0)"failed to select rectangle")
  
;; TESTS for "drag" mouse event
(check-equal?(drag-rectangle 
(make-world 200 150 true 10 20 30 40) 50 50)
(make-world 20 10 true 50 50 30 40)"failed to drag rectangle")
(check-equal?(drag-rectangle
(initial-world 10) 50 50) 
(make-world 50 50 true 50 50 0 0)"failed to drag rectangle")
  
;; TESTS for "button up" mouse event
(check-equal?(release-rectangle 
(make-world 200 150 true 0 0 0 0) 50 50)
(make-world 200 150 false 50 50 0 0)"failed to release rectangle")
(check-equal?(release-rectangle 
(initial-world 10) 50 50)
(make-world 200 150 false 50 50 0 0)"failed to release rectangle")
  
;; TESTS to select if the mouse point is inside rectangle
(check-equal?(in-rectangle? 
(initial-world 10) 50 50)false 
"could not find location of mouse")
(check-equal?(in-rectangle?
(make-world 20 30 true 0 0 0 0) 50 50)true)
"could not find location of mouse")
(run-tests rectangle-tests)
