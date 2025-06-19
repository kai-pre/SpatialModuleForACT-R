;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Kai Preuss
;;; Address     : TU Berlin, Faculty V
;;;             : Department of Psychology and Ergonomics
;;;             : 10587 Berlin
;;;             : preuss@tu-berlin.de
;;; 
;;; Copyright   : (c)2019 Kai Preuss
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : spatial.lisp
;;; Version     : 1.2
;;; 
;;; Description : Source code for the ACT-R Spatial Module.  
;;;
;;; Bugs        : [ ]	(Placeholder: Bug text here.)
;;;
;;; Todo        : [X]	Merge rotate-around-axis and -line
;;;
;;;				  [X]	Make compare-objects callable, including adding a slot/renaming existing slots for the points of another object
;;;
;;;				  [X]	Return results of compare-objects either as slot of spatial action or internal variable (i.e. like last-command)
;;;
;;;				  [X]	Make sure esc-parameter is implemented correctly at all points where delay/complexity is used
;;;
;;;				  [ ]	Apply transformations to attached objects as well (except for axis hinge and beyond) - but how? Can't manipulate chunks in declarative memory...
;;;						==> implement a function get-attached that gives the points of all recursively attached objects *except* object attached to the current-axis, in a single list
;;;						==> but then? Returning the transformed points into new objects?
;;;
;;;				  [ ]	Grouping functions - see models for an idea on how to implement those. But open questions - is grouping generalizable beyond specific models? Does grouping create a time delay?
;;;
;;;				  [ ]	compare-extent, mirror-horizontally / mirror-vertically
;;;
;;;				  [ ]	Separate actual object transformation and perspective change, especially in rotation? (big, big maybe!), maybe at least in how complexity is computed?
;;;
;;;				  [X]	fix complexity penalty computation
;;;
;;;				  [ ]	offer several options for complexity penalty computation (constant, linear, exponential, factorial)
;;;
;;;				  [X]	separate module activity into each buffer so models can produce differentiated storage and action activity
;;; 
;;; ----- History ----- [look also at function comments]
;;;
;;; 2019.01.07	Kai
;;;				: * Starting work on spatial module
;;; 2019.01.23	Kai
;;;             : * Beginning proper documentation
;;;				: * Starting work on testing model
;;;				: * Merged functions for rotating around a main axis and an arbitrary one
;;; 2019.01.25	Kai
;;;             : * Made compare-objects command available for spatial-action requests
;;;	2019.01.28	Kai
;;;				: * Improved :esc behavior
;;; 2019.12.28	Kai
;;;				: * Fixed normalization for rotating around arbitrary axes (didn't normalize, but did denormalize, leading to transformed objects further removed from the center)
;;; 2020.05.23	Kai
;;;				: * Removed hard-coding of 0.5 as an internal reference point for the delay in seconds
;;; 2020.05.24	Kai
;;;				: * Query commands such as compare-objects or compare-vector-angle do not add to current-complexity anymore
;;; 2022.04.20	Kai
;;;				: * Module functions are now internally called by 'function instead of #'function, as external connections require the former notation
;;; 2022.04.22	Kai
;;;				: * Changed delay default value from 1.0 to 0.01...finally
;;;				: * Adjusted lambda and schedule calls to work with external connections (e.g. Python)
;;; 2022.05.31	Kai
;;;				: * Bugfixes
;;;				: * Fixed weird complexity penalty function into something more sensible
;;; 2022.06.01	Kai
;;;				: * Module now differentiates between spatial and spatial-action buffer states, hopefully leading to separate module activity output
;;; 2023.12.05	Kai
;;;				: * Fixed faulty reset function of module, keeping spatial action buffer in busy state if reset mid-trial
;;;

(defvar spatial-module-bugfixing nil) ; enables additional output during processing

(defstruct spatial-module delay complexity complexity-factor (current-complexity 0) esc busy action-busy error action-error (last-command 'none))
; maybe go all fancy and implement bordeaux-threads like the official modules for thread safety? :priority :min

(defun create-spatial-module (model-name)
	(declare (ignore model-name))
	(make-spatial-module))

(defun delete-spatial-module (spatial)
	(declare (ignore spatial)))	

(defun reset-spatial-module (spatial)
	;Terms to reset (Variables, chunk-types, chunks ...)
	(setf (spatial-module-last-command spatial) 'none)
	(setf (spatial-module-current-complexity spatial) 0)
	(specify-compilation-buffer-type spatial imaginal)
	(chunk-type spatial-object class points transformations attached-to (origin T))
	(chunk-type spatial-command cmd value axis)
	(chunk-type spatial-result result)
	; clear busy and error flags
	(free-spatial-module spatial)
	)
	
(defun free-spatial-module (spatial)
	(setf (spatial-module-busy spatial) nil)
	(setf (spatial-module-action-busy spatial) nil)
	(setf (spatial-module-error spatial) nil)
	(setf (spatial-module-action-error spatial) nil))
	
(defun free-spatial-buffer (spatial)
	(setf (spatial-module-busy spatial) nil))
	
(defun free-spatial-action-buffer (spatial)
	(setf (spatial-module-action-busy spatial) nil))
	
(defun error-spatial-module (spatial)
	(setf (spatial-module-error spatial) t)
	(setf (spatial-module-action-error spatial) t))
	
(defun error-spatial-buffer (spatial)
	(setf (spatial-module-error spatial) t))
	
(defun error-spatial-action-buffer (spatial)
	(setf (spatial-module-action-error spatial) t))
	
(defun spatial-module-params (spatial param)
	(if (consp param)
		(case (car param)
			(:s-delay
				(setf (spatial-module-delay spatial) (cdr param)))
			(:s-complexity
				(setf (spatial-module-complexity spatial) (cdr param)))
			(:s-complexity-factor
				(setf (spatial-module-complexity-factor spatial) (cdr param)))
			(:esc
				(setf (spatial-module-esc spatial) (cdr param))))
		(case param
			(:s-delay (spatial-module-delay spatial))
			(:s-complexity (spatial-module-complexity spatial))
			(:s-complexity-factor (spatial-module-complexity-factor spatial))
			(:esc (spatial-module-esc spatial)))))
			
(defun spatial-module-queries (spatial buffer query value)
	(case buffer
		(spatial
			(case query
				(state
					(case value
						(busy (spatial-module-busy spatial))
						(free (not (spatial-module-busy spatial)))
						(error (spatial-module-error spatial))
						(t (model-warning "Bad state query to the spatial buffer" buffer))))
				(last-command "Queries for last command must be made to the spatial-action buffer")
				(current-complexity (equal value (spatial-module-current-complexity spatial)))
				(t (model-warning "Invalid query ~s to the spatial buffer" query))))
		(spatial-action
			(case query
				(state
					(case value
						(busy (spatial-module-action-busy spatial))
						(free (not (spatial-module-action-busy spatial)))
						(error (spatial-module-action-error spatial))
						(t (model-warning "Bad state query to the spatial-action buffer" buffer))))
				(last-command (equal value (spatial-module-last-command spatial)))
				(current-complexity "Queries for current complexity must be made to the spatial buffer")
				(t (model-warning "Invalid query ~s to the spatial-action buffer" query))))))

(defun spatial-module-requests (spatial buffer spec)
	(case buffer
		(spatial
			(if (test-for-clear-request spec)	; should clear buffer and reset current complexity if it works. If not, use one of those chunk-spec transformations that give out the modifier
				(progn
					(schedule-event-now 'clear :module 'spatial :destination 'spatial :output 'low)
					(if spatial-module-bugfixing (format t "*debug message* CHUNK CLEARED!~%")))
				(create-spatial-object spatial spec)))
		(spatial-action (progn (transform-spatial-object spatial spec) (if spatial-module-bugfixing (format t "*debug message* TRANSFORMING CHUNK!~%"))))
		(t (model-warning "Invalid request to spatial module"))))
		
(defun spatial-module-clear (spatial buffer chunk)
	(declare (ignore chunk))
	(if (eq buffer 'spatial)
		(setf (spatial-module-current-complexity spatial) 0)))
		
(defun spatial-remember-last-command (spatial command)
	(setf (spatial-module-last-command spatial) command))
		
(defun create-spatial-object (spatial spec)
	(if (or (spatial-module-busy spatial) (spatial-module-action-busy spatial)) (model-warning "Cannot create spatial object when busy")
		(let* ((chunk-def (chunk-spec-to-chunk-def spec)) (chunk (when chunk-def (car (define-chunks-fct (list (append (list 'isa 'spatial-object) chunk-def)))))))
			(when chunk
				;(let ((delay (if (spatial-module-esc spatial) (* (spatial-module-delay spatial) .2) .2))) ; 200ms for the creation of a mental image
				(let ((delay .2)) ; 200ms for the creation of a mental image
					(setf (spatial-module-busy spatial) t (spatial-module-error spatial) nil)
					(schedule-set-buffer-chunk 'spatial chunk delay :module 'spatial)
					(schedule-event-relative delay 'free-spatial-buffer :params (list spatial) :module 'spatial))
					(if spatial-module-bugfixing (format t "*debug message* CHUNK CREATED!~%"))
					))))
						
(defun transform-spatial-object (spatial spec)
	(if (or (spatial-module-action-busy spatial) (query-buffer 'spatial '(buffer empty)))
		(model-warning "Cannot transform spatial object without object in spatial buffer or when spatial-action buffer is busy")
		(if (>= (spatial-module-current-complexity spatial) (spatial-module-complexity spatial)) (progn (model-warning "Cannot transform spatial object: complexity limit of ~s transformations reached" (spatial-module-complexity spatial)) (error-spatial-module spatial))
			(let* ((chunk-def (chunk-spec-to-chunk-def spec)) (chunk (when chunk-def (car (define-chunks-fct (list chunk-def)))))
					(obj-spec (chunk-name-to-chunk-spec (buffer-read 'spatial)))
					;(obj-def (chunk-spec-to-chunk-def obj-spec))
					;(obj (when obj-def (car (define-chunks-fct (list obj-def)))))
					)
				(when chunk
					(let (;(delay (if (spatial-module-esc spatial) (* (spatial-module-delay spatial) .05) .05))
						(delay (spatial-module-delay spatial))
						(cmd-spec (chunk-spec-slot-spec spec 'cmd))
						(value-spec (chunk-spec-slot-spec spec 'value))
						(axis-spec (chunk-spec-slot-spec spec 'axis))
						(obj-points-spec (chunk-spec-slot-spec obj-spec 'points)))
							(if cmd-spec
								(if (= (length cmd-spec) 1)
									(if (eq (caar cmd-spec) '=)
										(if (or (eq (caddar cmd-spec) 'rescale)
											 	(eq (caddar cmd-spec) 'translate-by-x)
												(eq (caddar cmd-spec) 'translate-by-y)
												(eq (caddar cmd-spec) 'translate-by-z)
												(eq (caddar cmd-spec) 'translate-by-xyz)
												(eq (caddar cmd-spec) 'rotate-around-x)
												(eq (caddar cmd-spec) 'rotate-around-y)
												(eq (caddar cmd-spec) 'rotate-around-z)
												(eq (caddar cmd-spec) 'rotate-around-xyz)
												(eq (caddar cmd-spec) 'rotate-around-axis)
												(eq (caddar cmd-spec) 'rotate-around-line)
												(eq (caddar cmd-spec) 'compare-objects)
												(eq (caddar cmd-spec) 'compare-vector-angle))
												; add others as they become avaliable
											(if obj-points-spec
												(progn
													(if spatial-module-bugfixing (format t "*debug message* RECEIVING: cmd: ~a obj: ~a value: ~a~%" (caddar cmd-spec) (caddar obj-points-spec) (caddar value-spec)))
													(setf (spatial-module-action-busy spatial) t (spatial-module-action-error spatial) nil)
													(if (not (or (eq (caddar cmd-spec) 'compare-objects) (eq (caddar cmd-spec) 'compare-vector-angle)))
														(setf (spatial-module-current-complexity spatial) (1+ (spatial-module-current-complexity spatial))))
													;
													; preliminary change: quote of lambda instead of lambda to see if python ACT-R can deal with that
													;
													(schedule-event-now 'spatial-remember-last-command :params (list spatial (caddar cmd-spec)))
													;
													;
													;
													(if value-spec
														(progn
															(if (not (or (numberp (caddar value-spec)) (listp (caddar value-spec))))
																(model-warning "Invalid value - must be a number or a list, is: ~a" (type-of (caddar value-spec))))
															(if (= (length value-spec) 1)
																(if (eq (caar value-spec) '=)
																	(if axis-spec
																		(if (= (length axis-spec) 1)
																			(if (eq (caar axis-spec) '=)
																				(progn (if spatial-module-bugfixing (format t "*debug message* DOING TRANSFORMATION '~a' with value ~a on axis ~a!~%" (caddar cmd-spec) (caddar value-spec) (caddar axis-spec)))
																				(schedule-event-relative (complexity-function spatial delay (caddar value-spec) (spatial-module-current-complexity spatial)) 'apply-transformation :module 'spatial :priority :max :details "Applying spatial transformation" :destination 'spatial
																				:params (list (caddar obj-points-spec) (caddar cmd-spec) (caddar value-spec) (caddar axis-spec))))
																				(model-warning "Invalid axis slot modifier ~s in spatial-action request" (caar axis-spec)))
																			(model-warning "Multiple axis slots specified in spatial-action request"))
																		(progn (if spatial-module-bugfixing (format t "*debug message* DOING TRANSFORMATION '~a' with value ~a!~%" (caddar cmd-spec) (caddar value-spec)))
																		(schedule-event-relative (complexity-function spatial delay (caddar value-spec) (spatial-module-current-complexity spatial)) 'apply-transformation :module 'spatial :priority :max :details "Applying spatial transformation" :destination 'spatial
																		:params (list (caddar obj-points-spec) (caddar cmd-spec) (caddar value-spec)))))
																	(model-warning "Invalid value slot modifier ~s in spatial-action request" (caar value-spec)))
																(model-warning "Multiple value slots specified in spatial-action request")))
														(progn (if spatial-module-bugfixing  (format t "*debug message* DOING TRANSFORMATION '~a'!~%" (caddar cmd-spec)))
														(schedule-event-relative (complexity-function spatial delay 1 (spatial-module-current-complexity spatial)) 'apply-transformation :module 'spatial :priority :max :details "Applying spatial transformation" :destination 'spatial
														:params (list (caddar obj-points-spec) (caddar cmd-spec)))))
													(schedule-event-relative (complexity-function spatial delay (if value-spec (caddar value-spec) 1) (spatial-module-current-complexity spatial)) 'free-spatial-action-buffer :module 'spatial :params (list spatial)))
												(model-warning "Invalid object in spatial buffer: no action possible"))
											(model-warning "Unknown command in spatial-action request: ~s" (caddar cmd-spec)))
										(model-warning "Invalid cmd slot modifier ~s in spatial-action request" (caar cmd-spec)))
									(model-warning "Multiple commands specified in spatial-action request"))
								(model-warning "cmd slot missing in spatial-action request")))))))
	; clear spatial-action buffer (stays cleared, except when a "get"-style request is processed, e.g. compare-objects, then a chunk with the slot 'result' is created in the buffer)
	; seems to be done by the procedural system already...
	;(schedule-clear-buffer 'spatial-action 0 :module 'spatial)
	)
				
						

;;; The heart of spatial actions - list of possible commands:
;;; translate-by-x, translate-by-y, translate-by-z, translate-by-xyz,rotate-around-x, rotate-around-y, rotate-around-z, rotate-around-xyz, rotate-around-axis, compare-objects, compare-vector-angle		
(defun apply-transformation (spatial object cmd &rest params)
	(declare (ignore spatial))
	(if spatial-module-bugfixing (format t "*debug message* BEGINNING TRANSFORMATION WITH: cmd: ~a obj: ~a also starring: ~a, which of course is of length ~a~%" cmd object params (length params)))
	(let ((new-object (copy-list object)) (normalization (get-center object)))
		(case (length params)
				(0 (if spatial-module-bugfixing (format t "*debug message* 0 parameters..."))
					(setf new-object (mapcar #'(lambda (x) (funcall cmd (-list x normalization))) object)))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;; how should compare-vector-angle be handled? It compares value to axis...?
				(1 (if spatial-module-bugfixing (format t "*debug message* 1 parameter...~%"))
					(case cmd
						(compare-objects
							(if spatial-module-bugfixing (format t "*debug message* Considering to compare these objects: ~a and ~a...~%" object (first params)))
							(let* ((normalization2 (get-center (first params))) (obj1 (mapcar #'(lambda (x) (-list x normalization)) object)) (obj2 (mapcar #'(lambda (x) (-list x normalization2)) (first params)))
										(comparison (compare-objects obj1 obj2)))
								(schedule-set-buffer-chunk 'spatial-action (first (define-chunks-fct `((isa spatial-result result ,comparison)))) 0 :module 'spatial)))
						(otherwise
							(setf new-object (mapcar #'(lambda (x) (funcall cmd (-list x normalization) (first params))) object))
							(if spatial-module-bugfixing (format t "*debug message* Normalization...~%"))
							)))
				(2 (if spatial-module-bugfixing (format t "*debug message* 2 parameters...~%"))
					(case cmd
						(compare-vector-angle
							(if spatial-module-bugfixing (format t "*debug message* Considering to compare these vectors: ~a and ~a...~%" (first params) (second params)))
							(let* ((comparison (compare-vector-angle (first params) (second params)))) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
								(schedule-set-buffer-chunk 'spatial-action (first (define-chunks-fct `((isa spatial-result result ,comparison)))) 0 :module 'spatial)))
						(otherwise (if (listp (second params)) ; no normalization if rotating around an *arbitrary* axis
							(progn
								(if spatial-module-bugfixing (format t "*debug message* Rotating around arbitrary axis: ~a" (second params)))
								(setf new-object (mapcar #'(lambda (x) (funcall cmd x (first params) (second params))) object))
								(if spatial-module-bugfixing (format t "*debug message* No Normalization...~%")))
							(progn
								(setf new-object (mapcar #'(lambda (x) (funcall cmd (-list x normalization) (first params) (second params))) object))
								(if spatial-module-bugfixing (format t "*debug message* Rotating around ~a-axis" (second params)))
								(if spatial-module-bugfixing (format t "*debug message* Normalization...~%")))))))
				(t (model-warning "Invalid number of arguments: ~s, maximum: 5" (length params))))
		(if (or
					(and (not (second params)) (not (eq cmd 'compare-objects))) ; if less than 2 parameters and command is not compare-objects, or
					(and (second params) (not (listp (second params))) (not (eq cmd 'compare-vector-angle)))) ; if two parameters, the second parameter is not a list and command isn't compare-vector-angle,
			(progn
				(setf new-object (mapcar #'(lambda (x) (+list x normalization)) new-object))
				(if spatial-module-bugfixing (format t "*debug message* De-Normalization...~%"))) ; object was normalized, so move object back to former location
			(if spatial-module-bugfixing (format t "*debug message* No De-Normalization...~%")))
		;(if (not (eq cmd 'rotate-around-line)) (setf new-object (mapcar #'(lambda (x) (+ x normalization)) new-object)))
		(if spatial-module-bugfixing (format t "*debug message* NEW POINT CLOUD IS: ~a~%" new-object))
		(if (not (or (eq cmd 'compare-objects) (eq cmd 'compare-vector-angle)))
			(mod-buffer-chunk 'spatial `(points ,new-object)))))



(defun complexity-function (spatial delay degree currcomplex)	; Subject to change! For now, factorial function. Maybe later change to counting the actually rotated objects per transformation (as in Shepard&Feng1972)
	(let ((result 0))
		(if (not (numberp degree))						; If degree is not a number, but a list of numbers, sum up the absolutes of those numbers.
			(if (listp degree)							; If it is a list, but not of numbers, or neither number nor list, just set degree to 0.
				(if (numberp (first degree))			; Otherwise, continue.
					(setf degree (apply '+ (mapcar #'abs degree)))
					(setf degree 0))
				(setf degree 0)))
				
		(setf degree (abs degree))						; As degree can be negative, use its absolute.
		
		(if (spatial-module-esc spatial)				; If subsymbolic computation is active, use current complexity, otherwise ignore it
			(setf result (* delay degree (+ (* (sq currcomplex) (spatial-module-complexity-factor spatial)) 1)))
			; Revised square function: complexity factor for 0, 1, 2, 3, 4, 5, 6... is 1, 1.2, 1.8, 2.8, 4.2 ...
			(setf result (* delay degree)))
		(if spatial-module-bugfixing (format t "*debug message* COMPLEXITY PENALTY: ~a seconds, with current complexity of ~a!~%" result currcomplex))
		
		(round-to result 3)))						; Round up to 3 decimal digits



(define-module-fct 'spatial
						'(spatial spatial-action)	; List of buffers
	(list ; List of parameters
		(define-parameter :s-delay
			:documentation "Time factor to rotate or translate a spatial object"
			:default-value 0.01
			:valid-test (lambda (x) (and (numberp x) (>= x 0)))
			:warning "non-negative number"
			:owner t)		
		(define-parameter :s-complexity
			:documentation "Number of transformations allowed on spatial object"
			:default-value 6 ; maybe change to 4 in line with finsts?
			:valid-test (lambda (x) (and (integerp x) (>= x 0)))
			:warning "non-negative integer"
			:owner t)
		(define-parameter :s-complexity-factor
			:documentation "Modifying factor for the influence of complexity on delay computation"
			:default-value 0.02
			:valid-test (lambda (x) (and (numberp x) (>= x 0)))
			:warning "non-negative number"
			:owner t)
		; maybe add a "soft threshold"-parameter, adding a sigmoid function to thresholds (either nil or the standard deviation of the curve)
		(define-parameter :esc :owner nil))

	:version "1.4"
	:documentation "Module for storage and transformation of spatial mental images"
	:creation 'create-spatial-module
	:reset 'reset-spatial-module
	:delete 'delete-spatial-module
	:params 'spatial-module-params
	:request 'spatial-module-requests
	:query 'spatial-module-queries
	:notify-on-clear 'spatial-module-clear
)









;;;;; MATH
; Rounding to n decimal places.
(defun round-to (number precision)
    (let ((div (expt 10 precision)))
         (/ (fround (* number div)) div)))
		 
; Square.
(defun sq (number)
	(* number number))
	
; Factorial.
(defun factorial (number)
	(if (and (numberp number) (>= number 1))
		(if (= number 1) 1 (* number (factorial (- number 1)))) 0))
		
; Sum To N.
(defun sum-to (number)
	(if (and (numberp number) (>= number 1))
		(/ (* number (+ 1 number)) 2) 0))

; Inversing a list of numbers
(defun inverse (point)
	(if (not (null point)) (cons (* -1 (car point)) (inverse (cdr point)))))

; Calculating the geometric average of given points - leading to the "gravitational" center
; (defun get-center (points)
	; (if (listp (first points))
		; (let ((x nil) (y nil) (z nil) (len (length points)))
			; (dolist (point points)
				; (setf x (append x (list (first point))))
				; (setf y (append y (list (second point))))
				; (setf z (append z (list (third point)))))
			; (setf x (/ (float (apply '+ x)) len) y (/ (float (apply '+ y)) len) z (/ (float (apply '+ z)) len))
			; (list x y z))
		; points))

; Calculating the average of minimal and maximal point values per dimension - leading to a true geometrical center
(defun get-center (points)
	(if (listp (first points))
		(let* ((start (first points)) (min-x (first start)) (max-x (first start)) (min-y (second start)) (max-y (second start)) (min-z (third start)) (max-z (third start)))
			(dolist (point points)
				(setf min-x (min min-x (first point)) max-x (max max-x (first point)) min-y (min min-y (second point)) max-y (max max-y (second point)) min-z (min min-z (third point)) max-z (max max-z (third point))))
			(list (/ (+ min-x max-x) 2) (/ (+ min-y max-y) 2) (/ (+ min-z max-z) 2)))))

				
; Z-Transformation, using get-center
(defun z-transform (points)
	(let ((center (get-center points)))
		(mapcar #'(lambda (pts) (-list pts center)) points)))
			
; Cross product, for lists
(defun *matrix (A B)
	(if (not (eq (length (first A)) (length B))) (model-warning "Invalid Arguments: number of columns of first matrix not identical to number of columns of second matrix.")
		(let* ((rows-A (length A)) (cols-b (length (first B))) (common-AB (length (first A))) (products nil) (result (make-list rows-A)))
			(dotimes (line rows-A)
				(dotimes (column cols-B)
					(setf products nil)
					(dotimes (element common-AB)
						(setf products (append products (list (* (nth element (nth line A)) (nth column (nth element B)))))))
					;(print (apply '+ products))
					(setf (nth line result) (append (nth line result) (list (apply '+ products))))))
			result)))
			
; Dot product, for lists
(defun .vector (A B)
	(if (not (eq (length A) (length B))) (model-warning "Invalid Arguments: length of first vector not identical to length of second vector.")
		(let ((products nil) (result 0))
			(dotimes (i (length A))
				(setf products (append products (list (* (nth i A) (nth i B))))))
			(setf result (apply '+ products))
			result)))
			
; Element-wise substraction, for lists
(defun -list (a b)
	(if (not (eq (length a) (length b))) (model-warning "Invalid Arguments: length of first vector not identical to length of second vector.")
		(let ((result '()))
			(dotimes (i (length a))
				(setf result (append result (list (- (nth i a) (nth i b))))))
			result)))
			
; Element-wise addition, for lists
(defun +list (a b)
	(if (not (eq (length a) (length b))) (model-warning "Invalid Arguments: length of first vector not identical to length of second vector.")
		(let ((result '()))
			(dotimes (i (length a))
				(setf result (append result (list (+ (nth i a) (nth i b))))))
			result)))
			

			
;;; Rotation
(defun rmat-x (angle)
  (list (list 1 0 0) (list 0 (cos angle) (- 0 (sin angle))) (list 0 (sin angle) (cos angle))))

(defun rotate-around-x (point angle)
	;transpose point if needed
	(if (not (listp (first point))) (setf point (mapcar 'list point)))
	; convert degrees to radians
	(setf angle (/ (* angle pi) 180))
	(setf point (*matrix (rmat-x angle) point))
	;for now: transpose back for ease of use
	(list (first (first point)) (first (second point)) (first (third point))))
	
(defun rmat-y (angle)
  (list (list (cos angle) 0 (sin angle)) (list 0 1 0) (list (- 0 (sin angle)) 0 (cos angle))))

(defun rotate-around-y (point angle)
	;transpose point if needed
	(if (not (listp (first point))) (setf point (mapcar 'list point)))
	; convert degrees to radians
	(setf angle (/ (* angle pi) 180))
	(setf point (*matrix (rmat-y angle) point))
	;for now: transpose back for ease of use
	(list (first (first point)) (first (second point)) (first (third point))))

(defun rmat-z (angle)
  (list (list (cos angle) (- 0 (sin angle)) 0) (list (sin angle) (cos angle) 0) (list 0 0 1)))

(defun rotate-around-z (point angle)
	;transpose point if needed
	(if (not (listp (first point))) (setf point (mapcar 'list point)))
	; convert degrees to radians
	(setf angle (/ (* angle pi) 180))
	(setf point (*matrix (rmat-z angle) point))
	;for now: transpose back for ease of use
	(list (first (first point)) (first (second point)) (first (third point))))
	
(defun rmat-xyz (angles)
  (*matrix (*matrix (rmat-z (third angles)) (rmat-y (second angles))) (rmat-x (first angles))))

(defun rotate-around-xyz (point angles)
	;transpose point if needed
	(if (not (listp (first point))) (setf point (mapcar 'list point)))
	; convert degrees to radians
	(setf angles (list (/ (* (first angles) pi) 180) (/ (* (second angles) pi) 180) (/ (* (third angles) pi) 180)))
	(setf point (*matrix (rmat-xyz angles) point))
	;for now: transpose back for ease of use
	(list (first (first point)) (first (second point)) (first (third point))))

(defun rotate-around-axis (point angle axis)
	; convert degrees to radians
	;(if spatial-module-bugfixing (format t "*debug message* Converting....."))
	(setf angle (/ (* angle pi) 180))
	; Is axis-parameter a letter or a list?
	(if (not (listp axis))
		(progn
			;(if spatial-module-bugfixing (format t "*debug message* Axis is not a list, parsing letter...~%"))
			;transpose point if needed
			(if (not (listp (first point))) (setf point (mapcar 'list point)))
			(setf point (*matrix (funcall (read-from-string (concatenate 'string "rmat-" (string axis))) angle) point))
			;for now: transpose back for ease of use
			(list (first (first point)) (first (second point)) (first (third point))))
		(if (not (and (eq (length axis) 2) (eq (length (first axis)) 3))) (model-warning "Invalid line for arbitrary axis rotation: must be two points in three-dimensional space")
			(progn
				;(if spatial-module-bugfixing (format t "*debug message* Axis is a list, parsing x-y-z-coordinates...~%"))
				; (1) translate space so that the rotation axis passes through the origin
				(let* ((new-point (list (- (first point) (first (first axis))) (- (second point) (second (first axis))) (- (third point) (third (first axis))))) (temp-point '(0 0 0)) (u (list (- (first (second axis)) (first (first axis))) (- (second (second axis)) (second (first axis))) (- (third (second axis)) (third (first axis))))) (d 0) (line-magnitude (abs (sqrt (+ (sq (first u)) (sq (second u)) (sq (third u)))))))
					; Convert angle to radian
					; (setf angle (/ (* angle pi) 180))
					; Normalize unit vector
					;(if spatial-module-bugfixing (format t "*debug message* Normalizing unit vector...~%"))
					(setf u (list (/ (first u) line-magnitude) (/ (second u) line-magnitude) (/ (third u) line-magnitude)))
					;(if spatial-module-bugfixing (format t "*debug message* ...which is: ~a...~%" u))
					; "If rotation axis is already aligned with the z axis (XY-plane) then steps 2, 3, 5, and 6 need not be performed"
					;(if (not (= (sqrt (+ (sq (first u)) (sq (second u)))) 0))
						;(progn
				; (2) rotate space about the x axis so that the rotation axis lies in the xz plane
						; Check if line is already on YZ-plane
							(setf d (sqrt (+ (sq (second u)) (sq (third u)))))
							;(if spatial-module-bugfixing (format t "*debug message* d is: ~a ~%" d))
							;(if spatial-module-bugfixing (format t "*debug message* Check if aligned on YZ plane...~%"))
							(if (not (= d 0))
								; Rotate axis to XZ-plane
								(setf temp-point (list (first new-point) (- (/ (* (second new-point) (third u)) d) (/ (* (third new-point) (second u)) d)) (+ (/ (* (second new-point) (second u)) d) (/ (* (third new-point) (third u)) d))))		
								; Else: Set temporary points to normalized points
								(setf temp-point new-point))
				; (3) rotate space about the y axis so that the rotation axis lies along the z axis
							;(if spatial-module-bugfixing (format t "*debug message* Rotate around y-axis...~%"))
							(setf new-point (list (- (* (first temp-point) d) (* (third temp-point) (first u))) (second temp-point) (+ (* (first temp-point) (first u)) (* (third temp-point) d))));))
				; (4) perform the desired rotation by theta about the z axis
					; multiply by (rotate-around-z angle)
					; Right hand:
					(setf temp-point (list (- (* (first new-point) (cos angle)) (* (second new-point) (sin angle))) (+ (* (first new-point) (sin angle)) (* (second new-point) (cos angle))) (third new-point)))
					; ; Left hand:
					; (setf temp-point (list (+ (* (first new-point) (cos angle)) (* (second new-point) (sin angle))) (+ (- (* (first new-point) (sin angle))) (* (second new-point) (cos angle))) (third new-point)))
				; (5) apply the inverse of step (3)
					;(if (not (= (sqrt (+ (sq (first u)) (sq (second u)))) 0))
						;(progn
							(setf new-point (list (+ (* (first temp-point) d) (* (third temp-point) (first u))) (second temp-point) (+ (* (- 0 (first temp-point)) (first u)) (* (third temp-point) d))))
				; (6) apply the inverse of step (2)
							;(if spatial-module-bugfixing (format t "*debug message* Check if aligned on YZ plane...~%"))
							(if (not (= d 0))
								(setf temp-point (list (first new-point) (+ (/ (* (second new-point) (third u)) d) (/ (* (third new-point) (second u)) d)) (+ (/ (* (- 0 (second new-point)) (second u)) d) (/ (* (third new-point) (third u)) d))))
								; Else: Set temporary points to normalized points
								(setf temp-point new-point));))
				; (7) apply the inverse of step (1)
					(setf new-point (list (+ (first temp-point) (first (first axis))) (+ (second temp-point) (second (first axis))) (+ (third temp-point) (third (first axis)))))
				; (8) Return new point
					;for now: transpose back for ease of use
					;(list (first (first new-point)) (first (second new-point)) (first (third new-point)))				
					new-point)))))


		
;;; Translation		
(defun translate-by-xyz (point xyz)
	(let ((new-point (copy-list point)))
		(setf (first new-point) (+ (first new-point) (first xyz)) (second new-point) (+ (second new-point) (second xyz)) (third new-point) (+ (third new-point) (third xyz)))
		new-point))
		
(defun translate-by-x (point x)
  (translate-by-xyz point (list x 0 0)))
		
(defun translate-by-y (point y)
	(translate-by-xyz point (list 0 y 0)))
		
(defun translate-by-z (point z)
	(translate-by-xyz point (list 0 0 z)))
	


;;; Scaling
(defun rescale (point factor)
	(mapcar #'(lambda (x) (* x factor)) point))


;;; Comparing
; Testing the equalness of two point lists based on euclidean distance of each point pair.
; If point lists are not of equal size, it creates temporary dummy points at the origin for the shorter list.
; If equal, gives back 0, otherwise a measure of deviation (average euclidean distance).
; Symmetry is not considered for now.
(defun compare-objects (points1 points2)
	(if spatial-module-bugfixing (format t "*debug message* COMPARING: Obj1: ~a and Obj2: ~a~%" points1 points2))
	(let ((size (length points1)) (size2 (length points2)) (dim1 (length (first points1))) (dim2 (length (first points2))))
		(if (not (eq dim1 dim2)) (model-warning "Dimensions of objects are not identical: ~s and ~s" (length (first points1)) (length (first points2)))
			(progn
				(if (not (eq size size2))
					(progn
						(loop while (< (length points1) (length points2)) 
							do (setf points1 (append points1 (list (make-list dim1 :initial-element 0)))))
						(loop while (> (length points1) (length points2)) 
							do (setf points2 (append points2 (list (make-list dim1 :initial-element 0)))))
						(setf size (length points1) size2 (length points2))))
				(let ((distance nil) (distances nil))
					(dotimes (pair size)
						(dotimes (d (length (nth pair points1)))
							(setf distance (append distance (list (sq (- (nth d (nth pair points1)) (nth d (nth pair points2))))))))
						(setf distances (append distances (list (sqrt (apply '+ distance)))))
						(setf distance nil))
					(/ (apply '+ distances) size))))))

; Returning the degree of the angle given by the two direction vectors.
; Now rounds its input, because decimals really shouldn't matter when comparing values.
; They did for a while...always nice to see when float precision leads to sudden complex numbers:
;
; CG-USER(20): (setf dotprod (.vector normvec1 normvec2))
; -1.0000000000000002d0
; CG-USER(21): (/ (* (acos dotprod) 180) pi)
; #C(0.0d0 -1.207418274506087d-6)
; CG-USER(22): (/ (* (acos -1) 180) pi)
; 180.0000053125203d0
;

(defun compare-vector-angle (vector1 vector2)
	(if spatial-module-bugfixing (format t "*debug message* COMPARING: Vec1: ~a and Vec2: ~a~%" vector1 vector2))
	(if (equal vector1 vector2) 0
		(let* ((rvector1 (mapcar #'(lambda (x) (round-to x 3)) vector1)) (rvector2 (mapcar #'(lambda (x) (round-to x 3)) vector2))
						(line-magnitude1 (abs (sqrt (+ (sq (first rvector1)) (sq (second rvector1)) (sq (third rvector1))))))
						(line-magnitude2 (abs (sqrt (+ (sq (first rvector2)) (sq (second rvector2)) (sq (third rvector2))))))
						(normvec1 (list (/ (first rvector1) line-magnitude1) (/ (second rvector1) line-magnitude1) (/ (third rvector1) line-magnitude1)))
						(normvec2 (list (/ (first rvector2) line-magnitude2) (/ (second rvector2) line-magnitude2) (/ (third rvector2) line-magnitude2)))
						(dotprod (.vector normvec1 normvec2)))
			(/ (* (acos dotprod) 180) pi))))		
			
(defun get-vector-angle (vector)
	(let ((a (atan (sqrt (+ (sq (second vector)) (sq (third vector)))) (first vector)))
			(b (atan (sqrt (+ (sq (third vector)) (sq (first vector)))) (second vector)))
			(c (atan (sqrt (+ (sq (first vector)) (sq (second vector)))) (third vector))))
		(list (/ (* a 180) pi) (/ (* b 180) pi) (/ (* c 180) pi))))
		
		
		
		
		
		
; safety copy rotate-around-axis
; (defun rotate-around-axis (point angle axis)
	; ; convert degrees to radians
	; ;(format t "*debug message* Converting.....")
	; (setf angle (/ (* angle pi) 180))
	; ; Is axis-parameter a letter or a list?
	; (if (not (listp axis))
		; (progn
			; ;(format t "*debug message* Axis is not a list, parsing letter...~%")
			; ;transpose point if needed
			; (if (not (listp (first point))) (setf point (mapcar 'list point)))
			; (setf point (*matrix (funcall (read-from-string (concatenate 'string "rmat-" (string axis))) angle) point))
			; ;for now: transpose back for ease of use
			; (list (first (first point)) (first (second point)) (first (third point))))
		; (if (not (and (eq (length axis) 2) (eq (length (first axis)) 3))) (model-warning "Invalid line for arbitrary axis rotation: must be two points in three-dimensional space")
			; (progn
				; ;(format t "*debug message* Axis is a list, parsing x-y-z-coordinates...~%")
				; ; (1) translate space so that the rotation axis passes through the origin
				; (let* ((new-point (list (- (first point) (first (first axis))) (- (second point) (second (first axis))) (- (third point) (third (first axis))))) (temp-point '(0 0 0)) (u (list (- (first (second axis)) (first (first axis))) (- (second (second axis)) (second (first axis))) (- (third (second axis)) (third (first axis))))) (d 0) (line-magnitude (abs (sqrt (+ (sq (first u)) (sq (second u)) (sq (third u)))))))
					; ; Convert angle to radian
					; ; (setf angle (/ (* angle pi) 180))
					; ; Normalize unit vector
					; ;(format t "*debug message* Normalizing unit vector...~%")
					; (setf u (list (/ (first u) line-magnitude) (/ (second u) line-magnitude) (/ (third u) line-magnitude)))
					; ;(format t "*debug message* ...which is: ~a...~%" u)
					; ; "If rotation axis is already aligned with the z axis (XY-plane) then steps 2, 3, 5, and 6 need not be performed"
					; (if (not (= (sqrt (+ (sq (first u)) (sq (second u)))) 0))
						; (progn
				; ; (2) rotate space about the x axis so that the rotation axis lies in the xz plane
						; ; Check if line is already on YZ-plane
							; (setf d (sqrt (+ (sq (second u)) (sq (third u)))))
							; ;(format t "*debug message* d is: ~a ~%" d)
							; ;(format t "*debug message* Check if aligned on YZ plane...~%")
							; (if (not (= d 0))
								; ; Rotate axis to XZ-plane
								; (setf temp-point (list (first new-point) (- (/ (* (second new-point) (third u)) d) (/ (* (third new-point) (second u)) d)) (+ (/ (* (second new-point) (second u)) d) (/ (* (third new-point) (third u)) d))))		
								; ; Else: Set temporary points to normalized points
								; (setf temp-point new-point))
				; ; (3) rotate space about the y axis so that the rotation axis lies along the z axis
							; ;(format t "*debug message* Rotate around y-axis...~%")
							; (setf new-point (list (- (* (first temp-point) d) (* (third temp-point) (first u))) (second temp-point) (+ (* (first temp-point) (first u)) (* (third temp-point) d))))))
				; ; (4) perform the desired rotation by theta about the z axis
					; ; multiply by (rotate-around-z angle)
					; ; Right hand:
					; (setf temp-point (list (- (* (first new-point) (cos angle)) (* (second new-point) (sin angle))) (+ (* (first new-point) (sin angle)) (* (second new-point) (cos angle))) (third new-point)))
					; ; ; Left hand:
					; ; (setf temp-point (list (+ (* (first new-point) (cos angle)) (* (second new-point) (sin angle))) (+ (- (* (first new-point) (sin angle))) (* (second new-point) (cos angle))) (third new-point)))
				; ; (5) apply the inverse of step (3)
					; (if (not (= (sqrt (+ (sq (first u)) (sq (second u)))) 0))
						; (progn
							; (setf new-point (list (+ (* (first temp-point) d) (* (third temp-point) (first u))) (second temp-point) (+ (* (- 0 (first temp-point)) (first u)) (* (third temp-point) d))))
				; ; (6) apply the inverse of step (2)
							; ;(format t "*debug message* Check if aligned on YZ plane...~%")
							; (if (not (= d 0))
								; (setf temp-point (list (first new-point) (+ (/ (* (second new-point) (third u)) d) (/ (* (third new-point) (second u)) d)) (+ (/ (* (- 0 (second new-point)) (second u)) d) (/ (* (third new-point) (third u)) d))))
								; ; Else: Set temporary points to normalized points
								; (setf temp-point new-point))))
				; ; (7) apply the inverse of step (1)
					; (setf new-point (list (+ (first temp-point) (first (first axis))) (+ (second temp-point) (second (first axis))) (+ (third temp-point) (third (first axis)))))
				; ; (8) Return new point
					; ;for now: transpose back for ease of use
					; ;(list (first (first new-point)) (first (second new-point)) (first (third new-point)))				
					; new-point)))))
					
;;; old version of complexity function
; (defun complexity-function (spatial delay degree currcomplex)	; Subject to change! For now, factorial function. Maybe later change to counting the actually rotated objects per transformation (as in Shepard&Feng1972)
	; (let ((result 0))
		; (if (not (numberp degree))						; If degree is not a number, but a list of numbers, sum up the absolutes of those numbers.
			; (if (listp degree)							; If it is a list, but not of numbers, or neither number nor list, just set degree to 0.
				; (if (numberp (first degree))			; Otherwise, continue.
					; (setf degree (apply '+ (mapcar #'abs degree)))
					; (setf degree 0))
				; (setf degree 0)))
				
		; (setf degree (abs degree))						; As degree can be negative, use its absolute.
		
		; (if (spatial-module-esc spatial)
		; ;	(+ (* delay degree) (* delay (factorial currcomplex))))		; Factorial Function: 0.05 0.1 0.15 0.35000002 1.25 6.05
			; (setf result (+ (* delay degree) (* delay (sq currcomplex))))				; Square Function: 0.05 0.1 0.25 0.5 0.85 1.3
			; (setf result (* delay degree)))
			; (if spatial-module-bugfixing (format t "*debug message* COMPLEXITY PENALTY: ~a seconds, with current complexity of ~a!~%" result currcomplex))
			; ; (format t "~&Penalty is ~a, resulting from s-delay (~a) and degrees (~a)" result delay degree)
			; result
			; ))											; Delay as a complexity constant, relative to the amount of change
		; ;	(+ (* delay degree) (* delay (sum-to currcomplex))))		; Sum-To-N Function: 0.05 0.1 0.2 0.35 0.55 0.8
		; ;	delay)														; Delay as a complexity constant
																		; ; Complexity by number of concurrently transformed surfaces?
																		; ; Complexity by size of transformed point cloud?