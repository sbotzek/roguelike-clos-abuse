;;;; A very basic roguelike, without combat!

;;; Set to true to quit
(defparameter *quit* t)


;;; Input Handling

(defclass input-handler ()
  ((cmd :initarg :cmd
        :initform (error "Please provide the cmd")
        :reader cmd)
   (handler :initarg :handler
            :initform (error "Please provide the handler function")
            :reader handler)))
(defmethod handler ((handler (eql nil)))
  nil)

(defparameter *input-handlers* '())
(defun input-handler (cmd handler)
  "Adds a new input handler for the cmd."
  (if (listp cmd)
      (loop for c in cmd
            do (input-handler c handler))
      (progn (when (some (lambda (h) (equal (cmd h) cmd)) *input-handlers*)
               (error (format nil "There is already a '~a' cmd input handler." cmd)))
             (setf *input-handlers* (cons (make-instance 'input-handler :cmd cmd :handler handler)
                                          *input-handlers*))
             nil)))

(defun handle-input (thingy cmd args)
  "Handles input from a thingy."
  (let* ((downcase-cmd (string-downcase cmd))
         (handler (loop for handler in *input-handlers*
                        when (equal downcase-cmd (cmd handler))
                          return handler))
         (handler-fun (handler handler)))
    (if handler-fun
        (funcall handler-fun thingy cmd args)
        (if (every (lambda (ch) (or (eql ch #\n) (eql ch #\s) (eql ch #\e) (eql ch #\w))) (string-downcase cmd))
            (mapcar (lambda (ch) (handle-input thingy (format nil "~a" ch) ""))
                    (coerce cmd 'list))
            (format t "What!? (type 'commands' to view all commands)~%")))))

(input-handler
 "commands"
 (lambda (thingy cmd args)
   (declare (ignore cmd)
            (ignore args)
            (ignore thingy))
   (loop for
         handler in *input-handlers*
         do (format t "~a~%" (cmd handler)))))

(defun parse-argument (input)
  "Parses an argument from the input.  Returns next-arg, rest."
  (let ((idx (position #\Space input)))
    (if idx
        (values (subseq input 0 idx)
                (string-left-trim '(#\Space #\Tab) (subseq input (1+ idx))))
        (values input nil))))

(defun parse-all-arguments (input)
  "Parses all arguments from the input.  Returns them as multiple values."
  (labels ((compute (in sofar)
             (multiple-value-bind (arg rest) (parse-argument in)
               (if arg
                   (compute rest (cons arg sofar))
                   (reverse sofar)))))
    (values-list (compute input '()))))


;;; da things
(defclass thingy ()
  ((name :initarg :name
         :initform nil
         :accessor name)
   (char :initarg :char
         :initform nil
         :accessor thingy-char)
   (map :initargs :map
        :initform nil
        :accessor thingy-map)
   (pos :initarg :pos
        :initform nil
        :accessor thingy-pos)
   (vision :initarg :vision
           :initform 10
           :accessor vision)
   (blocks :initarg :blocks
           :initform nil
           :accessor blocks)))

(defun to-pos (x y)
  "Turns an x, y pair into a position."
  (cons x y))
(defun pos-x (pos)
  "Returns the x part of a position."
  (car pos))
(defun pos-y (pos)
  "Returns the y part of a position."
  (cdr pos))
(defun pos-sub (pos1 pos2)
  "Subtracts two positions."
  (to-pos (- (pos-x pos1) (pos-x pos2))
          (- (pos-y pos1) (pos-y pos2))))
(defun pos-add (pos1 pos2)
  "Adds two positions."
  (to-pos (+ (pos-x pos1) (pos-x pos2))
          (+ (pos-y pos1) (pos-y pos2))))
(defun pos-mul (pos m)
  "Multiplies a position by a number.  The result is rounded off."
  (to-pos (round (* (pos-x pos) m))
          (round (* (pos-y pos) m))))
(defun distance (pos1 pos2)
  "Calculates the distance between two positions."
  (sqrt
   (+ (expt (- (pos-x pos1) (pos-x pos2)) 2)
      (expt (- (pos-y pos1) (pos-y pos2)) 2))))
(defun points-between (pos1 pos2)
  "Returns a list of points between two positions."
  (let ((pos-diff (pos-sub pos2 pos1))
        (last-pos nil))
    (loop for i from 1 upto 19
          for new-pos = (pos-add pos1
                                 (pos-mul pos-diff (* 0.05 i)))
          when (and (not (equal last-pos new-pos))
                    (not (equal pos1 new-pos))
                    (not (equal pos2 new-pos)))
            collect new-pos
          do (setf last-pos new-pos))))

(defparameter *player* nil)
(defparameter *thingies* '())
(defparameter *monster-table* '())

(defun thingies-at (pos)
  "Returns all thingies at the position."
  (loop for thingy in *thingies*
        if (equal (thingy-pos thingy) pos)
          collect thingy))

(defun add-thingy (thingy)
  "Adds a thingy to the list of thingies."
  (push thingy *thingies*))

(defclass goblin (thingy)
  ((name :initform "goblin")
   (char :initform "g")
   (blocks :initform t)))

(defclass orc (thingy)
  ((name :initform "orc")
   (char :initform "o")
   (blocks :initform t)))

(defclass troll (thingy)
  ((name :initform "troll")
   (char :initform "T")
   (blocks :initform t)))

(setf *monster-table* (list (cons 50 'goblin)
                            (cons 90 'orc)
                            (cons 100 'troll)))

;;; da world
(defclass tile ()
  ((explored :initarg :explored
             :initform nil
             :accessor explored)
   (blocked :initarg :blocked
            :initform nil
            :accessor blocked)))
(defparameter *map* nil)
(defparameter *map-width* 100)
(defparameter *map-height* 100)
(defparameter *max-rooms* 30)
(defparameter *max-room-monsters* 3)
(defparameter *max-room-size* 10)
(defparameter *min-room-size* 6)

(defun place-at (thingy new-map new-pos)
  "Places a thingy in a map at a position."
  (let ((old-map (thingy-map thingy))
        (pos (thingy-pos thingy)))
    (when (blocks thingy)
      (when (and pos old-map)
        (setf (blocked (gethash pos old-map)) nil))
      (setf (blocked (gethash new-pos new-map)) t))
    (setf (thingy-map thingy) new-map)
    (setf (thingy-pos thingy) new-pos)))

(defgeneric has-los-p (thingy pos)
  (:documentation "Checks to see if a thingy has line of site to the position."))
(defmethod has-los-p (thingy pos)
  "Default implementation."
  (not
   (loop for point in (points-between (thingy-pos thingy) pos)
         for tile = (gethash point (thingy-map thingy))
         when (not tile)
           return t)))

(defun can-see-p (thingy pos)
  "Checks to see if thingy can see a position."
  (and (> (vision thingy)
          (distance (thingy-pos thingy) pos))
       (has-los-p thingy pos)))

(defun random-range (from to)
  "Returns a random number in the range (inclusive)."
  (+ from (random (1+ (- to from)))))

(defun random-percent ()
  "Returns a random percent."
  (1+ (random 100)))

(defclass rect ()
  ((x1 :initarg :x1
       :reader rect-x1)
   (y1 :initarg :y1
       :reader rect-y1)
   (x2 :initarg :x2
       :reader rect-x2)
   (y2 :initarg :y2
       :reader rect-y2)))

(defun rect-center (rect)
  "Returns the center of the rect."
  (with-slots ((x1 x1)
               (y1 y1)
               (x2 x2)
               (y2 y2)) rect
    (to-pos (floor (/ (+ x1 x2) 2))
            (floor (/ (+ y1 y2) 2)))))

(defun rect->positions (rect)
  "Turns the rect into a list of positions the rect covers."
  (loop for y from (rect-y1 rect) upto (rect-y2 rect)
        append (loop for x from (rect-x1 rect) upto (rect-x2 rect)
                     collect (to-pos x y))))

(defun rect-intersects-p (rect1 rect2)
  "Returns true when rect1 and rect2 intersect eachother."
  (some (lambda (pos)
          (some (lambda (pos2)
                  (equal pos pos2))
                (rect->positions rect2)))
        (rect->positions rect1)))

(defun spawn-monsters (map room)
  "Spawns monsters in the room."
  (loop for i from 1 upto (random (1+ *max-room-monsters*))
        do (let* ((roll (random-percent))
                  (monster-type (cdr (find-if (lambda (m) (>= (car m) roll)) *monster-table*)))
                  (pos (to-pos (random-range (rect-x1 room) (rect-x2 room))
                               (random-range (rect-y1 room) (rect-y2 room)))))
             (when (not (blocked (gethash pos map)))
               (let ((monster (make-instance monster-type)))
                 (add-thingy monster)
                 (place-at monster map pos))))))

(defun dig-horizontal-tunnel (map x1 x2 y)
  "Digs a horizontal tunnel from x1 to x2 at y."
  (loop for x from (min x1 x2) upto (max x1 x2)
        for pos = (to-pos x y)
        do (setf (gethash pos map) (make-instance 'tile))))

(defun dig-vertical-tunnel (map y1 y2 x)
  "Digs a vertical tunnel from y1 to y2 at x."
  (loop for y from (min y1 y2) upto (max y1 y2)
        for pos = (to-pos x y)
        do (setf (gethash pos map) (make-instance 'tile))))

(defun connect-rooms (map room1 room2)
  "Connects room1 with room2."
  (let* ((roll (random 2))
         (center1 (rect-center (if (= 0 roll) room1 room2)))
         (center2 (rect-center (if (= 0 roll) room2 room1))))
    (dig-horizontal-tunnel map (pos-x center1) (pos-x center2) (pos-y center2))
    (dig-vertical-tunnel map (pos-y center1) (pos-y center2) (pos-x center1))))

(defun generate-map (width height player)
  "Generates a map."
  (setq *map* (make-hash-table :test #'equal))
  (let ((set-player-pos nil)
        (rooms '())
        (last-room nil))
    (loop for room-num upto (1- *max-rooms*)
          do (let* ((room-width (random-range *min-room-size* *max-room-size*))
                    (room-height (random-range *min-room-size* *max-room-size*))
                    (room-left (random (- width room-width)))
                    (room-top (random (- height room-height)))
                    (room (make-instance 'rect :x1 room-left :y1 room-top
                                               :x2 (+ room-left room-width) :y2 (+ room-top room-height))))
               (when (notany (lambda (r) (rect-intersects-p r room)) rooms)
                 ;; fill the map with this room
                 (loop for pos in (rect->positions room)
                       do (setf (gethash pos *map*) (make-instance 'tile)))
                 ;; player is set to the first room
                 (when (not set-player-pos)
                   (place-at player *map* (rect-center room)))
                 (spawn-monsters *map* room)
                 ;; connect the rooms
                 (when last-room
                   (connect-rooms *map* room last-room))
                 (setf last-room room)
                 (push room rooms))))))


(input-handler
 "generate"
 (lambda (thingy cmd args)
   (declare (ignore cmd)
            (ignore args)
            (ignore thingy))
   (generate-map *map-width* *map-height* *player*)
   (format t "Map generated.~%")))

(defun game-loop (player)
  "Main game loop."
  (format t "~%> ")
  (let ((input (string-trim '(#\Space #\Tab) (read-line))))
    (when (not (equal "" input))
      (multiple-value-bind (cmd-str args) (parse-argument input)
        (handle-input player cmd-str args)))
    (when (not *quit*)
      (game-loop player))))

(defun init ()
  "Initializes the game."
  (setf *quit* nil)
  (setf *player* (make-instance 'thingy
                                :name "player"
                                :char '@'
                                :blocks t))
  (add-thingy *player*)
  (generate-map *map-width* *map-height* *player*))

(defun main ()
  "Main function."
  (init)
  (game-loop *player*))



;;; Some basic commands.
(input-handler
 "quit"
 (lambda (thingy cmd args)
   (declare (ignore cmd)
            (ignore args)
            (ignore thingy))
   (format t "Goodbye!~%")
   (setf *quit* t)))

(input-handler
 "look"
 (lambda (thingy cmd args)
   (declare (ignore cmd)
            (ignore thingy))
   (multiple-value-bind (at) (parse-all-arguments args)
     (if at
         (format t "You look at the ~a~%" at)
         (format t "Look at what?~%")))))

(defun draw-map (thingy)
  "Draws the map for thingy."
  (let* ((map (thingy-map thingy))
         (pos (thingy-pos thingy))
         (x (pos-x pos))
         (y (pos-y pos)))
    (loop for y from (- y (vision thingy)) upto (+ y (vision thingy))
          do (progn
               (loop for x from (- x (vision thingy)) upto (+ x (vision thingy))
                     do (let* ((map-pos (to-pos x y))
                               (tile (gethash map-pos map))
                               (map-thingy (car (thingies-at map-pos))))
                          (cond
                            ((equal pos map-pos)
                             (princ (thingy-char thingy)))
                            ((not (can-see-p thingy map-pos))
                             (princ " "))
                            (map-thingy
                             (princ (thingy-char map-thingy)))
                            (tile
                             (princ "."))
                            (t (princ "#")))))
               (format t "~%")))))

(input-handler
 "scan"
 (lambda (thingy cmd args)
   (declare (ignore cmd)
            (ignore args))
   (draw-map thingy)))

(defun dir-name (dir)
  "Converts a direction to a name."
  (case dir
    (:n "north")
    (:s "south")
    (:e "east")
    (:w "west")))

(defun dir-pos (dir from-pos)
  "Returns position you'd end up at if you move the direction."
  (let* ((x (pos-x from-pos))
         (y (pos-y from-pos))
         (new-x (case dir
                  (:e (1+ x))
                  (:w (1- x))
                  (otherwise x)))
         (new-y (case dir
                  (:s (1+ y))
                  (:n (1- y))
                  (otherwise y))))
    (to-pos new-x new-y)))

(defun try-move (thingy dir)
  "Tries to move the thingy the direction."
  (let* ((pos (thingy-pos thingy))
         (new-pos (dir-pos dir pos))
         (tile (gethash new-pos (thingy-map thingy))))
    (cond
      ((not tile)
       (format t "You cannot move ~a.~%" (dir-name dir)))
      ((blocked tile)
       (format t "Someone is already in there.~%"))
      (t (progn (format t "You move ~a.~%" (dir-name dir))
                (place-at thingy (thingy-map thingy) new-pos)
                (draw-map thingy))))))

(input-handler
 '("n" "s" "e" "w")
 (lambda (thingy cmd args)
   (declare (ignore args))
   (try-move thingy (intern (string-upcase cmd) :keyword))))



;;; Traits!
(defclass trait () ())

(defun has-trait-p (thingy trait)
  "Returns true if thingy has the trait."
  (some (lambda (o) (equal o trait))
        (mapcar 'class-name (sb-mop:class-direct-superclasses (class-of thingy)))))

(defun add-trait (thingy trait)
  "Add a trait to a thingy."
  ;; Since every thingy has its own class, we can just add the trait as a superclass to
  ;; that mob's class.
  (let* ((class (class-of thingy))
         (superclasses (mapcar 'class-name (sb-mop:class-direct-superclasses class)))
         (class-name (class-name class)))
    (sb-mop:ensure-class class-name
                         :direct-superclasses (cons trait superclasses))))

(defun remove-trait (thingy trait)
  "Remove a trait from a thingy."
  (let* ((class (class-of thingy))
         (superclasses (mapcar 'class-name (sb-mop:class-direct-superclasses class)))
         (class-name (class-name class)))
    (sb-mop:ensure-class class-name
                         :direct-superclasses (remove-if (lambda (superclass) (equal superclass trait))
                                                         superclasses))))

(input-handler
 "trait"
 (lambda (thingy cmd args)
   (declare (ignore cmd))
   (if args
       (let* ((trait (intern (string-upcase (concatenate 'string "trait-" args))))
              (trait-class (find-class trait nil)))
         (cond
           ((or (not trait-class)
                (notany (lambda (c) (eq 'trait (class-name c))) (sb-mop:class-direct-superclasses trait-class)))
            (format t "Could not find trait ~a~%" args))
           ((has-trait-p thingy trait)
            (progn
              (format t "Removing trait ~a.~%" args)
              (remove-trait thingy trait)))
           (t (progn
                (format t "Adding trait ~a.~%" args)
                (add-trait thingy trait)))))
       (progn (format t "Please specify a trait.~%Possible options:~%")
              (loop for trait in (sb-mop:class-direct-subclasses (find-class 'trait))
                    for trait-name = (subseq (string (class-name trait)) (length "trait-"))
                    do (format t "~a~%" trait-name))))))

(defclass trait-x-ray-vision (trait) ())
(defmethod has-los-p ((thingy trait-x-ray-vision) pos)
  "x-ray-vision always gives you line of sight."
  t)

(main)
