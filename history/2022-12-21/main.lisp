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
  (if (listp cmd)
      (loop for c in cmd
            do (input-handler c handler))
      (progn (when (some (lambda (h) (equal (cmd h) cmd)) *input-handlers*)
               (error (format nil "There is already a '~a' cmd input handler." cmd)))
             (setf *input-handlers* (cons (make-instance 'input-handler :cmd cmd :handler handler)
                                          *input-handlers*))
             nil)))

(defun handle-input (thingy cmd args)
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

;;; parses an argument from the input
;;; returns next-arg, rest
(defun parse-argument (input)
  (let ((idx (position #\Space input)))
    (if idx
        (values (subseq input 0 idx)
                (string-left-trim '(#\Space #\Tab) (subseq input (1+ idx))))
        (values input nil))))

;;; parses all arguments from the input
;;; returns multiple values
(defun parse-all-arguments (input)
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
   (pos :initarg :pos
             :initform nil
             :accessor thingy-pos)
   (vision :initarg :vision
           :initform 10
           :accessor vision)
   (blocks :initarg :blocks
            :initform nil
            :accessor blocks)
   (traits :initargs :traits
           :initform nil
           :accessor traits)))

(defun has-trait-p (thingy trait)
  (assoc trait (traits thingy)))

(defun to-pos (x y) (cons x y))
(defun pos-x (pos) (car pos))
(defun pos-y (pos) (cdr pos))
(defun pos-sub (pos1 pos2)
  (to-pos (- (pos-x pos1) (pos-x pos2))
          (- (pos-y pos1) (pos-y pos2))))
(defun pos-add (pos1 pos2)
  (to-pos (+ (pos-x pos1) (pos-x pos2))
          (+ (pos-y pos1) (pos-y pos2))))
(defun pos-mul (pos m)
  (to-pos (round (* (pos-x pos) m))
          (round (* (pos-y pos) m))))
(defun distance (pos1 pos2)
  (sqrt
   (+ (expt (- (pos-x pos1) (pos-x pos2)) 2)
      (expt (- (pos-y pos1) (pos-y pos2)) 2))))
(defun points-between (pos1 pos2)
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
  (loop for thingy in *thingies*
        if (equal (thingy-pos thingy) pos)
          collect thingy))

(defun add-thingy (thingy)
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

(defun move-to (thingy new-pos)
  (let ((pos (thingy-pos thingy)))
    (when (blocks thingy)
      (when pos
        (setf (blocked (gethash pos *map*)) nil))
      (setf (blocked (gethash new-pos *map*)) t))
    (setf (thingy-pos thingy) new-pos)))

(defun has-los-p (map thingy pos)
  (cond
    ((has-trait-p thingy :x-ray-vision) t)
    (t (not
        (loop for point in (points-between (thingy-pos thingy) pos)
              for tile = (gethash point map)
              when (not tile)
                return t)))))

(defun can-see-p (map thingy pos)
  (and (> (vision thingy)
          (distance (thingy-pos thingy) pos))
       (has-los-p map thingy pos)))

;;; returns a random number in the range (inclusive)
(defun random-range (from to)
  (+ from (random (1+ (- to from)))))

(defun random-percent ()
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

;;; returns the center of the rect
(defun rect-center (rect)
  (with-slots ((x1 x1)
               (y1 y1)
               (x2 x2)
               (y2 y2)) rect
    (to-pos (floor (/ (+ x1 x2) 2))
            (floor (/ (+ y1 y2) 2)))))

;;; turns the rect into a list of positions the rect covers
(defun rect->positions (rect)
  (loop for y from (rect-y1 rect) upto (rect-y2 rect)
        append (loop for x from (rect-x1 rect) upto (rect-x2 rect)
                     collect (to-pos x y))))

(defun rect-intersects-p (rect1 rect2)
  (some (lambda (pos)
          (some (lambda (pos2)
                  (equal pos pos2))
                (rect->positions rect2)))
        (rect->positions rect1)))

(defun spawn-monsters (map room)
  (loop for i from 1 upto (random (1+ *max-room-monsters*))
        do (let* ((roll (random-percent))
                  (monster-type (cdr (find-if (lambda (m) (>= (car m) roll)) *monster-table*)))
                  (pos (to-pos (random-range (rect-x1 room) (rect-x2 room))
                               (random-range (rect-y1 room) (rect-y2 room)))))
                  (when (not (blocked (gethash pos map)))
                    (let ((monster (make-instance monster-type)))
                      (add-thingy monster)
                      (move-to monster pos))))))

(defun dig-horizontal-tunnel (map x1 x2 y)
  (loop for x from (min x1 x2) upto (max x1 x2)
        for pos = (to-pos x y)
        do (setf (gethash pos map) (make-instance 'tile))))

(defun dig-vertical-tunnel (map y1 y2 x)
  (loop for y from (min y1 y2) upto (max y1 y2)
        for pos = (to-pos x y)
        do (setf (gethash pos map) (make-instance 'tile))))

(defun connect-rooms (map room1 room2)
  (let* ((roll (random 2))
         (center1 (rect-center (if (= 0 roll) room1 room2)))
         (center2 (rect-center (if (= 0 roll) room2 room1))))
    (dig-horizontal-tunnel map (pos-x center1) (pos-x center2) (pos-y center2))
    (dig-vertical-tunnel map (pos-y center1) (pos-y center2) (pos-x center1))))

(defun generate-map (width height player)
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
                   (move-to player (rect-center room)))
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

;(+ 6 (random (1+ (- 10 6))))
;;; main game loop
(defun game-loop (player)
  (format t "~%> ")
  (let ((input (string-trim '(#\Space #\Tab) (read-line))))
    (when (not (equal "" input))
      (multiple-value-bind (cmd-str args) (parse-argument input)
        (handle-input player cmd-str args)))
    (when (not *quit*)
      (game-loop player))))

(defun init ()
  (setf *quit* nil)
  (setf *player* (make-instance 'thingy
                                :name "player"
                                :char '@'
                                :blocks t))
  (add-thingy *player*)
  (generate-map *map-width* *map-height* *player*))

(defun main ()
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

(defun do-scan (thingy)
  (let* ((pos (thingy-pos thingy))
         (x (pos-x pos))
         (y (pos-y pos)))
    (loop for y from (- y (vision thingy)) upto (+ y (vision thingy))
          do (progn
               (loop for x from (- x (vision thingy)) upto (+ x (vision thingy))
                     do (let* ((map-pos (to-pos x y))
                               (tile (gethash map-pos *map*))
                               (map-thingy (car (thingies-at map-pos))))
                          (cond
                            ((equal pos map-pos)
                             (princ (thingy-char thingy)))
                            ((not (can-see-p *map* thingy map-pos))
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
   (do-scan thingy)))

(defun dir-name (dir)
  (case dir
    (:n "north")
    (:s "south")
    (:e "east")
    (:w "west")))

(defun dir-pos (dir from-pos)
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

(defun do-move (thingy dir)
  (let* ((pos (thingy-pos thingy))
         (new-pos (dir-pos dir pos))
         (tile (gethash new-pos *map*)))
    (cond
      ((not tile)
       (format t "You cannot move ~a.~%" (dir-name dir)))
      ((blocked tile)
       (format t "Someone is already in there.~%"))
      (t (progn (format t "You move ~a.~%" (dir-name dir))
                (move-to thingy new-pos)
                (do-scan thingy))))))

(input-handler
 '("n" "s" "e" "w")
 (lambda (thingy cmd args)
   (declare (ignore args))
   (do-move thingy (intern (string-upcase cmd) :keyword))))


(main)
