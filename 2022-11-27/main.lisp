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
        (format t "What!? (type 'commands' to view all commands)~%"))))

(input-handler
 "commands"
 (lambda (thingy cmd args)
   (declare (ignore cmd)
            (ignore args)
            (ignore thingy))
   (loop for handler in *input-handlers*
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
   (blocks :initarg :blocks
            :initform nil
            :accessor blocks)))

(defun to-pos (x y) (cons x y))
(defun pos-x (pos) (car pos))
(defun pos-y (pos) (cdr pos))

(defparameter *player* nil)
(defparameter *thingies* '())

(defun thingies-at (pos)
  (loop for thingy in *thingies*
        if (equal (thingy-pos thingy) pos)
          collect thingy))

(defun add-thingy (thingy)
  (push *thingies* thingy))


;;; da world
(defclass tile ()
  ((explored :initarg :explored
             :initform nil
             :accessor explored)
   (blocked :initarg :blocked
            :initform nil
            :accessor blocked)))
(defparameter *map* nil)
(defun generate-map (width height player)
  (setq *map* (make-hash-table :test #'equal))
  ;; for now, the map is just 1 giant room
  (loop for x upto width
        do (loop for y upto height
                 do (setf (gethash (to-pos x y) *map*) (make-instance 'tile))))
  (setf (thingy-pos player) (to-pos (/ width 2) (/ height 2))))


;;; main game loop
(defun game-loop (player)
  (format t "~%> ")
  (let ((input (string-trim '(#\Space #\Tab) (read-line))))
    (when (not (equal "" input))
      (multiple-value-bind (cmd-str args) (parse-argument input)
        (handle-input player cmd-str args)))
    (when (not *quit*)
      (game-loop player))))

(defparameter *map-width* 10)
(defparameter *map-height* 10)
(defun init ()
  (setf *quit* nil)
  (setf *player* (make-instance 'thingy
                                :name "player"
                                :char '@'
                                :pos (to-pos -1 -1)
                                :blocks t))
  (generate-map *map-width* *map-height* *player*)
  (add-thingy *player*))

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
    (loop for y from (- y 5) upto (+ y 5)
          do (progn
               (loop for x from (- x 5) upto (+ x 5)
                     do (let* ((map-pos (to-pos x y))
                               (tile (gethash map-pos *map*))
                               (map-thingy (car (thingies-at map-pos))))
                          (cond
                            ((equal pos map-pos)
                             (princ (thingy-char thingy)))
                            (map-thingy
                             (princ (thingy-char map-thingy)))
                            (tile
                             (princ " "))
                            (t (princ "X")))))
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
    (if (or (not tile) (blocked tile))
        (format t "You cannot move ~a.~%" (dir-name dir))
        (progn (format t "You move ~a.~%" (dir-name dir))
               (setf (blocked (gethash pos *map*)) nil)
               (setf (blocked (gethash new-pos *map*)) t)
               (setf (thingy-pos thingy) new-pos)
               (do-scan thingy)))))

(input-handler
 '("n" "s" "e" "w")
 (lambda (thingy cmd args)
   (declare (ignore args))
   (do-move thingy (intern (string-upcase cmd) :keyword))))


;(main)
