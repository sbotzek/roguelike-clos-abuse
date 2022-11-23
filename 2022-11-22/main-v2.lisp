(defparameter *quit* t)

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
(defun add-input-handler (cmd handler)
  (when (some (lambda (h) (equal (cmd h) cmd)) *input-handlers*)
    (error (format nil "There is already a '~a' cmd input handler." cmd)))
  (setf *input-handlers* (cons (make-instance 'input-handler :cmd cmd :handler handler)
                               *input-handlers*))
  nil)
(defun handle-input (cmd args)
  (let* ((downcase-cmd (string-downcase cmd))
         (handler (loop for handler in *input-handlers*
                       when (equal downcase-cmd (cmd handler))
                         return handler))
         (handler-fun (handler handler)))
    (if handler-fun
        (funcall handler-fun cmd args)
        (format t "What!?~%"))))

; parses an argument from the input
; returns next-arg, rest
(defun parse-argument (input)
  (let ((idx (position #\Space input)))
    (if idx
        (values (subseq input 0 idx)
                (string-left-trim '(#\Space #\Tab) (subseq input (1+ idx))))
        (values input nil))))

; parses all arguments from the input
; returns multiple values
(defun parse-all-arguments (input)
  (labels ((compute (in sofar)
             (multiple-value-bind (arg rest) (parse-argument in)
               (if arg
                   (compute rest (cons arg sofar))
                   (reverse sofar)))))
    (values-list (compute input '()))))

; main game loop
(defun game-loop ()
  (format t "~%> ")
  (let ((input (string-trim '(#\Space #\Tab) (read-line))))
    (when (not (equal "" input))
      (multiple-value-bind (cmd-str args) (parse-argument input)
        (handle-input cmd-str args)))
    (when (not *quit*)
      (game-loop))))

(defun main ()
  (setf *quit* nil)
  (game-loop))



(add-input-handler
 "quit"
 (lambda (cmd args)
   (declare (ignore cmd)
            (ignore args))
   (format t "Goodbye!~%")
   (setf *quit* t)))

(add-input-handler
 "look"
 (lambda (cmd args)
   (declare (ignore cmd))
   (multiple-value-bind (at) (parse-all-arguments args)
    (if at
        (format t "You look at the ~a~%" at)
        (format t "Look at what?~%")))))

(main)
