(defparameter *quit* t)

(defgeneric handle-input (cmd args))
(defmethod handle-input (cmd args)
  (format t "What!?~%"))

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
        ; I'm pretty sure interning user input is a terrible idea... oh well
        (handle-input (intern (string-upcase cmd-str) :keyword) args)))
    (when (not *quit*)
      (game-loop))))

(defun main ()
  (setf *quit* nil)
  (game-loop))



(defmethod handle-input ((cmd (eql :quit)) args)
  (format t "Goodbye!~%")
  (setf *quit* t))

(defmethod handle-input ((cmd (eql :look)) args)
  (multiple-value-bind (at) (parse-all-arguments args)
    (if at
        (format t "You look at the ~a~%" at)
        (format t "Look at what?~%"))))

(main)
