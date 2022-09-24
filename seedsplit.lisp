(setf *default-pathname-defaults* #P"/home/derch/seedsplit-cl/")
(load "word-list.lisp")

(defun sub-and-mod (i seedphrase key-a)
  (mod
   (- (car (nth i seedphrase))
      (car (nth i key-a)))
   2048))

(defun add-and-mod (i key-a key-b)
  (mod
   (+ (car (nth i key-a))
      (car (nth i key-b)))
   2048))

(defun get-random-word ()
  (let ((index (random 2048)))
    (cons index
          (nth index +word-list-english+))))

(defun get-random-seed (n)
  (let (seed)
    (dotimes (i n (nreverse seed))
      (push (get-random-word) seed))))

(defun calc-key-b (seedphrase key-a)
  (let (key-b)
    (dotimes (i (length seedphrase) (nreverse key-b))
      (push (let ((index (sub-and-mod i seedphrase key-a)))
              (cons index
                    (nth index +word-list-english+))
              )
            key-b))))

(defun split (seedphrase)
  (let (key-a key-b)
    (setf key-a (get-random-seed (length seedphrase)))
    (setf key-b (calc-key-b seedphrase key-a))
    (cons key-a key-b)))

(defun rebuild (key-a key-b)
  (let (seedphrase)
    (dotimes (i (length key-a) (nreverse seedphrase))
      (push (let ((index (add-and-mod i key-a key-b)))
              (cons index
                    (nth index +word-list-english+)))
            seedphrase))))
