(setq-local example "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(setq-local puzzle
            (f-read-text "Day1.txt"))

(defun parse-lines (input)
  (mapcar
   (lambda (elf) (mapcar (lambda (x) (string-to-number x)) (s-split "\n" elf)))
   (s-split "\n\n" input)))

(defun solve-first-part (input)
  (seq-max
   (mapcar
    (lambda (x) (seq-reduce #'+ x 0))
    (parse-lines input))))

(eq 24000 (solve-first-part example))
(eq 68775 (solve-first-part puzzle))

(defun solve-second-part (input)
  (seq-reduce
   #'+
   (seq-take
    (seq-sort
     (lambda (x y) (> x y))
     (mapcar
      (lambda (x) (seq-reduce #'+ x 0))
      (parse-lines input)))
    3)
   0))

(eq 45000 (solve-second-part example))
(eq 202585 (solve-second-part puzzle))

