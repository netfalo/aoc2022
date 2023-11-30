;;; package --- Summary
;;; Commentary:
;;; Code:

(setq-local example "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(setq-local puzzle
            (f-read-text "Day4.txt"))


(defun parse-lines (x)
  (mapcar
     (lambda (x)
       (mapcar
        (lambda (y) (string-to-number y))
        (mapcan (lambda (y) (s-split "-" y)) (s-split "," x))))
     (seq-filter
      (lambda (y) (not (s-blank? y)))
      (s-split "\n" input))))

(defun solve-first-part (input)
  (length
   (seq-filter
    (lambda (y) (or
                 (and
                  (<= (nth 0 y) (nth 2 y))
                  (>= (nth 1 y) (nth 3 y)))
                 (and
                  (>= (nth 0 y) (nth 2 y))
                  (<= (nth 1 y) (nth 3 y)))))
    (parse-lines input))))

(defun solve-second-part (input)
  (length
   (seq-filter
    (lambda (y) (or
                 (and
                  (<= (nth 0 y) (nth 2 y))
                  (>= (nth 1 y) (nth 2 y)))
                 (and
                  (>= (nth 0 y) (nth 2 y))
                  (<= (nth 0 y) (nth 3 y)))))
    (parse-lines input))))


(eq (solve-first-part example) 2)
(eq (solve-first-part puzzle) 459)
(eq (solve-second-part example) 4)
(eq (solve-second-part puzzle) 779)

(provide 'Day4)
;;; Day4.el ends here
