
(defun YAPY-accept (key state)
  (cond ((null state) (list nil nil))
        ((let ((S (car state)))
           (let* ((ch (car S)))
             (if (string= key ch)
                 (list t (car-safe (cdr-safe S)))
               (YAPY-accept key (cdr state))))))))

(defun YAPY-accept-t (key state)
  (and (car (YAPY-accept key state))
       t))

(defun YAPY-pinyin-key (key codes state init-state &optional ignore)
  (let* ((S (YAPY-accept key state))
         (Y (car S))
         (s (nth 1 S)))
    (if Y
        (list s (concat codes key))
      (if (not ignore)
          (YAPY-pinyin-key key (concat codes " ") init-state init-state t)
        (list init-state codes)))))



(defun YAPY-pinyin-keyseq (keyseq state)
  (let ((codes ""))
    (reduce #'(lambda (s c)
                (let* ((k (char-to-string c))
                       (r (YAPY-pinyin-key k codes s state)))
                  (setq codes (nth 1 r))
                  (car r))) keyseq :initial-value state)
    (split-string codes)))

(defun YAPY--pinyin-score (pinyin)
  (- (* 1000 (reduce '+ (mapcar 'string-width pinyin))) (length pinyin)))

(defun YAPY-pinyin-keyseq2 (keyseq state &optional seq N)
  (if (null N)
      (setq N 0))
  (if (>= N (length keyseq))
      (car (sort seq #'(lambda (a b)
                         (> (YAPY--pinyin-score a) (YAPY--pinyin-score b)))))
    (let* ((head (subseq keyseq 0 N))
           (tail (nthcdr N keyseq))
           (py (append (YAPY-pinyin-keyseq head state)
                       (YAPY-pinyin-keyseq tail state))))
      (if (and tail
               (YAPY-accept-t (char-to-string (car tail)) state))
          (YAPY-pinyin-keyseq2 keyseq state (append seq (cons py '())) (+ 1 N))
        (YAPY-pinyin-keyseq2 keyseq state seq (+ 1 N))))))



