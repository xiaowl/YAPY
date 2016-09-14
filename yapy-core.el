;;; yapy-core contains the essential algorithms to do pinyin index searching
;;; It's heavily depends on the structure of index.
;;; The index is just a simple list, consists of elements in below format
;;; ("[a-z]+"{1,} M N)
;;; Elements except the last 2 are pinyin code indexes, (M, N) is index range
;;; Give a dict, after indexing, the list might look like this
;;; '(("a" "ba" 0 12)
;;;   ("a" "ca" 13 24)
;;;   )
;;; Which means pinyin with first 2 codes, say "a-ba-..", "a-bai-..", "a-ban-..", "a-bang-..", "a-bao-..",
;;; is located from line 0 to line 12 in the dict.
;;; A important property of the index is that, pinyin codes are indexed using a
;;; "multiple-level" order to index, not the traditional alphabet way.
;;; For example, with the traditional way, give
;;; ai-ba
;;; an-bi
;;; ai-ca

;;; the order will be:
;;; ai-ba
;;; ai-ca
;;; an-bi

;;; Using the multiple-level way, it will be:
;;; ai-ba
;;; an-bi
;;; ai-ca

;;; Which makes fuzzy/prefix matching easy, for example, we want to match "a-b"
;;; Using the ML index way, the first match in the list will be "ai-ba", and
;;; after consuming all the data indexed by "ai-ba", we can easily forward ahead
;;; to next element an-bi, until the element failing to match "a-b"

(require 'cl)


; compare order of =a= and =b= 
; =a=: input, in form of '("xx" "xx")
; =b=: element from index
; =c=: first =c= code of an pinyin that being indexed. if =c= == 2, then "zhong-hua"
;from "zhong-hua-min-guo" will be indexed
; =w=: first =w= chars of an code will be indexed. if =w= == 2, then "zh-hu" from
;"zhong-hua" will be indexed

(defun YAPY--string-cmp (a b c w)
  (let (v)
    (dotimes (j w)
      (dotimes (i c)
        (if (not v)
            (let* ((ai (nth i a))
                   (bi (nth i b))
                   (ais (length ai))
                   (bis (length bi)))
              (if (or ai bi)
                  (cond ((>= j ais) (setq v 0)) ;; in practice, =a= is input, so can do fuzzy match
                        ((>= j bis) (setq v 1)) ;; but =b= is the target we want to match,
                        ((let ((aij (elt ai j))
                               (bij (elt bi j)))
                           (cond ((< aij bij) (setq v -1))
                                 ((> aij bij) (setq v 1)))))))))))
    (cond (v v)
          (0))))

; helper function
(defun YAPY--index-eq (codes width)
  (lambda (a b)
    (= 0 (YAPY--string-cmp a b codes width))))

; helper function
(defun YAPY--index-gt (codes width)
  (lambda (a b)
    (= 1 (YAPY--string-cmp a b codes width))))

; general bi-search function
(defun YAPY--bisearch (v seq fn-eq fn-gt)
  (let ((n (length seq))
        (bs (lambda (l h)
              (let ((m (/ (+ l h) 2)))
                (if (> l h)
                    -1
                  (cond ((funcall fn-eq v (nth m seq)) m)
                        ((funcall fn-gt v (nth m seq)) (funcall bs (+ 1 m) h))
                        ((funcall bs l (- m 1)))))))))
    (funcall bs 0 (- n 1))))

;; Called after YAPY--bisearch to find the first position in seq that
;; eval (fn-eq v (nth n seq)) -> t
;; After YAPY--bisearch, the found pinyin codes might be only a prefix-match.
;; Example:
;; Input '("a" "b") which means user input "ab", it might match:
;; - '("a" "ba")
;; - '("a" "bao")
;; - ...
;; We always to start pinyin candidates filter from the exactly first pinyin code
;; in dict.

;; This function use a exponential way to make find the first match quickly
;; From given =pos=, first try to jump forward 1 element, if fn-eq holds,
;; then 2, 4, 8... as long as it failed. Then back to the last fn-eq holds places,
;; and start from 1 again.
;; Return once fn-eq failed when jump 1.
(defun YAPY--jump-forward (v seq pos fn-eq)
  (let ((jump (lambda (n)
                (let* ((p (- pos n))
                      (e (nth p seq)))
                  (if (< p 0)
                      (setq e nil))
                  (if e
                      (if (funcall fn-eq v e)
                          (funcall jump (* n 2))
                        (if (= n 1)
                            pos
                          (YAPY--jump-forward v seq (- pos (/ n 2)) fn-eq)))
                    (if (= n 1)
                        pos
                      (YAPY--jump-forward v seq (- pos (/ n 2)) fn-eq)))))))
    (funcall jump 1)))

(defun YAPY--lookup (code index codes width)
  (let* ((fn-eq (YAPY--index-eq codes width))
         (fn-gt (YAPY--index-gt codes width))
         (pos (YAPY--bisearch code index fn-eq fn-gt)))
    (if (>= pos 0)
        (YAPY--jump-forward code index pos fn-eq)
      -1)))


(setq YAPY-run-test t)
(when YAPY-run-test
  (defmacro yp-assert-eq (a b)
    `(assert (= 0 (funcall cmp ,a ,b))))
  (defmacro yp-assert-gt (a b)
    `(assert (= 1 (funcall cmp ,a ,b))))
  (defmacro yp-assert-lt (a b)
    `(assert (= -1 (funcall cmp, a, b))))
  (defmacro yp-assert-search (a n index)
    `(let ((pos (YAPY--bisearch ,a
                                ,index
                                (lambda (v e)
                                  (= 0 (funcall cmp v e)))
                                (lambda (v e)
                                  (= 1 (funcall cmp v e))))))
       (assert (= pos ,n))))

  (defmacro yp-assert-lookup (a n index)
    `(let ((pos (YAPY--lookup ,a ,index 2 2)))
       (assert (= pos, n))))


  (let ((cmp (lambda (a b) (YAPY--string-cmp a b 2 2))))
    (yp-assert-eq '() '())
    (yp-assert-eq '("a") '("a"))
    (yp-assert-eq '("a" "b") '("a" "b"))
    ;;as we do partial match so =a= matches =ab=
    (yp-assert-eq '("a") '("ab"))
    (yp-assert-gt '("ab") '("a"))

    ;;NOTE: this test case just demonstrate the limitation
    ;;of the way of indexing, it's good at indexing:
    ;;"a b"
    ;;"ax b"
    ;;"ax bx"
    ;; but not good at "a bx"
    ;; BUT it's ok, as his match is only used to got a index
    (yp-assert-eq '("a" "ba") '("ad" "be"))
    (yp-assert-eq '("a" "c") '("ab" "c"))

    (let ((index '(("a" "b")
                   ("a" "c")
                   ("b" "c")
                   ("cc" "e")
                   ("cd" "ef")
                   ("cd" "eg")
                   ("cd" "f")
                   )))
      (yp-assert-search '("a" "b") 0 index)
      (yp-assert-search '("a" "c") 1 index)
      (yp-assert-search '("b" "c") 2 index)
      (yp-assert-search '("cc" "e") 3 index)
      (yp-assert-search '("cd" "ef") 4 index)
      (yp-assert-search '("cd" "eg") 5 index)
      (yp-assert-search '("cd" "f") 6 index)
      (yp-assert-search '("cd" "ff") -1 index)

      ;; fuzzy search is tricky, as "c d" has more than one matches
      ;; so we directly test lookup
      (yp-assert-lookup '("a" "b") 0 index)
      (yp-assert-lookup '("a" "c") 1 index)
      (yp-assert-lookup '("b" "c") 2 index)
      (yp-assert-lookup '("c" "e") 3 index)
      (yp-assert-lookup '("cd" "e") 4 index)
      (yp-assert-lookup '("cd" "ef") 4 index)
      (yp-assert-lookup '("cd" "eg") 5 index)
      (yp-assert-lookup '("c" "f") 6 index)
      (yp-assert-lookup '("cd" "ff") -1 index)
      )
    )
  )


(defun YAPY--index-pos (code)
  (YAPY--lookup code YAPY-global-index 2 2))

(defun YAPY--code-to-regexp (code)
  (concat code "[a-z]*"))

(defun YAPY--search-dict (piyin state buf N &optional P)
  (with-current-buffer buf
    (let* ((code (nth 1 state))
           (start (nth 0 code))
           (end (nth 1 code))
           (end-point end)
           (lines '())
           (result (make-hash-table :size 3)))
      (if P
          (goto-char P)
        (goto-char start))
      (let ((forward (lambda ()
                      (let* ((rxs (mapconcat 'YAPY--code-to-regexp pinyin "-"))
                             (regx (concat "^" rxs " ")))
                        (if (search-forward-regexp regx end-point 'e 1)
                            (progn
                              (add-to-list 'lines (thing-at-point 'line t))
                              (if (and (< (length lines)
                                           N)
                                       (<= (point) end-point))
                                  (funcall forward))))))))
        (funcall forward))
      (forward-line)
      (beginning-of-line)
      (puthash :point (point) result)
      (puthash :stop (>= (point) end-point) result)
      (puthash :state state result)
      (puthash :lines lines result)
      result)))

(defun YAPY--search-pinyin (pinyin &optional state)
  (let ((make-state (lambda (p)
                      (let* ((idx (nth p YAPY-global-index))
                             (scope (last idx 2)))
                        (list p scope)))))
    (if (not state)
        (let ((pos (YAPY--index-pos pinyin)))
          (if pos
              (setq state (funcall make-state pos))))
      (let ((pos (+ 1 (car state))))
        (if (and (< pos (length YAPY-global-index))
                 (= 0 (YAPY--string-cmp pinyin (nth pos YAPY-global-index) 2 2)))
            (setq state (funcall make-state pos))
          (setq state nil))))
    state))

(defun YAPY--match-pinyin (pinyin match N)
  (let ((state)
        (P)
        (stop))
    (if match
        (progn
          (setq state (gethash :state match))
          (setq P (gethash :point match))
          (setq stop (gethash :stop match))))
    (if (or stop (not state))
        (setq state (YAPY--search-pinyin pinyin state)))
    (if state
        (YAPY--search-dict pinyin state "gen_dict.txt" N P))))

(defun YAPY-match-pinyin (pinyin &optional match N)
  (if (not N)
      (setq N 5))
  (let* ((candidates)
         (M)
         (match-all (lambda (m n)
                     (let ((mch (YAPY--match-pinyin pinyin m n)))
                       (if mch
                           (progn
                             (setq M mch)
                             (setq candidates (append (gethash :lines mch) candidates))
                             (let ((cnt (length candidates)))
                               (if (< cnt n)
                                   (funcall match-all mch (- n cnt))))))
                       mch))))
    (funcall match-all match N)
    (if M
        (puthash :lines (reverse candidates) M))
    M))

(defun yp-flatten (seq)
  (cond
   ((null seq) nil)
   ((= 1 (length seq)) (car seq))
   ((append (yp-flatten (cdr seq)) (car seq)))))

(defun YAPY--make-words (match)
  (let ((lines (gethash :lines match)))
    (yp-flatten (reverse (mapcar #'(lambda (line)
                                     (cdr (split-string line)))
                                 lines)))))

(defun YAPY--pinyin (pinyin M N)
  (let ((match (YAPY-match-pinyin pinyin M N)))
    (if match
        (progn
          (puthash :words (YAPY--make-words match) match)
          match))))

(defun YAPY-pinyin (pinyin N &optional M)
  (let ((M (or M (YAPY--pinyin pinyin nil N))))
    (let* ((words (gethash :words M))
           (n (length words)))
      (if (>= n N)
          (progn
            (puthash :words (nthcdr N words) M)
            (list (yy-subseq words 0 N) M))
        (progn
          (let ((ws (gethash :words M))
                (MM (YAPY--pinyin pinyin M (- N n))))
            (if MM
                (list (append ws (yy-subseq (gethash :words MM) 0 (- N n)))
                      MM)
              (list ws nil))))))))

(defun YY-pinyin-to-words
    (pinyin N &optional C M)
  (let ((C (or C (length pinyin))))
    (let* ((match (YAPY-pinyin pinyin N M))
           (words (car match))
           (state (nth 1 match))
           (wc (length words)))
      (if (null words)
          (YY-pinyin-to-words (subseq pinyin 0 (- C 1)) (- N wc) nil)
        (list pinyin words state)))))



(defun pinyin
    (py)
  (if (< (length py) 2) py
    (nth 1 (YY-pinyin-to-words py 5))))


(defun py-tokenizer (py N)
  (if (= 1 N)
      (let (W)
        (dotimes (n (- (length py) 1) W)
          (setq W (append W (list (list (subseq py 0 (+ 1 n)) (subseq py (+ 1 n))))))
          W))
    (let (W)
      (dotimes (n (- (length py) N) W)
        (let ((C (py-tokenizer (subseq py (+ 1 n)) (- N 1))))
          (setq W (append W (mapcar #'(lambda (c)
                      (append (list (subseq py 0 (+ 1 n))) c)) C)))))
      W)))

(defun py-all (py n)
  (let ((C (py-tokenizer py n)))
    (mapcar #'(lambda (T)
                (mapcar 'pinyin T)) C)))


(defun py-forward-match-single (py end)
  (let* ((s (subseq py 0 end))
         (n (length s)))
    (if (< n 2)
        (list (pinyin s) (+ 1 end))
      (let* ((match (YAPY-pinyin s 5))
             (words (car match)))
        (if words
            (list words end)
          (py-forward-match-single py (- end 1)))))))

(defun py-backward-match-single (py start)
  (let* ((s (subseq py start))
         (n (length s)))
    (if (<= n 2)
        (list (pinyin s) start)
      (let* ((match (YAPY-pinyin s 5))
             (words (car match)))
        (if words
            (list words start)
          (py-backward-match-single py (+ start 1)))))))

(defun py-forward-match (py)
  (let* ((m (py-forward-match-single py (length py)))
         (w (car m))
         (c (nth 1 m)))
    (if (>= c (length py))
        (list w)
      (append (list w) (py-forward-match (subseq py c))))))

(defun py-backward-match (py)
  (let* ((m (py-backward-match-single py 0))
         (w (car m))
         (c (nth 1 m)))
    (if (<= c 0)
        (list w)
      (append (list w) (py-backward-match (subseq py 0 c))))))

(defun mk-pinyin (input)
  (split-string input))
(message "%s" (reverse (py-backward-match (mk-pinyin "ba yue fen wei ba"))))
