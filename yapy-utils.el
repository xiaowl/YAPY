
(defun yy-subseq (LIST START END)
  (let ((n (if (> END (length LIST))
                (length LIST)
              END)))
    (subseq LIST START n)))
