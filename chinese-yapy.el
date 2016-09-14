(register-input-method "chinese-yapy" "euc-cn" 'yapy-init "YAPY")

(defun trie (root-node code)
  (if root-node
      (trie root-node)

(defun yapy-init (name &optional func)
  (interactive)
  (setq input-method-function 'yapy-input)
  (setq deactivate-current-input-method-function 'yapy-exit))

(defun yapy-input (arg)
  (message  "%s" (= ?a arg))
  (char-to-string arg))

(defun yapy-exit ()
  (message "gone"))



(trie 1 "c")


