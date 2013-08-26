;(use pox)

;; While developing, you may want to
;;   (load "scm/pox.scm")
;; instead of (use pox)

(load "scm/pox.scm")

(define credentials
  '((dbname . "pox")
    (user . "hugo")
    (password . "")
    (host . "localhost")))

(pox "/" credentials)

