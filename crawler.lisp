(load ".quicklisp-install/require-quicklisp.lisp")

(ql:quickload :swank)
(swank:create-server :dont-close t :port 4005)
