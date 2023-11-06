(load "main.lisp")
(ext:saveinitmem #p"./build/foo" :init-function 
                 (lambda () (init-fun) (ext:quit))
                 :executable t :quiet t :norc t)
