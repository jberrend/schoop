all:
	sbcl --eval "(ql:quickload :schoop)" --eval "(sb-ext:save-lisp-and-die \"schoop\" :toplevel #'schoop:start-game :executable :t)"
