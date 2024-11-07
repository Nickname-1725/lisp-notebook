CURRENT_IMPL := $(shell ros config show default.lisp)

build/foo-sbcl: main.lisp src/data-structure.lisp src/features.lisp src/file-io.lisp src/user-interact.lisp
	@echo "Switching from $(CURRENT_IMPL)..."
	@ros use sbcl-bin
	ros -Q package-sbcl.lisp
	@echo "Switching back to $(CURRENT_IMPL)..."
	@ros use $(CURRENT_IMPL)

build/foo-ccl: main.lisp src/data-structure.lisp src/features.lisp src/file-io.lisp src/user-interact.lisp
	@echo "Switching from $(CURRENT_IMPL)..."
	@ros use ccl-bin
	ros -Q package-ccl.lisp
	@echo "Switching back to $(CURRENT_IMPL)..."
	@ros use $(CURRENT_IMPL)

clean-trash:

