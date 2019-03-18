.PHONY: message

message:
	@echo 'The Makefile is only used for testing purposes.'
	@echo 'Type "make help" for options.'

.PHONY: tests
tests:
	@for file in test/*.plt test/**/*.plt; do \
		echo "Running $$file" ; \
		swipl -l $$file -t "run_tests"; \
		echo "\n" ; \
	done

.PHONY: single_test
single_test:
	swipl -l test/$(n).plt -t "run_tests"

.PHONY: help
help:
	@echo 'make n=name single_test - To run all tests in "test/name.plt".'
	@echo 'make tests - To run all tests in "test/*.plt".'

test/%.plt: src/%.pl
	swipl -l $@ -t "run_tests"
