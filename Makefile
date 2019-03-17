.PHONY: message

message:
	@echo 'The Makefile is only used for testing purposes.'
	@echo 'Type "make help" for options.'

tests:
	for file in test/*.plt; do swipl -l $$file -t "run_tests"; done

single_test:
	swipl -l test/$(n).plt -t "run_tests"

help:
	@echo 'make n=name single_test - To run all tests in "test/name.plt".'
	@echo 'make tests - To run all tests in "test/*.plt".'
