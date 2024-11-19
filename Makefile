ifeq (pcc,$(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(RUN_ARGS):;@:)
endif

.PHONY: pcc test.c

pcc: build
	@./target/release/pcc $(RUN_ARGS)

build:
	@cargo build --release -q

