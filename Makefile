.PHONY: build
build:
	cargo build --release

.PHONY: install
install:
	cargo install --path . --locked
