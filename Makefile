
.PHONY: install_deps test

install_deps:
	poetry install

test: install_deps
	poetry run pytest
	poetry run black --check *.py tests/