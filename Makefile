# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.



# Extract name and version from Cargo.toml
NAME := $(shell sed -n 's/^name = "\(.*\)"/\1/p' Cargo.toml)
VERSION := $(shell sed -n 's/^version = "\(.*\)"/\1/p' Cargo.toml)

# If on Windows, add the .exe extension to the executable
ifeq ($(OS),Windows_NT)
	EXT := .exe
else
	EXT := 
endif


# OpenBench specifies that the binary name should be changeable with the EXE parameter
ifndef EXE
	EXE := $(NAME)-$(VERSION)$(EXT)
else
	EXE := $(EXE)$(EXT)
endif



# Compile an executable for use with OpenBench
openbench:
	@echo Compiling $(EXE) for OpenBench
	cargo rustc --release --bin toad -- -C target-cpu=native --emit link=$(EXE)

# Remove the EXE created
clean:
	@echo Removing $(EXE)
	rm $(EXE)
