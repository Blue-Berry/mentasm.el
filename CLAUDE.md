# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs Lisp package (mentasm.el) for OCaml assembly viewing. It provides assembly output mapping from OCaml source code to assembly, using existing dune build artifacts instead of compilation.

## Architecture

The package is contained in a single file `mentasm.el` that includes:

- **OCaml-Only Language Support**: Stripped down to support only OCaml
- **Assembly File Discovery**: Finds pre-compiled assembly files in dune `_build` directories
- **Assembly Parser**: Parses assembly to create mappings to original OCaml source code
- **UI Components**: Provides highlighting between source and assembly lines, overlays for matched code regions
- **Filter System**: Strips unneeded information to show only useful assembly code

## Key Components

- `mentasm-mode`: Minor mode that enables assembly viewing and code highlighting
- Assembly file discovery functions for dune projects
- Filter functions for cleaning assembly output
- Overlay system for visual source-to-assembly mapping

## OCaml File Discovery Functions

The plugin uses these functions to find assembly files instead of compiling:

- `mentasm--find-dune-project`: Finds dune-project by traversing up directory tree
- `mentasm--find-build-directories`: Locates all _build directories under project root  
- `mentasm--find-assembly-files`: Finds all .s assembly files in build directories
- `mentasm--get-source-file-components`: Extracts basename and directory components for matching
- `mentasm--match-assembly-file`: Matches source files to corresponding assembly files using scoring
- `mentasm--source-files-match-p`: Checks if target file matches assembly file path with flexible path matching

## Development Requirements

1. **Rename all functions**: Convert `rmsbolt-*` to `mentasm-*`
2. **Remove multi-language support**: Keep only OCaml-specific functionality
3. **Replace compilation**: Use file discovery instead of compiler calls
4. **Preserve core functionality**: Keep assembly parsing and UI mapping features