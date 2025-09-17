# mentasm.el

An Emacs package for viewing OCaml assembly output with source-to-assembly line mapping and highlighting.

## Overview

mentasm.el is an OCaml-focused assembly viewer for Emacs that displays assembly output alongside your OCaml source code. It automatically finds pre-compiled assembly files from dune build artifacts and provides bidirectional highlighting between source lines and their corresponding assembly instructions.

## Features

- **OCaml-only support** - Works with both `tuareg-mode` and `ocaml-mode`
- **Assembly file discovery** - Automatically finds `.s` files in dune `_build` directories
- **Source-to-assembly mapping** - Click or navigate in source to highlight corresponding assembly
- **Assembly-to-source mapping** - Click in assembly to highlight corresponding source lines
- **Automatic file watching** - Assembly display updates automatically when `.s` files change (e.g., after `dune build`)
- **Filter system** - Strips unnecessary assembly directives for cleaner output
- **No compilation required** - Uses existing build artifacts instead of invoking the compiler

## Installation

1. Place `mentasm.el` in your Emacs load path
2. Add to your Emacs configuration:
   ```elisp
   (require 'mentasm)
   ```

## Usage

### Basic Usage

1. Open an OCaml file (`.ml` or `.mli`) in Emacs
2. Make sure your project has been built with dune: `dune build`
3. Run `M-x mentasm` to load assembly and enable highlighting
4. A new buffer `*mentasm-output*` will show the corresponding assembly
5. Navigate in either buffer to see highlighting in the other

### Keybindings

When `mentasm-mode` is active:
- `C-c C-c` - Load/reload assembly for current buffer

### Workflow

1. Edit your OCaml source code
2. Build with `dune build` in your terminal
3. Assembly display automatically updates (thanks to file watching)
4. Navigate between source and assembly with automatic highlighting

### Stopping mentasm

To quit mentasm and clean up:
- `M-x mentasm-mode` - Toggle the mode off (recommended)
- Or kill the assembly buffer: `C-x k` in the `*mentasm-output*` buffer
- Or kill the source buffer

All methods automatically clean up file watchers and remove highlighting.

## Requirements

- Emacs with `file-notify` support (for automatic assembly reloading)
- OCaml project using dune build system
- Pre-compiled assembly files (`.s`) in `_build` directories

## Configuration

### Customization Variables

- `mentasm-automatic-recompile` - Whether to automatically reload when source changes (default: `t`)
- `mentasm-compile-delay` - Delay before auto-reloading (default: 0.4 seconds)
- `mentasm-filter-*` - Various filtering options for assembly output
- `mentasm-debug` - Enable debug messages

### Example Configuration

```elisp
(setq mentasm-automatic-recompile t)
(setq mentasm-filter-directives t)
(setq mentasm-filter-labels t)
(setq mentasm-debug nil)
```

## Project Structure

The package expects a dune project structure:
```
your-project/
├── dune-project
├── src/
│   ├── dune
│   └── your_file.ml
└── _build/
    └── default/
        └── src/
            └── your_file.s
```

## How It Works

1. **File Discovery**: Searches for `dune-project` to find project root
2. **Build Directory Search**: Locates all `_build` directories
3. **Assembly File Matching**: Finds `.s` files that correspond to your source file
4. **Line Mapping**: Parses `.loc` directives in assembly to map back to source lines
5. **File Watching**: Monitors assembly files for changes using `file-notify`

## Debugging

Enable debug messages:
```elisp
(setq mentasm-debug t)
```

Then check the `*Messages*` buffer for detailed information about file discovery and assembly processing.

## Troubleshooting

**No assembly found:**
- Ensure your project has been built: `dune build`
- Check that `.s` files exist in `_build` directories
- Verify you're in a dune project (has `dune-project` file)

**Assembly not updating:**
- Make sure file watching is supported on your system
- Check that the assembly file actually changed after building
- Try manually reloading with `M-x mentasm`

**Highlighting not working:**
- Ensure your assembly contains `.loc` directives (compiled with debug info)
- Check that file paths in assembly match your source file location

## License

This project is derived from RMSBolt and maintains its original license structure.

## Acknowledgments

Based on RMSBolt by Jay Kamat and contributors. Adapted specifically for OCaml with file discovery instead of compilation.