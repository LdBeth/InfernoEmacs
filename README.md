# InfernoEmacs

A Brain F**cking Emacs Config

You can even find a Common Lisp BrainFuck interpreter
under `lsp/`!!

## Coding Style

1. Don't be afraid to use tones of syntax sugers.
2. Use macros to reduce code base and optimize byte compiled code.
3. All configs are assumed to be byte-compiled.

## Structure

### boot/
Core files that only evaluate functions. Dynamic scoped.

### lib/
Macro libraries, function libraries. Lexical binded.

### etc/
Per-package base configurations.

### lsp/
Extra lisp scripts.

### exec/
Shell scripts and other tools.

### share/
Multi media resources.
