"""Zeta Language Server and REPL integration package.

This package provides:
- A pygls-based Language Server for the Zeta Lisp dialect.
- A lightweight indexer that scans documents for top-level forms without evaluation.
- A simple TCP REPL server to evaluate code via the existing Interpreter.

Note: The LSP does not evaluate user buffers; it builds a static index from text.
"""

__all__ = [
    "server",
    "indexer",
]
