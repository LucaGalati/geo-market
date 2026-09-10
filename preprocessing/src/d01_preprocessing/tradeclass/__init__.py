"""Vendored copy of jktis/Trade-Classification-Algorithms (MIT License, S. Jurkatis).
Build the Cython extension once with:  python setup.py build_ext --inplace  (run inside this folder)."""
import os as _os
import sys as _sys
_sys.path.insert(0, _os.path.dirname(__file__))  # the module imports `tradeclassification_c` by bare name
from classifytrades import TradeClassification  # noqa: E402,F401
