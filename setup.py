# setup.py
from setuptools import setup, Extension
from Cython.Build import cythonize
import os

# Full path to the pyx
pyx_path = os.path.join("zeta", "compiler", "vm_cy.pyx")

setup(
    name="zeta",
    ext_modules=cythonize(
        Extension(
            name="zeta.compiler.vm_cy",  # module path for import
            sources=[pyx_path],
        ),
        compiler_directives={'language_level': "3", "boundscheck": False, "wraparound": False}
    ),
    zip_safe=False,
)
