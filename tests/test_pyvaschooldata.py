"""
Tests for pyvaschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pyvaschooldata
    assert pyvaschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pyvaschooldata
    assert hasattr(pyvaschooldata, 'fetch_enr')
    assert callable(pyvaschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pyvaschooldata
    assert hasattr(pyvaschooldata, 'get_available_years')
    assert callable(pyvaschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pyvaschooldata
    assert hasattr(pyvaschooldata, '__version__')
    assert isinstance(pyvaschooldata.__version__, str)
