#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author:     Edoardo Costantini
Project:    
Created:    
"""

# =============================================================================
# Check Python Version
# =============================================================================

import sys
print(sys.version)

# =============================================================================
# Check Packages
# =============================================================================

import pkg_resources

installed_packages = pkg_resources.working_set
sorted(["%s==%s" % (i.key, i.version) for i in installed_packages])

# =============================================================================
# Installing Packages
# =============================================================================
