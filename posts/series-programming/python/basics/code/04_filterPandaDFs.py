#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author:     Edoardo Costantini
Project:    Learning Python
Created:    2021-03-24
"""

import pandas as pd
import numpy as np

# =============================================================================
# Filtering pandas DataFrames
# =============================================================================

    brics = pd.read_csv("../data/brics.csv", index_col = 0)
    type(brics)
    
    # Take countries with 8 million km2
    # 1. Get column (to have a Panda series, not Pandas dataframe)
    brics["area"]
    type(brics["area"]) # it's a series!
    # or
    brics.loc[:, "area"]
    brics.iloc[:, 2]
    
    # 2. Perform Comparison
    brics["area"] > 8
    
    # 3. Use it to select
    index = brics["area"] > 8
    brics[index]
    
    # Work in 1 line as well!
    brics[brics["area"] > 8]
    
    ## Using Boolean operator
    # panda is built on numpy, so we can use the "logical_and" function!
    index = np.logical_and(brics["area"] > 8, brics["area"] < 10)
    brics[index]