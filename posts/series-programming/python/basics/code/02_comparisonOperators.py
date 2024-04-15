#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author:     Edoardo Costantini
Project:    Learning Python
Created:    2021-03-23
"""

import numpy as np

# COMPARISON OPERATORS
# Works for numbers
2 < 3
2 == 3
3 <= 3
3 >= 3
2 != 3

# Comparison of strings
"carl" < "chris"         # works because carl comes before chris in Python
"pyscript" == "PyScript" # Capitalization matters

# Compare a boolean with an integer
False == 0
True == 1

# Vectorization
bmi = [21, 22, 20, 21, 22]
bmi > 21 # error! This is not supported

bmi = np.array([21, 22, 20, 21, 22])

bmi > 21 # great! This is not supported
bmi[bmi > 21]

# Compare Arrays Elementwise
# Create arrays
my_house = np.array([18.0, 20.0, 10.75, 9.50])
your_house = np.array([14.0, 24.0, 14.25, 9.0])

# my_house less than your_house
my_house[my_house < your_house]
print(my_house < your_house)

## Boolean Operators
# To combine comparison operators

# and: both are true
True and True
False and True
True and False
False and False

x = 12
x > 5 and x < 15

# or: at least one is true
True or True
False or True
True or False
False or False

y = 5
y < 7 or y > 13

# not: negates the bolean value you use it on
not True
not False

# In NumPy
bmi = np.array([21.21, 22.18, 20.81, 21.06, 22.15])

bmi > 21 and bmi < 22 # error!

# The array equivalents of this are:
print(bmi)
np.logical_and(bmi > 21, bmi < 22)
np.logical_or(bmi > 21, bmi < 22)
np.logical_not(bmi > 21, bmi < 22)

bmi[np.logical_and(bmi > 21, bmi < 22)] # used for selection



