# Author:     Edoardo Costantini
# Project:    Learning Python
# Created:    2021-03-24

# Loading Modules --------------------------------------------------------------

import os
import pandas as pd

# Working Directory ------------------------------------------------------------

cwd = os.getcwd()
cwd

# Creating Variables -----------------------------------------------------------

# A variable is a shortcut to a data
# Rules:
# 1. Must start w/ letter
# 2. After can use numbers/letters/underscores
# 3. Case sensitive

variable1 = 3

# Basic Types ------------------------------------------------------------------

# Boolean
a = 10 > 9
a
b = True
c = False
type(c)

# Numbers
a = int(1)  # integers
b = float(1)  # floats
c = complex("1+2j")  # complex

# Strings (immutable)
file = "example.xlsx"
file[3]
file[0:3]
file[-2]
file[-0:-3]
file[:2]
file[:2] + file[7:]
file[0] = "a"  # immutable
"a" + file[1:]
len(file)  # length of string

# Lists (mutable)
squares = [1, 4, 9, 16, "ds"]
squares = [1, 4, 9, 16, 25]
squares[0]  # indexing from left to right with positive numbers
squares[-3]  # from right to left with the "-"
squares[-3:]
squares + [36, 49, 64, 81, 100]  # supports operations
squares.append(12)  # method to append something
len(squares)  # length of object

# Matrices (actually lists)
w, h = 3, 8
# dimensions of the matrix
Matrix = [[0 for x in range(w)] for y in range(h)]
Matrix
type(Matrix)

# Classes
type(a)
type(1)
type(1.0)
type(file)
type(Matrix)

# Functions --------------------------------------------------------------------

# Action turning inputs into outputs
# Function name: module.name
# Arguments:
# - Positional argument
# - Keyword argument

# Methods ----------------------------------------------------------------------

# Different functions
# Function name: object.function_name
