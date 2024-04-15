# Project:   quarto-website
# Objective: Learning Python: Basics
# Author:    Edoardo Costantini
# Created:   2021-03-24
# Modified:  2024-04-15
# Notes:

################################################################################
# Set up
################################################################################

# Import modules
import os
import pandas as pd

# Print Working Directory
os.getcwd()

################################################################################
# Variables
################################################################################

# A variable is a shortcut to a data
variable1 = 3

# Rules:
# 1. Must start w/ letter
# 2. After can use numbers/letters/underscores
# 3. Case sensitive

# ---------------------------------------------------------------------------- #
# Basic Types
# ---------------------------------------------------------------------------- #

# Boolean
a = 10 > 9
a
b = True
c = False
type(c)

# Numbers
a = int(1)          # integers
b = float(1)        # floats
c = complex("1+2j") # complex

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

################################################################################
# Functions
################################################################################

# Action turning inputs into outputs
x = str(5)

# Function name: module.name
# Arguments:
# - Positional argument
# - Keyword argument

# ---------------------------------------------------------------------------- #
# function without inputs
# ---------------------------------------------------------------------------- #

# Define function to return square of 4
def square():
    new_value = 4**2
    print(new_value)

# Use the function
square()

# ---------------------------------------------------------------------------- #
# Function with inputs
# ---------------------------------------------------------------------------- #

# Define a function to return the square of a number
def square(value):
    new_value = value**2
    print(new_value)

# Use the function
square(3)

# ---------------------------------------------------------------------------- #
# Returning an output
# ---------------------------------------------------------------------------- #

# Function does not allow to store output right now
squared3 = square(3)

# Define a function to return the value so it can be stored
def square(value):
    new_value = value**2
    return(new_value)

# Assign the output to a variable
squared3 = square(3)

# ---------------------------------------------------------------------------- #
# Document with 'docstrings'
# ---------------------------------------------------------------------------- #

# Add a `docstrings` to clarify purpuse
def square(value):
    """Returns the square of a value"""
    new_value = value**2
    return new_value

# Check documentation
help(square)
square.__doc__

# ---------------------------------------------------------------------------- #
# Function with multiple arguments
# ---------------------------------------------------------------------------- #

# Pass multiple arguments
def raise_to_power(value1, value2):
    """Raises value1 to the power of value2"""
    new_value = value1**value2
    return new_value

# Use the function
raise_to_power(2, 3)

# ---------------------------------------------------------------------------- #
# Function with multiple outcomes with tuples
# ---------------------------------------------------------------------------- #

# Define shout with parameters word1 and word2
def shout(word1, word2):
    """Concatenate strings with three exclamation marks"""
    # Concatenate word1 with '!!!': shout1
    shout1 = word1 + "!!!"

    # Concatenate word2 with '!!!': shout2
    shout2 = word2 + "!!!"

    # Concatenate shout1 with shout2: new_shout
    new_shout = shout1 + shout2

    # Return new_shout
    return new_shout

# Pass 'congratulations' and 'you' to shout(): yell
yell = shout("congratulations", "you")

# Print yell
print(yell)

################################################################################
# Methods
################################################################################

# Different functions
# Function name: object.function_name
