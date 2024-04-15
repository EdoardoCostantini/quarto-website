#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author:     Edoardo Costantini
Project:    Learning Python
Created:    2021-03-24
source:     https://docs.python.org/3/tutorial/controlflow.html
"""

# =============================================================================
# Import Modules
# =============================================================================

import pandas as pd

# =============================================================================
# While Loop
# =============================================================================
# syntax 
# while condition:
#    expression
error = 50
while error > 1: 
    error = error / 4
    print(error)

# Example
# Initialize offset
offset = 8

# Code the while loop
while offset != 0:
    print("correcting...")
    offset = offset - 1
    print(offset)

# Combine with if statement
# Initialize offset
offset = -6

# Code the while loop
while offset != 0 :
    print("correcting...")
    if offset > 0 :
      offset = offset - 1
    else : 
      offset = offset + 1
    print(offset)

# =============================================================================
# For Loop
# =============================================================================
# Syntax
# for var in seq :
#    expression
# Example from data.camp:
fam = [1.73, 1.68, 1.71, 1.89]
for height in fam :
    print(height)
    
# Add enumeration
for index, height in enumerate(fam) :
    print(str(index) + ": " + str(height))
    
# Make it change the input
for c in "family" :
    print(c.capitalize())

# Examples from source:
# (iterate over strings)
words = ['cat', 'window', 'defenestrate']
for w in words:
    print(w, len(w))

# (iterate over sequence of numbers)
for i in range(5):
    print(i)

# combined (advanced) use
a = ['Mary', 'had', 'a', 'little', 'lamb']
for i in range(len(a)):
    print(i, a[i])

# =============================================================================
# For Loop over Data Sturctures
# =============================================================================
    
    import numpy as np

    ## Dictionary: iterate over key-value pairs in a dictionary
    ##              Use the .items() method on the dictionary to define the
    ##              sequence in the for loop
    
    world = { "afg": 30.55,
             "albania": 2.77,
             "algeria": 39.21}
    # I want to print the key and value for each line
    
    # This will not work
    for key, value in world :
        print(key + " -- " + str(value))
    
    # The items() method will generate a key and value for every iteration
    world.items()
    
    for key, value in world.items():
        print(key + " -- " + str(value))
    
    ## Numpy Array: iterate over all elements in a numpy array use the np.nditer()
    ##              function to define the sequence in the for loop
    ##              for val in np.nditer(my_array)
    
    multiD = np.arange(15).reshape(3, 5)
    type(multiD)
    
    # Loop to print valyes tored in the rows
    for val in multiD:
        print(val)

# =============================================================================
# For Loops over Panda Dataframes
# =============================================================================

    brics = pd.read_csv("../data/brics.csv", index_col = 0)
    type(brics) # panda da.frames
    
    # Iterate over columns
    for j in brics:
        print(brics[j])
        # prints the columns at every iteration
        
    # Iterate over rows 
    # - Use .iterrows() method to iterate over rows instead
    help(brics.iterrows())
    # generates the label of the row and the actual data in the row
    for i in brics.iterrows():
        print(i) # label of row
        
    for i, i_cont in brics.iterrows():
        print(i) # label of row
        
    for i, i_cont in brics.iterrows():
        print(i_cont) # label of row
        
    for lab, row in brics.iterrows():
        print(lab) # label of row
        print(row) # content of row (as a panda series)
        
    # use indexing within
    for lab, row in brics.iterrows():
        print(lab + ": " + row["capital"])

    # add Column to data (ex: print number of characters in the capital name)
    help(brics.loc)
    temp_obj = brics.loc["BR", "capital"] # extract capital with .loc
    help(len)
    len(temp_obj) # return number of items in a container
    for lab, row in brics.iterrows():
        brics.loc[lab, "name_length"] = len(row["capital"])
        
    # .apply(): applies a function to a particular better approach
    brics["length"] = brics["capital"].apply(len)

# =============================================================================
# Functions
# =============================================================================
    # Defining one: Print a Fibonacci series up to n.
    def fib(n): # def must be followed by the function name and the
                # parenthesized list of formal parameters
        a, b = 0, 1
        while a < n:
            print(a, end=' ')
            a, b = b, a+b
            print()

    prova = fib(2000) # output cannot be saved!!
    # Coming from other languages, you might object that fib is not a function
    # but a procedure since it doesn’t return a value. In fact, even functions
    # without a return statement do return a value, albeit a rather boring one.
    # This value is called None (it’s a built-in name). Writing the value None
    # is normally suppressed by the interpreter if it would be the only value written.

    # Paying attention to the output
    def fib2(n):    # Print a Fibonacci series up to n
        result = []
        a, b = 0, 1
        while a < n:
            result.append(a) # calls a method of the list object result
            a, b = b, a+b
            return result

    prova = fib2(2000)

# Function Arguments
    def ask_ok(prompt,
               retries=4, # default value of 4
               reminder='Please try again!'):
        while True:
            ok = input(prompt)
            if ok in ('y', 'ye', 'yes'):
                return True
            if ok in ('n', 'no', 'nop', 'nope'):
                return False
            retries = retries - 1
            if retries < 0:
                raise ValueError('invalid user response')
            print(reminder)


    ask_ok('Do you really want to quit?')

    # the " in " keyword
    words = ["ciao", "come", "stai?"]
    saluto = "ciao"
    saluto in words

# Quirks:
# Range:
    print(range(10)) # range(10) is an iterable object
    sum(range(4))

# Multiple assignment
    a, b = 0, 1 # think of (a, b) = (0, 1)
    print(a, b)
    # Order is important: rhs valuated first, left to right
    h, r, l, = 1, h, 3 # works
    x, y, z, = 1, z, 3 # does not work

# Default values keep track of changes
    def f(a, L=[]):
        L.append(a)
        return L

    print(f(1))
    print(f(2))
    print(f(3))