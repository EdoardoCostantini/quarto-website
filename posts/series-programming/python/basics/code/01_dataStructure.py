# Author:     Edoardo Costantini
# Project:    Learning Python
# Created:    2021-03-24
# source:     https://docs.python.org/3/tutorial/datastructures.html

import numpy as np
import pandas as pd

# Lists ------------------------------------------------------------------------

# Lists methods
fruits = ['orange', 'apple', 'pear', 'banana', 'kiwi', 'apple', 'banana']
type(fruits)
fruits.count('pear') # number of times appearing
fruits.index('apple') # first item whose value is equal to x
fruits.index('banana', 4)  # Find next banana 'after' 4
fruits
fruits.reverse() # permanent
fruits

# Using Lists as Stacks
stack = [3, 4, 5]
stack.append(6)
stack.append(7)
stack
stack.pop() # print/save

# List Comprehensions
squares = []
for x in range(10): # x becomes an object
    squares.append(x**2)

squares = list(map(lambda x: x**2, range(10))) 
    # x is not an object in the enviornment
squares = [x**2 for x in range(10)] 
    # alternative

# Dictionaries -----------------------------------------------------------------
# a dictionary maps a key to a value

some_dict = {'California': 38332521,
             'Texas': 26448193,
             'New York': 19651127,
             'Florida': 19552860,
             'Illinois': 12882135}
some_dict
type(some_dict)

# .keys() vs .items()
items = some_dict.items()
type(items)
for k in items:
    print(type(k))

keys = some_dict.keys()
type(keys)
for k in keys:
    print(type(k))
    
# Counters ---------------------------------------------------------------------

# Import the Counter object
from collections import Counter

# Define a list of elements
items = ["sword", "sword", "sword",
         "shield", "shield", "shield", "shield",
         "buket helmet", "buket helmet"]

# Create a Counter of the stations list: station_count
items_count = Counter(items)

# Type
type(items_count).__name__

# Compare to a dictionary w/ similar info
items_dict = {'sword': 3,
             'shield': 4,
             'buket helmet': 2}

# same use of .keys()
items_dict.keys()
items_count.keys()

# same use of .update()
items_dict.update({"potion": 8})
items_count.update({"potion": 8})

# same use of .pop()
items_dict.pop("potion")
items_count.pop("potion")

# NumPy Objects ----------------------------------------------------------------

# (1) Array
# ordered sequence of aligned one-dimensional columns
oneD = np.arange(15)
type(oneD)

multiD = np.arange(15).reshape(3, 5)
type(multiD)

# Panda Objects ----------------------------------------------------------------
# Enhanced NumPy structures
# Module for working with tabular data (row and columns)
    
# (1) Series Object
# one-dimensional array of indexed data
data = pd.Series([0.25, 0.5, 0.75, 1.0])
type(data)
data.values # access values
data.index # access index attributes
# and data can be accessed with the same indexing used in NumPy
data[0]
data[1:3]

# Essential difference:
# - the Numpy Array has an implicitly defined integer index used to 
#   access the values, 
# - the Pandas Series has an explicitly defined index associated 
#   with the values.
# This allows to work on the index:
data = pd.Series([0.25, 0.5, 0.75, 1.0],
             index=['a', 'b', 'c', 'd'])
data
data["b"]

# Specialized Dictionary
# Dictionaries in general
some_dict = {'California': 38332521,
               'Texas': 26448193,
               'New York': 19651127,
               'Florida': 19552860,
               'Illinois': 12882135}
some_dict
type(some_dict)
some_dict["California":"Texas"]

# Dictionaries as Pandas series
population = pd.Series(some_dict)
# supports slicing and other array-style operations
population["California":"Texas"] 
    
# (2) DataFrame Object
# Sequence of aligned Series objects
# Generalization of a NumPy array / specialization of a python dictionary
# Consider two series:
area_dict = {'California': 423967, 
                'Texas': 695662, 
                'New York': 141297,
                'Florida': 170312, 
                'Illinois': 149995}
area = pd.Series(area_dict)
area
# and
popu_dict = {'California': 38332521,
                'Texas': 26448193,
                'New York': 19651127,
                'Florida': 19552860,
                'Illinois': 12882135}
popu = pd.Series(popu_dict)
popu
# Combine them into a DataFrame
states = pd.DataFrame({'population': popu,
                        'area': area})
states
# Row indices
states.index
# Column indeces (names)
states.columns
# Indexing
states["population"]

# (3) Load tabular data as Pandas DataFrame
df = pd.read_csv("./data/brics.csv")
df.head()
df.info()
type(df)

# Selecting columns
df["country"] # with column names
df.country  # with column notation (not working when column 
            # anmes contain special characters)
            
# Select rows (with logical statements)
df[df.area > 8]

# Create an empty dictionary: ridership
ridership = {}

# Iterate over the entries
for date, stop, riders in entries:
    # Check to see if date is already in the ridership dictionary
    if date not in ridership:
        # Create an empty list for any missing date
        ridership[date] = []
    # Append the stop and riders as a tuple to the date keys list
    ridership[date].append((stop, riders))
    
# Print the ridership for '03/09/2016'
print(ridership['03/09/2016'])

# Import defaultdict
from collections import defaultdict

# Create a defaultdict with a default type of list: ridership
ridership = defaultdict(list)

# Iterate over the entries
for date, stop, riders in entries:
    # Use the stop as the key of ridership and append the riders to its value
    ridership[stop].append(riders)
    
# Print the first 10 items of the ridership dictionary
print(list(ridership.items())[:10])