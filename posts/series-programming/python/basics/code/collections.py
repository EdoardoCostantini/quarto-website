#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author:     Edoardo Costantini
Project:    Learning Python
Created:    2021-05-27
source:     https://docs.python.org/3/tutorial/datastructures.html
"""


# =============================================================================
# defaultdict vs dictionary
# =============================================================================
# Goal: show how you can intialize a dictionary with 
#       defaultdict from collections

# Consider this list

entries = [('01/01/2015', 'Austin-Forest Park', '587'),
           ('01/02/2015', 'Austin-Forest Park', '1386'),
           ('01/28/2015', 'Austin-Forest Park', '2133'),
           ('01/29/2015', 'Austin-Forest Park', '2083'),
           ('01/30/2015', 'Austin-Forest Park', '2074'),
           ('01/31/2015', 'Austin-Forest Park', '953'),
           ('01/01/2015', 'Harlem-Lake', '1106'),
           ('01/02/2015', 'Harlem-Lake', '3113'),
     ('01/03/2015', 'Harlem-Lake', '1818'),
     ('01/04/2015', 'Harlem-Lake', '1339'),
     ('01/05/2015', 'Harlem-Lake', '3287'),
     ('01/28/2015', 'Harlem-Lake', '3944'),
     ('01/29/2015', 'Harlem-Lake', '3872'),
     ('01/30/2015', 'Harlem-Lake', '4010'),
     ('01/31/2015', 'Harlem-Lake', '2311'),
     ('01/01/2015', 'Pulaski-Lake', '811'),
     ('01/02/2015', 'Pulaski-Lake', '1529'),
     ('01/30/2015', 'Pulaski-Lake', '1909'),
     ('01/31/2015', 'Pulaski-Lake', '1187'),
     ('01/01/2015', 'Quincy/Wells', '1117'),
     ('01/02/2015', 'Quincy/Wells', '4645'),
     ('01/30/2015', 'Quincy/Wells', '7593'),
     ('01/31/2015', 'Quincy/Wells', '1390'),
     ('01/01/2015', 'Davis', '1400')]

type(entries)

## Initialize with empty dictionary

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
    
ridership

## Initialize with default dictionary

# Import defaultdict
from collections import defaultdict

# Create a defaultdict with a default type of list: ridership
ridership = defaultdict(list)

# Iterate over the entries
for date, stop, riders in entries:
    # Use the stop as the key of ridership and append the riders to its value
    ridership[stop].append(riders)
    
# Print the first 10 items of the ridership dictionary
ridership