#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author:     Edoardo Costantini
Project:    
Created:    
"""

# Load Pandas
import pandas as pd
import csv

# =============================================================================
# Import .xls
# =============================================================================

# Directory and file name
data_dir = '../data/' # file loaction
file = 'Reportistica TIM_ATLANTIC SRL_ATL_888011538744_20B4.xlsx' # file name

# Load spreadsheet
xl = pd.ExcelFile(data_dir + file)

# Load spreadsheet in
xl = pd.ExcelFile(file)

# =============================================================================
# Import CSV
# =============================================================================

# Create a python file object in read mode for the `baby_names.csv` file: csvfile
csvfile = open("../data/brics.csv", "r")
csv_reader = csv.reader(csvfile)
next(csv_reader) # discard first line

# Create an empty dictionary
empty_dict = {}

# Loop over a csv reader on the file object
for row in csv_reader:
    # Print each row 
    print(row[2])
    # Add the rank and name to the dictionary
    empty_dict[row[0]] = row[1]
    
print(empty_dict)
