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

import numpy as np
import matplotlib.pyplot as plt # to plot random walk

# =============================================================================
# Random Number Generators
# =============================================================================
# Set seed
np.random.seed(1234)

# Perform random number generation
np.random.rand()

# Simulate a coin toss
np.random.seed(123)
np.random.randint(low = 0,
                  high = 2)

# =============================================================================
# Populate a list with a for loop
# =============================================================================
# Set seed
np.random.seed(1234)

# Generate Empty list
outcome = []
type(outcome)

# Simulate a coin toss
for x in range(10) : 
    coin = np.random.randint(0, 2) # coin flip
    if coin == 0 : 
        outcome.append("heads")
    else : 
        outcome.append("tails")
print(outcome)

# Make it a random walk
tails = [0]
type(tails)
for x in range(10) : 
    coin = np.random.randint(0, 2)
    tails.append(tails[x] + coin)
print(tails)
    # the number reported increment by 1 every time that a 
    # tail is drawn 
    
# =============================================================================
# Build a Random Walk
# =============================================================================
# Initialization
random_walk = [0]

for x in range(100) :
    step = random_walk[-1]
    dice = np.random.randint(1,7)

    if dice <= 2:
        step = max(0, step - 1)
    elif dice <= 5:
        step = step + 1
    else:
        step = step + np.random.randint(1,7)

    random_walk.append(step)

# Import matplotlib.pyplot as plt


# Plot random_walk
plt.plot(random_walk)

# Show the plot
plt.show()

# =============================================================================
# Create a Disitrbution of random walk results
# =============================================================================

# initialize and populate all_walks
all_walks = []
for i in range(500) :
    random_walk = [0]
    for x in range(100) :
        step = random_walk[-1]
        dice = np.random.randint(1,7)
        if dice <= 2:
            step = max(0, step - 1)
        elif dice <= 5:
            step = step + 1
        else:
            step = step + np.random.randint(1,7)
        random_walk.append(step)
    all_walks.append(random_walk)

# Convert all_walks to Numpy array: np_aw
np_aw = np.array(all_walks)
type(all_walks)
all_walks
type(np_aw)
np_aw

# Plot np_aw and show
plt.plot(np_aw)
plt.show()

# Transpose np_aw: np_aw_t
np_aw_t = np.transpose(np_aw)

# Plot np_aw_t and show
plt.plot(np_aw_t)
plt.show()

# Plot the disitrbution of the final steps
# Select last row from np_aw_t: ends
ends = np_aw_t[-1, :]

# Plot histogram of ends, display plot
plt.hist(ends)
plt.show()

# Compute chances
sum(ends > 60)/len(ends)




