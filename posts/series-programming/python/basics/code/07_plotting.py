#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author:     Edoardo Costantini
Project:    Learning Python - Plotting
Created:    2021-04-21
"""

from matplotlib import pyplot as plt
from pydataset import data

# =============================================================================
# The Line Plot w/ pyplot.plot function
# =============================================================================
# Syntax
# plt.plot( x_values = data_farme_name.column_name, y_values )
# plt.show() # to communicate the plot is over

# Load Iris data (https://en.wikipedia.org/wiki/Iris_flower_data_set)
mtcars = data("mtcars")
type(mtcars)

# Decide on plot style
plt.style.use("ggplot")

# Plot Officer Deshaun's hours_worked vs. day_of_week
plt.plot(mtcars.mpg, mtcars.gear,
         label = "Gear",
         linestyle = ":")
plt.plot(mtcars.mpg, mtcars.carb,
         label = "Carb",
         color = "DarkCyan")

# Improve readability
plt.xlabel("Miles per Gallon") # add labes for X axis
plt.ylabel("Accelaration") # add labels for Y axis
plt.title("My Cool Plot", fontsize = 20)  # add title

# Legends
plt.legend()

# Quick notes
plt.text(10, 2, "(x=10, y=2)")

# Display Deshaun's plot
plt.show()
