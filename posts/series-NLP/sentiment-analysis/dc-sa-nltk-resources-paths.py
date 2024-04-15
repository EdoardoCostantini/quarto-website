# Project:   quarto-website
# Objective: Load nltk for the sentiment analysis
# Author:    Edoardo Costantini
# Created:   2024-04-15
# Modified:  2024-04-15
# Notes: 

# Load os module to define pahts
import os

# Get working directory
cwd = os.getcwd()

# Add the relative path to the NLP resources
resources_loc = cwd + "/posts/series-NLP/nltk-resources/"

# Import modules
import nltk

# Attach the resource location to the list of file paths
nltk.data.path.append(resources_loc)