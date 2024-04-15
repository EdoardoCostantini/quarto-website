# Project:   quarto-website
# Objective: Numeric Features from text
# Author:    Edoardo Costantini
# Created:   2024-04-15
# Modified:  2024-04-15
# Notes:

# Set up -----------------------------------------------------------------------

# Import the CountVectorizer function
from sklearn.feature_extraction.text import CountVectorizer

# Import pandas for dataframes
import pandas as pd

# Source file to specify location of
exec(open("./posts/series-NLP/Sentiment Analysis/dc-sa-nltk-resources-paths.py").read())

# Import tokenizer from nltk.tokenize
from nltk.tokenize import word_tokenize

# import random for sampling values
import random

# Detecting languages
from langdetect import detect_langs

# Create a matrix of token counts ----------------------------------------------

# Define a corpus
corpus = [
    "This is the first document.",
    "This document is the second document.",
    "And this is the third one.",
    "Is this the first document?",
]

# Initialize CountVectorizer
vectorizer = CountVectorizer()

# Fit the vectorier
vectorizer.fit(corpus)

# Transform to a sparse matrix
X = vectorizer.transform(corpus)

# Create a bow representation with panda
X_df = pd.DataFrame(X.toarray(), columns=vectorizer.get_feature_names_out())

# Print the matrix
print(X_df.head())

# Tokens length and n-grams ----------------------------------------------------

# Define a corpus
corpus = [
    "I am happy, not sad",
    "I am sad, not happy",
    "I am not happy every day",
    "I am not sad every day",
    "I am sad every day",
    "I am happy every day"
]

# Build the vectorizer, specify token sequence and fit
vect = CountVectorizer(ngram_range=(1, 2))

# Fit the vectorizer
vect.fit(corpus)

# Transform the review column
X = vect.transform(corpus)

# Create the bow representation
X_df = pd.DataFrame(X.toarray(), columns=vect.get_feature_names_out())

print(X_df.head())

# Enrich data with more columns ------------------------------------------------

# Set random seed
random.seed(10)

# Consider a simple data with reviews and score as the outcome of interest
dat = {
    "score": random.choices([0, 1], k=len(corpus)),
    "log": corpus,
}

# Make it a DataFrame
daily_log = pd.DataFrame(data = dat)

# Tokenize a string with word_tokenize function
word_tokenize(daily_log.log[1])

# Use list comprehension
word_tokens = [word_tokenize(day) for day in daily_log.log]

# Create an empty list to count the number of tokens in the reviews
len_tokens = []

# Count the number of tokens per review
for i  in range(len(word_tokens)):
    len_tokens.append(len(word_tokens[i]))

# We can use this as additional information for the prediction task
daily_log.insert(2, "n_tokens", len_tokens)

# Detecting languages ----------------------------------------------------------

# A non-english language text
italian = 'Venerdì scorso ho consegnato la tesi.'

# Detect the language
detect_langs(italian)

# Define an ambiguous string
ambiguous = "Venerdì scorso ho consegnato la tesi. It was an intense day."

# Detect the language
detected_ambi = detect_langs(ambiguous)

# Extract the first guess in a nice format
str(detected_ambi).split(":")[0][1:]
