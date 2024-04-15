# Project:   quarto-website
# Objective: String tokenization
# Author:    Edoardo Costantini
# Created:   2024-04-10
# Modified:  2024-04-10
# Notes:

# Set up -----------------------------------------------------------------------

# Source file with required modules and stuff
exec(open("./posts/series-NLP/intro-NLP/dc-intro-nlp-modules.py").read())

# Import modules
import re
import nltk

# Attach the resource location to the list of file paths
nltk.data.path.append(resources_loc)

# Load the required functions from nltk.tokenize
from nltk.tokenize import sent_tokenize
from nltk.tokenize import word_tokenize
from nltk.tokenize import regexp_tokenize
from nltk.tokenize import TweetTokenizer

# Lecture notes ----------------------------------------------------------------

# Pattern for digits or words
pattern_or = ('(\d+|\w+)') 

# Find all
re.findall(pattern_or, 'He has 11 cats.')

# New string
my_str = "match lowercase spaces nums like 12, but no commas"

# Stops after comma
re.match("[a-z0-9 ]+", my_str)

# Practice ---------------------------------------------------------------------

# A string
my_string = "SOLDIER #1: Found them? In Mercea? The coconut's tropical!"

# Some patterns
pattern1 = r"(\w+|\?|!)"
pattern2 = r"(\w+|#\d|\?|!)"
pattern3 = r"(#\d\w+\?!)"
pattern4 = r"\s+"

# Check the patterns
regexp_tokenize(my_string, pattern1)
regexp_tokenize(my_string, pattern2)
regexp_tokenize(my_string, pattern3)
regexp_tokenize(my_string, pattern4)

# Make some tweets as input list
tweets = [
    "This is the best #nlp exercise ive found online! #python",
    "#NLP is super fun! <3 #learning",
    "Thanks @datacamp :) #nlp #python",
]

# Define a regex pattern to find hashtags: pattern1
pattern1 = r"#\w+"

# Use the pattern on the first tweet in the tweets list
hashtags = regexp_tokenize(tweets[0], pattern1)
print(hashtags)

# Write a pattern that matches both mentions (@) and hashtags
pattern2 = r"(#\w+|@\w+)"

# Use the pattern on the last tweet in the tweets list
mentions_hashtags = regexp_tokenize(tweets[-1], pattern2)
print(mentions_hashtags)

# Use tokenize from the tweet tokenizer on the third tweet
TweetTokenizer().tokenize(tweets[2])

# You could have done the same with patterns of your choice.. but would have been more work!
regexp_tokenize(tweets[2], r"(#\w+|@\w+)")

# Use the TweetTokenizer to tokenize all tweets into one list
all_tokens = [TweetTokenizer().tokenize(t) for t in tweets]

# And print the list
print(all_tokens)

# Define some german text with special characters
german_text = "Wann gehen wir Pizza essen? 🍕 Und fährst du mit Über? 🚕"

# Tokenize and print all words in german_text
all_words = word_tokenize(german_text)
print(all_words)

# Tokenize and print only capital words
capital_words = r"[A-ZÜ]\w+"
print(regexp_tokenize(german_text, capital_words))

# Tokenize and print only emoji
emoji = "['\U0001F300-\U0001F5FF'|'\U0001F600-\U0001F64F'|'\U0001F680-\U0001F6FF'|'\u2600-\u26FF\u2700-\u27BF']"
print(regexp_tokenize(german_text, emoji))
