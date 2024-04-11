# Project:   quarto-website
# Objective: Regex in python with re module
# Author:    Edoardo Costantini
# Created:   2024-04-10
# Modified:  2024-04-10
# Source:    https://campus.datacamp.com/courses/introduction-to-natural-language-processing-in-python/regular-expressions-word-tokenization?ex=1

# Intro ------------------------------------------------------------------------

# Import re for regular expressions
import re

# Use to find abc in a word
re.match('abc', 'abcdef')

# Define a pattern to match words
word_regex = '\w+' 

# And apply it tos ome string
re.match(word_regex, 'hi there')

# Depending on the re method, you can get different outputs
re.split('\s+', 'Split on spaces.')

# re.search() vs re.match() ----------------------------------------------------

# define a string
some_string = 'abcde'

# define a pattern to search
some_pattern = 'cd'

# Use re.match (String must start with pattern to be found!)
re.match(some_pattern, some_string)

# Use re.search
re.search(some_pattern, some_string)

# Exercise 1: Which pattern? ---------------------------------------------------

my_string = "Let's write RegEx!"
re.findall('\w+', my_string)

# Ex 2: Practicing regular expressions: re.split() and re.findall() ------------

# Consider the string
my_string = "Let's write RegEx!  Won't that be fun?  I sure think so.  Can you find 4 sentences?  Or perhaps, all 19 words?"

# Write a pattern to match sentence endings: sentence_endings
sentence_endings = r"[.?!]"

# Split my_string on sentence endings and print the result
print(re.split(sentence_endings, my_string))

# Find all capitalized words in my_string and print the result
capitalized_words = r"[A-Z]\w+"
print(re.findall(capitalized_words, my_string))

# Split my_string on spaces and print the result
spaces = r"\s+"
print(re.split(spaces, my_string))

# Find all digits in my_string and print the result
digits = r"\d+"
print(re.findall(digits, my_string))
