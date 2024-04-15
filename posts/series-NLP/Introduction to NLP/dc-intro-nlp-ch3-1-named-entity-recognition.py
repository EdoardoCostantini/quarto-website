# Project:   quarto-website
# Objective: Named Entity Recognition
# Author:    Edoardo Costantini
# Created:   2024-04-14
# Modified:  2024-04-14
# Source:    https://campus.datacamp.com/courses/introduction-to-natural-language-processing-in-python/regular-expressions-word-tokenization?ex=1

# Set up -----------------------------------------------------------------------

# Source file with required modules and stuff
exec(open("./posts/series-NLP/dc-intro-nlp-modules.py").read())

# Lecture notes ----------------------------------------------------------------

# Define a document
sentence = """In New York, I like to ride the Metro to visit MOMA and some restaurants rated well by Ruth Reichl."""

# Tokenize
tokenized_sent = nltk.word_tokenize(sentence)

# Add tags for nouns, pronuouns, veebrs, etc.
tagged_sent = nltk.pos_tag(tokenized_sent)

# Explore tags
tagged_sent[:2]

# Return the sentence as a tree
print(nltk.ne_chunk(tagged_sent))