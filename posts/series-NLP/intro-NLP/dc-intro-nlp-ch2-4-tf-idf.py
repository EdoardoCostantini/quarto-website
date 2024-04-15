# Project:   quarto-website
# Objective: Term Frequency Inverse Document Frequency
# Author:    Edoardo Costantini
# Created:   2024-04-11
# Modified:  2024-04-11
# Source:    https://campus.datacamp.com/courses/introduction-to-natural-language-processing-in-python/regular-expressions-word-tokenization?ex=1

# Set up -----------------------------------------------------------------------

# Source file with required modules and stuff
exec(open("./posts/series-NLP/intro-NLP/dc-intro-nlp-modules.py").read())

# Lecture notes ----------------------------------------------------------------

# Import the Term Frequency Inverse Document Frequency function
from gensim.models import TfidfModel

# List of movie reviews
my_documents = [
    "I love space and aliens",
    "I loved it",
    "Space for space",
    "The movie was awful. I hate alien movie. I hate them so much. I cannot stand any alien. Aline is trash.",
]

# Tokenize in lower case
tokenized_docs = [word_tokenize(doc.lower()) for doc in my_documents]

# Use the dictionary function on the tokenized docs
dictionary = Dictionary(tokenized_docs)

# Create a gensim corpus (first item token id, second item number of appearances)
corpus = [dictionary.doc2bow(doc) for doc in tokenized_docs]

# Create a new TfidfModel using the corpus: tfidf
tfidf = TfidfModel(corpus)

# Calculate the tfidf weights of doc: tfidf_weights
tfidf_weights = tfidf[corpus[1]]

# Print the first five weights
print(tfidf_weights[:2])

# Practice ---------------------------------------------------------------------
