# Project:   quarto-website
# Objective: Simple Text Processing
# Author:    Edoardo Costantini
# Created:   2024-04-11
# Modified:  2024-04-11
# Source:    https://campus.datacamp.com/courses/introduction-to-natural-language-processing-in-python/regular-expressions-word-tokenization?ex=1

# Set up -----------------------------------------------------------------------

# Source file with required modules and stuff
exec(open("./posts/series-NLP/dc-intro-nlp-modules.py").read())

# Lecture notes ----------------------------------------------------------------

# Import
from nltk.corpus import stopwords
from collections import defaultdict
from itertools import chain

# Import the Dictionary function from the gensim module
from gensim.corpora.dictionary import Dictionary

# List of movie reviews
my_documents = [
    "I love space and aliens",
    "I loved it",
    "Space for space",
    # "Awesome action scenes, but boring characters."
    "The movie was awful. I hate alien movie. I hate them so much. I cannot stand any alien. Aline is trash.",
    # "Space is cool! I liked the movie.",
    # "More space films, please!",
]

# Tokenize in lower case
tokenized_docs = [word_tokenize(doc.lower()) for doc in my_documents]

# Use the dictionary function on the tokenized docs
dictionary = Dictionary(tokenized_docs)

# Show token and their ids in the baby corpus we created
dictionary.token2id

# Create a gensim corpus (first item token id, second item number of appearances)
corpus = [dictionary.doc2bow(doc) for doc in tokenized_docs]

# Prints the nicely formatted dictionary
pprint.pprint(dictionary.token2id)
pprint.pprint(corpus)

# Select a target id
dictionary.token2id.get("love")

# Sort the doc for frequency: bow_doc
bow_doc = sorted(corpus[3], key=lambda w: w[1], reverse=True)

# Print the top 5 words of the document alongside the count
for word_id, word_count in bow_doc[:5]:
    print(dictionary.get(word_id), word_count)