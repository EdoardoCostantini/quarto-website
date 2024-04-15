# Load os module to define pahts
import os

# Get working directory
cwd = os.getcwd()

# Add the relative path to the NLP resources
resources_loc = cwd + "/posts/series-NLP/nltk-resources/"

# Import modules
import re
import nltk
import pprint

# Attach the resource location to the list of file paths
nltk.data.path.append(resources_loc)

# Load tokenizers
from nltk.tokenize import sent_tokenize
from nltk.tokenize import word_tokenize
from nltk.tokenize import regexp_tokenize
from nltk.tokenize import TweetTokenizer

# Load other functions
from collections import Counter
from nltk.corpus import stopwords
from collections import defaultdict

# Import the Dictionary function from the gensim module
from gensim.corpora.dictionary import Dictionary
