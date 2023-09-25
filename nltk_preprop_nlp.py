import nltk
import re
import pandas as pd

# nltk.download("stopwords")  # Set of stopwords
# nltk.download('punkt')  # Split the text into number of sentences
# nltk.download('wordnet')  # Lemmatize
from nltk.corpus import stopwords


def nltk_preprocess(text_data):
    new_text = []
    for i in text_data:
        # Remove and clean
        text = re.sub('[^a-zA-Z]', " ", i)
        # Tokenize
        text = nltk.word_tokenize(text, language="english")
        # Lemmatize
        lemmatize = nltk.WordNetLemmatizer()
        text = [lemmatize.lemmatize(word) for word in i]
        # Combine words
        text = "".join(text)
        new_text.append(text)
    return new_text


example = ['This is an example sentence. It contains stop words and punctuation!', 'If you read this - Give me a star!']
preprocessed_example = nltk_preprocess(example)
print(preprocessed_example)
