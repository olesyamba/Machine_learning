import nltk
import re  # library for regular expression operations
import string  # for string operations

from nltk.corpus import stopwords  # module for stop words that come with NLTK
from nltk.stem import PorterStemmer  # module for stemming
from nltk.tokenize import TweetTokenizer  # module for tokenizing strings


# nltk.download("stopwords")  # Set of stopwords
# nltk.download('punkt')  # Split the text into number of sentences
# nltk.download('wordnet')  # Lemmatize


def nltk_preprocess(text):
    text = re.sub(r'^RT[\s]+', '', text)  # remove old style retweet text "RT"
    text = re.sub(r'https?:\/\/.*[\r\n]*', '', text)  # remove hyperlinks
    text = re.sub(r'#', '', text)  # Removing '#' hashtag
    text = re.sub(r'[^a-zA-Z]', " ", text)
    text = re.sub(r'@[A-Za-z0–9]+', '', text)  # Removing @mentions
    # Tokenize
    tokenizer = TweetTokenizer(preserve_case=False, strip_handles=True,
                               reduce_len=True)
    text = tokenizer.tokenize(text)
    stopwords_english = stopwords.words('english')  # Import the english stop words list from NLTK
    # Lemmatize
    lemmatize = nltk.WordNetLemmatizer()
    text = [lemmatize.lemmatize(word) for word in text if
            word not in stopwords_english and word not in string.punctuation]
    # Instantiate stemming class
    stemmer = PorterStemmer()
    text = [stemmer.stem(word) for word in text]
    # Combine words
    text = " ".join(text)
    return text


example = ['I'm really hungry. Give me the biggest hot-dog!', 'If you read this - Give me a star!']
preprocessed_example = [nltk_preprocess(sentence) for sentence in example]
print(preprocessed_example)

tweet = 'My beautiful sunflowers on a sunny Friday morning off :) ' \
        '#sunflowers #favourites #happy #Friday off… https://t.co/3tfYom0N1i '  # example tweet
print('Example of preprocessing tweets \n' + 'Before: ' + tweet + '\nAfter: ' + nltk_preprocess(tweet))
