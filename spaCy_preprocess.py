"""This code uses the Spacy library to tokenize the text, remove stop words and punctuation, and lemmatize the
tokens.The resulting preprocessed text is returned as a string."""
# import en_core_web_sm
import spacy

# Load the English language model
# spacy.cli.download("en_core_web_sm")
nlp = spacy.load('en_core_web_sm')


# Define a function to preprocess text data

def preprocess_text(text):
    # Tokenize the text
    doc = nlp(text)

    # Remove stop words and punctuation
    tokens = [token.text.lower() for token in doc if not token.is_stop and not token.is_punct]

    # Lemmatize the tokens
    lemmas = [token.lemma_ for token in nlp(" ".join(tokens))]

    # Return the preprocessed text as a string
    return " ".join(lemmas)


# Example usage
text = "This is an example sentence. It contains stop words and punctuation!"
preprocessed_text = preprocess_text(text)
print(preprocessed_text)
