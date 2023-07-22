import pandas as pd
import numpy as np

# read the CSV file
data = pd.read_csv('IMDB Dataset.csv')

# make the header x and y
header = ['x', 'y']

# select the first 5000 records for training
train = data[:5000]

# randomly select 1000 records for validation
validation = data.sample(n=1000, random_state=1)

# randomly select another 1000 records for testing
test = data.sample(n=1000, random_state=2)

# save the train, validation, and test sets to separate CSV files with header
train.to_csv('train/train.csv', index=False, header=header)
validation.to_csv('validation/validation.csv', index=False, header=header)
test.to_csv('test/test.csv', index=False, header=header)
