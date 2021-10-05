from sklearn.preprocessing import LabelEncoder
from xgboost.sklearn import XGBRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
import category_encoders as ce

import pandas as pd
import numpy as np


def read_file(file_name, columns=None, header='infer', names=None):
    print('Reading {}...'.format(file_name))

    file = pd.read_csv(file_name,
                       sep=";",
                       usecols=columns,
                       header=header,
                       names=names)

    print('Extracted {number_of_lines} lines!'.format(
        number_of_lines=len(file)))
    print('First 10 lines of the file:\n ', file[:10])
    print('--------')

    return file


columns_to_use = [
    'product_name',
    'weekday',
    'month',
    'qty_lag1',
    'qty_lag2',
    'qty_lag3',
    'qty_lag4',
    'qty_lag5',
    'qty_lag6',
    'qty_lag7',
    'qty_lag8',
    'qty_lag9',
    'qty_lag10',
    'qty_lag11',
    'qty_lag12',
    'qty_lag13',
    'qty_lag14',
    'avg_discount_mean_value_lag1',
    'avg_discount_count_lag1',
    'avg_from_blik_lag1',
    'avg_from_paypass_lag1',
    'avg_from_payu_lag1',
    'avg_total_lag1',
    'avg_total_to_discount_lag1',
    'avg_total_base_lag1',
    'avg_sum_fv_lag1',
    'avg_transaction_discount_count_lag1',
    'days_since_prev_delivery',
    'sales_since_prev_delivery',
    'available_products',
    'productTotalSalesLastPeriod_lag1',
    'numberOfPOSwithSales_lag1',
    'salesRelativeToGlobalProductAverage_lag1',
    'posTotalSalesLastPeriod_lag1',
    'numberOfProductsWithSales_lag1',
    'salesRelativeToGlobalPoSAverage_lag1',
    'prods_avail_in_cat_590053bdc5c79d3575eb44f6',
    'prods_avail_in_cat_59005cd6c5c79d3575eb450d',
    'prods_avail_in_cat_591301913dd75608a9c2ef19',
    'prods_avail_in_cat_591301b53dd75608a9c2ef1a',
    'prods_avail_in_cat_591301c83dd75608a9c2ef1b',
    'prods_avail_in_cat_5a0033206cdc0d08a6591bfb',
    'prods_avail_in_cat_5a6f110ca0899f5ca2f7d6e9',
    'prods_avail_in_cat_5abe0aed049e180557e22330',
    'prods_avail_in_cat_5cb9b8eedf68013fb09db8f0',
    'prods_avail_in_cat_5cd1a4d32b10792bc08dab31',
    'prods_avail_in_cat_5cd1a4f40a544c2d0d156fea',
    'prods_avail_in_cat_5d1b55aa5379175d45e9360a',
    'prods_avail_in_cat_5d7103b8c8c4a843bc5b5706',
    'prods_avail_in_cat_5d9f1dcab962ef075bd26ecb',
    'prods_avail_in_cat_5e54ca889dca612f7a92cab9',
    'prods_avail_in_cat_none'
]
ordinal_columns = ['weekday']
nominal_columns = ['product_name']


training_set_file_name = '../files/SUS_project_training_data.csv'
testing_set_file_name = '../files/SUS_project_test_data.csv'
training_targets_file_name = '../files/training_targets.txt'

print('Reading files...')
training_set_X = read_file(training_set_file_name, columns=columns_to_use)
testing_set_X = read_file(testing_set_file_name, columns=columns_to_use)
training_set_size = len(training_set_X)
testing_set_size = len(testing_set_X)

training_set_y = read_file(training_targets_file_name, columns=[0], header=None, names=["sale"])
print('Reading files done!')

print('Encoding features...')

set_X = pd.concat([training_set_X, testing_set_X])
set_X[ordinal_columns] = set_X[ordinal_columns].fillna('None')
set_X[nominal_columns] = set_X[nominal_columns].fillna('None')
set_X = set_X.fillna(0)

print(set_X)

encoder = ce.OrdinalEncoder(cols=ordinal_columns)
set_X = encoder.fit_transform(set_X)


for column_to_encode in nominal_columns:
    encoder = LabelEncoder()
    set_X[column_to_encode] = encoder.fit_transform(set_X[column_to_encode])

set_X = pd.get_dummies(set_X, columns=nominal_columns, drop_first=True, sparse=True)
print(set_X)

training_set_X = set_X.head(training_set_size)
testing_set_X = set_X.tail(testing_set_size)
print('Encoding features done!')

print('Training model...')
model = XGBRegressor(
    objective='reg:squarederror',
    colsample_bytree=0.6,
    learning_rate=0.3,
    max_depth=3,
    min_child_weight=11,
    n_estimators=40,
    subsample=0.6)

# splitting into train and eval sets
train_X, eval_X, train_y, eval_y = train_test_split(
    training_set_X, training_set_y,
    test_size=0.25,
    random_state=1237)

model.fit(train_X, train_y,
          early_stopping_rounds=5,
          eval_set=[(eval_X, eval_y)],
          verbose=True)
print('Training model done!')
eval_prediction = model.predict(eval_X)
print("Score: {}\n".format(r2_score(eval_y, eval_prediction)))

print('Predicting...')
prediction = model.predict(testing_set_X)
print('Predicting done!')

print(list(filter(lambda a: a < 0, prediction)))

prediction = np.maximum(np.zeros(len(prediction)), prediction)
output_file = "out.csv"
print("Saving predictions to {}".format(output_file))
np.savetxt(output_file, prediction, delimiter="\n")
