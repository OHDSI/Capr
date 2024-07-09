import os

import pandas as pd

# Read concept table from Athena vocabulary download
concept_table = pd.read_csv('./project/data/vocabs_14-May-2024/CONCEPT.csv',
                            sep='\t',
                            dtype={'concept_id': object})
# Clean concept_id values
concept_table['concept_id'] = concept_table['concept_id'].apply(lambda x: x.strip().lower())

# Specify the path to the tables with the source code/terms and concept_id
tables_path = './project/data/phems_variable_list/'

# Specify path to save results of standard/non-standard lookup
save_path = './project/data/phems_variable_list/is_standard/'

# Get all tables with source code/terms and concept_id
tables = os.listdir(tables_path)
for table_name in tables:
    # Skip non-csv files
    if ".csv" not in table_name:
        continue

    # Read table of source code/terms and concept_id
    table = pd.read_csv(
        tables_path + table_name,
        dtype={'sourceCode': object, 'concept_id': object}
    # Drop empty rows
    ).dropna(how='all')
    # Clean concept_id values
    table['concept_id'] = table['concept_id'].apply(lambda x: x.strip().lower())

    # Inner join the concept table with the table of source code/terms and concept_id
    joined = concept_table.merge(table, on='concept_id', how='inner')
    # Keep only relevant columns
    res = joined.filter(items=['sourceCode', 'concept_id', 'standard_concept'])
    
    # Save the result
    res.to_csv(save_path + table_name)
