# main_script.R
# calling each of the scripts in sequence

# Step 1: Parse raw files
source("parse_files.R")

# Step 2: Preprocess the CSV data
source("preprocessing_csv.R")

# Step 3: Perform descriptive analysis
source("descriptive_analysis.R")

# Step 4: Perform text mining (e.g., topic modeling)
source("text_mining.R")

# Step 5: Update the food fraud dictionary
#source("update_dictionary.R")

