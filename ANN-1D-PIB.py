from unittest import case
import PIB_model.data_generator as dg
import PIB_model.PIB_models as pib 

# set file paths 
output_file = 'dataset/training_data.csv'
training_data = 'dataset/training_data_pd.csv'

# parameter block
print(f"Press Enter to proceed with curated dataset. Press Y to generate new dataset.")
dataset_choice = input().strip().upper()
match dataset_choice:
    case 'Y':
        print(f"Enter L_max (default 1.0): ")
        L_max = float(input() or 1.0)
        print(f"Enter A (default 10.0): ")
        A = float(input() or 10.0)
        print(f"Enter N (default 1024): ")
        N = int(input() or 1024)
        print("Generating new dataset...")
        dg.run_solver(L_max, A, N, output_file)
        dg.convert_headers(output_file, training_data)
    case _:
        print("Using curated dataset.")

# load data and train model
print (f"Choose model to train: Random Forest (RF) or Neural Network (NN). Type RF/NN.")
model_choice = input().strip().upper()
match model_choice:
    case 'RF':
        print("Training Random Forest model...")
        df = pib.load_dataset(training_data)
        X_train, X_test, y_train, y_test = pib.split_dataset(df)
        rf_model, y_pred, y_test = pib.PIB_random_forest(X_train, X_test, y_train, y_test)
    case 'NN':
        print("Training Neural Network model...")
        df = pib.load_dataset(training_data)
        X_train, X_test, y_train, y_test = pib.split_dataset(df)
        nn_model, y_pred, y_test = pib.PIB_ANN(X_train, X_test, y_train, y_test)
    case _:
        print("Invalid input.")

# evaluate model
print (f"Training complete. Results:")
res = pib.PIB_evaluate_model(y_test, y_pred)
