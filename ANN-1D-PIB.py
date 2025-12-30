from unittest import case
import PIB_model.data_generator as dg
import PIB_model.PIB_models as pib 

# parameter block
print(f"Enter parameters for PIB script. Press Enter for default values.")
print(f"Enter L_max (default 1.0): ")
L_max = float(input() or 1.0)
print(f"Enter A (default 10.0): ")
A = float(input() or 10.0)
print(f"Enter N (default 1000): ")
N = int(input() or 1000)

# set file paths 
output_file = 'dataset/training_data.csv'
training_data = 'dataset/training_data_nodim.csv'

# generate data
print(f"Generate new dataset with L_max={L_max}, A={A}, N={N}? Type Y/N")
newdata_choice = input().strip().upper()
match newdata_choice:
    case 'Y':
        print("Generating new dataset...")
        dg.run_solver(L_max, A, N, output_file)
        dg.convert_solved_data(output_file, training_data)
    case 'N':
        print("Using existing dataset...")
    case _:
        print("Invalid input.")

# load data and train model
print (f"Choose model to train: Random Forest (RF), Ridge Regression (RR), Neural Network (NN). Type RF/RR/NN")
model_choice = input().strip().upper()
match model_choice:
    case 'RF':
        print("Training Random Forest model...")
        df = pib.load_dataset(training_data)
        X_train, X_test, y_train, y_test = pib.split_dataset(df)
        rf_model, y_pred, y_test = pib.PIB_random_forest(X_train, X_test, y_train, y_test)
    case 'RR':
        print("Training Ridge Regression model...")
        df = pib.load_dataset(training_data)
        X_train, X_test, y_train, y_test = pib.split_dataset(df)
        krr_model, y_pred, y_test = pib.PIB_ridge_regression(X_train, X_test, y_train, y_test)
    case 'NN':
        print("Training Neural Network model...")
        df = pib.load_dataset(training_data)
        X_train, X_test, y_train, y_test = pib.split_dataset(df)
        nn_model, y_pred, y_test = pib.PIB_ANN(X_train, X_test, y_train, y_test)
    case _:
        print("Invalid input.")

# evaluate model
print (f"Training complete. Results:")
r2 = pib.PIB_evaluate_model(y_test, y_pred)
print (f"R^2 Score: {r2}")
