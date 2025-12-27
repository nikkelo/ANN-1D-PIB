import PIB_model.data_generator as dg
import PIB_model.PIB_models as pib 

L_max = 1.0
A = 10.0
N = 1000
output_file = 'dataset/training_data.csv'
training_data = 'dataset/training_data_nodim.csv'

dg.run_solver(L_max, A, N, output_file)
dg.convert_solved_data(output_file, training_data)

df = pib.load_dataset(training_data)
X_train, X_test, y_train, y_test = pib.split_dataset(df)
rf_model, y_pred, y_test = pib.PIB_random_forest(X_train, X_test, y_train, y_test)

r2 = pib.PIB_evaluate_model(y_test, y_pred)