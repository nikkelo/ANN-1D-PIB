"""
ANN-1D-PIB 

Module for training and evaluating ML models on 1D particle in a box data, generated from an exact solver built in Fortran
"""

import pandas as pd
from sklearn.model_selection import train_test_split

def load_dataset(file_path):
    """
    Load the dataset from the specified CSV file.

    Args:
        file_path (str): Path to the CSV file containing the dataset.
    """

    df = pd.read_csv(file_path)
    return df

def split_dataset(df, test_size=0.2, random_state=42):
    """
    Split the dataset into training and testing sets.
    
    Args:
        df (pd.DataFrame): The complete dataset.
        test_size (float): Proportion of the dataset to include in the test split.
        random_state (int): Random seed.
    """

    X = df[["Box Length", "Coefficient 0", "Coefficient 1", "Coefficient 2", "Coefficient 3", "Coefficient 4"]]
    y = df[["Energy 0", "Energy 1", "Energy 2", "Energy 3", "Energy 4", "Energy 5", "Energy 6", "Energy 7", "Energy 8", "Energy 9"]]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.33, random_state = 42)
    return X_train, X_test, y_train, y_test

def PIB_random_forest(X_train, X_test, y_train, y_test):
    """
    Train a Random Forest Regressor on the training data and evaluate it on the test data.

    Args:
        X_train (pd.DataFrame): Training features.
        X_test (pd.DataFrame): Testing features.
        y_train (pd.DataFrame): Training targets.
        y_test (pd.DataFrame): Testing targets.
    """
    from sklearn.ensemble import RandomForestRegressor
    
    rf = RandomForestRegressor(
    n_estimators = 300,
    max_depth = None,
    random_state = 42,
    n_jobs = 1
    )

    rf.fit(X_train, y_train)
    y_pred = rf.predict(X_test)

    return rf, y_pred, y_test

def PIB_ANN(X_train, X_test, y_train, y_test):
    from sklearn.linear_model import Ridge

    ridge = Ridge(alpha=1.0)
    ridge.fit(X_train, y_train)
    y_pred = ridge.predict(X_test)

    return None

def ridge_regression(X_train, X_test, y_train, y_test):
    return None

def PIB_evaluate_model(y_test, y_pred):
    """
    Evaluate the model's performance using R^2 score.

    Args:
        y_test (pd.DataFrame): True target values.
        y_pred (np.ndarray): Predicted target values.
    """

    from sklearn.metrics import r2_score

    r2 = r2_score(y_test, y_pred)
    print(f"R^2 Score: {r2}")
    return r2

