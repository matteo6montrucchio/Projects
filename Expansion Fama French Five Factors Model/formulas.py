import pandas as pd
import numpy as np
import seaborn as sns
from cvxpy.atoms.affine.index import index
from pandas import read_csv
import sklearn
from sklearn.decomposition import PCA
from sklearn.linear_model import LinearRegression, LassoCV
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score, mean_squared_error
import statsmodels.api as sm
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt





def load_data(path): #to load data and transform it in csv

    df = pd.read_csv(path, index_col=0, dtype={0:"int64"})
    df.rename_axis("date", inplace=True)
    df.index = pd.to_datetime(df.index.astype(str), format="%Y%m%d")

    df = df[~df.index.weekday.isin([5, 6])] #remove weekends

    return df

def clean_csv(path): #handle extra spaces in mom csv
    with open(path, 'r') as file:
        lines = file.readlines()

    # Remove extra spaces after commas (both for negative and positive values)
    cleaned_lines = [line.replace(' ,', ',').replace(', ', ',').replace('   ', ' ').replace('  ', ' ') for line in lines]

    # Save the cleaned data to a new CSV file
    cleaned_path = "cleaned_" + path.split('/')[-1]  # Save the cleaned file with a new name
    with open(cleaned_path, 'w') as file:
        file.writelines(cleaned_lines)

    return cleaned_path

def load_data_web(path):

    df = pd.read_csv(path, index_col=0, dtype={0: "object"})
    df.rename_axis("date", inplace=True)
    df.index = pd.to_datetime(df.index, format="%Y%m%d", errors='coerce')

    df.sort_index(ascending=False, inplace=True)

    df /= 100

    return df

def load_data_indu(path):

    df = pd.read_csv(path, index_col=0, dtype=str, low_memory=False)
    df.rename_axis("date", inplace=True)
    df.index = pd.to_datetime(df.index, format="%Y%m%d", errors='coerce')
    df = df.apply(pd.to_numeric, errors='coerce')
    df = df[~df.index.duplicated(keep="first")]
    df.fillna(df.mean(), inplace=True)
    df.sort_index(ascending=False, inplace=True)
    df /= 100

    df.rename(columns=lambda x: f"Industry_{x.strip()}", inplace=True)

    pca = PCA(n_components = 5)
    indu_factors = pca.fit_transform(df)

    df_pca = pd.DataFrame(indu_factors, index=df.index, columns=[f"Industry_Factor_{i+1}" for i in range(5)])

    return df_pca


def regression(dataset, target):
    # Splitting dataset
    X = dataset.drop(columns=[target])
    y = dataset[target]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # Standardizing features
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    # Linear Regression Model
    model = LinearRegression()
    model.fit(X_train_scaled, y_train)
    y_pred = model.predict(X_test_scaled)

    # Performance metrics
    r2 = r2_score(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    print(f"R^2 Score: {r2:.4f}")
    print(f"Mean Squared Error: {mse:.10f}")

    # OLS Regression for beta coefficients & p-values
    X_train_sm = sm.add_constant(X_train_scaled)  # Add constant for intercept
    model_sm = sm.OLS(y_train, X_train_sm).fit()
    results_df = pd.DataFrame({
        "Feature": ["Intercept"] + list(X.columns),
        "Beta Coefficient": model_sm.params,
        "P-Value": model_sm.pvalues
    })
    results_df["Significant (<0.10)?"] = results_df["P-Value"].apply(lambda p: "Yes" if p < 0.10 else "No")

    print("\nRegression Results:")
    print(results_df)

    # Residual Analysis
    residuals = y_test - y_pred
    residuals_df = pd.DataFrame({"Actual": y_test, "Predicted": y_pred, "Residual": residuals})

    print("\nSum of Residuals:", sum(residuals_df["Residual"]))  # Should be close to 0
    print("\nResidual Analysis:")
    print(residuals_df.describe())

    # Residuals Plot
    plt.figure(figsize=(8, 5))
    plt.scatter(y_pred, residuals, alpha=0.5, color="blue")
    plt.axhline(y=0, color='red', linestyle='--')
    plt.xlabel("Predicted Values")
    plt.ylabel("Residuals")
    plt.title("Residuals Plot")
    plt.show()


def lasso_reg(dataset, target):
    X = dataset.drop(columns=[target])
    y = dataset[target]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    lasso = LassoCV(cv=5,alphas=np.logspace(-4,1,50)).fit(X_train_scaled,y_train)

    y_pred = lasso.predict(X_test_scaled)
    r2 = r2_score(y_test, y_pred)
    mse = mean_squared_error(y_test, y_test)
    print(f"R^2 Score: {r2:.4f}")
    print(f"Mean Squared Error: {mse:.10f}")
    print(f"Best Alpha (λ): {lasso.alpha_:.6f}")

    lasso_results = pd.DataFrame({
        "Feature": X.columns,
        "Lasso Coefficient": lasso.coef_
    })
    lasso_results["Selected"] = lasso_results["Lasso Coefficient"].apply(lambda coef: "Yes" if coef != 0 else "No")
    lasso_results_sorted = lasso_results[lasso_results["Selected"] == "Yes"].copy()
    lasso_results_sorted["Abs Coefficient"] = lasso_results_sorted["Lasso Coefficient"].abs()
    lasso_results_sorted = lasso_results_sorted.sort_values(by="Abs Coefficient", ascending=False).drop(columns=["Abs Coefficient"])
    print("\nLasso Regression Results (Selected Features Only, Sorted by Absolute Value):")
    print(lasso_results_sorted)

def load_daily(path): #to load data and transform it in csv

    df = pd.read_csv(path, index_col=0, dtype={0:"int64"})
    df.rename_axis("date", inplace=True)
    df.index = pd.to_datetime(df.index.astype(str), format="%Y%m%d")

    df.fillna(0, inplace=True)
    df = df.mean(axis=1).round(18)
    df = df.to_frame(name="daily_return")

    return df


def plot_daily_returns(df, column):
    plt.figure(figsize=(12, 6))

    plt.plot(df.index, df[column], color='blue', alpha=0.5, label="Daily Return")

    plt.xlabel("Date")
    plt.ylabel("Daily Return")
    plt.title("Daily Returns Over Time")
    plt.legend()
    plt.grid(True, linestyle="--", alpha=0.5)

    plt.xticks(rotation=45)

    plt.show()

def plot_fama_french_factors(df, factors):
    plt.figure(figsize=(12, 6))

    # Plot each factor
    for factor in factors:
        plt.plot(df.index, df[factor], label=factor, linewidth=1)

    # Customize x-axis: show only key years (e.g., 2006, 2008, 2010)
    plt.xticks(df.index[::500], rotation=45)  # Adjust step size based on dataset size

    # Add title, labels, and legend
    plt.xlabel("Year")
    plt.ylabel("Factor Value")
    plt.title("Fama-French 5-Factor Model Over Time")
    plt.legend(loc="upper right")
    plt.grid(True, linestyle="--", alpha=0.5)

    plt.show()