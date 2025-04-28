from pandas import read_csv

from formulas import load_data, clean_csv, load_data_web, load_data_indu, regression, lasso_reg, load_daily, plot_daily_returns, plot_fama_french_factors
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

#fase 1 - 2

#spx dataset
path = "data2/SPX/daily_return_mat.csv"
spx = load_daily(path)

#fama french 5 factors
path1 = "data/SX5E/ff_5_factors_returns_mat.csv"
ff5 = load_data(path1)

#dataset uno: spx e fama french
dataset1 = ff5.join([spx], how="inner")

#momentum
path_mom = "mom.csv"
cleaned_path = clean_csv(path_mom)
mom = load_data_web(cleaned_path)

#short term reversal factor
path_strf = "strf.csv"
cleaned_path1 = clean_csv(path_strf)
strf = load_data_web(cleaned_path1)

#lhort term reversal factor
path_ltrf = "ltrf.csv"
cleaned_path2 = clean_csv(path_ltrf)
ltrf = load_data_web(cleaned_path2)

# industry
path_indu = "industry.csv"
cleaned_path3 = clean_csv(path_indu)
industry = load_data_indu(cleaned_path3)

#datset 2: all factors and daily returns - matte per la regressione
full = ff5.join([mom,strf,ltrf,industry,spx], how="inner")
#print(full.head())
#print(full.tail())
#print(full.info())

#regression(full,"daily_return")
lasso_reg(full,"daily_return")

# dataset 3: spx e variabli importanti selezionate con lasso
dataset3 = ff5.join([industry,mom,spx], how="inner")
dataset3 = dataset3.drop(["SMB", "HML", "CMA", "Industry_Factor_3", "Industry_Factor_4", "Industry_Factor_5",], axis=1)

regression(dataset1,"daily_return")
regression(dataset3,"daily_return")

#plot_daily_returns(spx, "daily_return")

#factors = ["Mkt-RF", "SMB", "HML", "RMW", "CMA"]
#plot_fama_french_factors(ff5, factors)

#print(ff5.describe())























