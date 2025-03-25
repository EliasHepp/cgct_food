
#%%
# Load packages

import numpy as np
import pandas as pd
from statsmodels.stats.outliers_influence import variance_inflation_factor
import pandas as pd

# Assume `df` is a DataFrame with your covariates
df = pd.read_csv('food_df_ana.csv') 
df = df.iloc[:, 6:18] 
vif_data = pd.DataFrame()
vif_data["Variable"] = df.columns
vif_data["VIF"] = [variance_inflation_factor(df.values, i) for i in range(df.shape[1])]

print(vif_data)

# %%
import seaborn as sns
import matplotlib.pyplot as plt
corr_matrix = df.corr()
plt.figure(figsize=(10,8))
sns.heatmap(corr_matrix, annot=True, cmap="coolwarm", fmt=".2f")
plt.show()

# %%
