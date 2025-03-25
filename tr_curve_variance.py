#%%
# Load packages

import numpy as np
import pandas as pd
import ast
from sklearn.preprocessing import MinMaxScaler
import torch
from main import *

#%% Estimated treatment-response curves 

# Real-world - train
df = pd.read_csv('data/food_df_rob.csv') 
year = 2018
df = df[df['year'] == year]
msfi_rate_previous = df['prev_msfi'].values

# Prepare real world data
X = df[['ad_exc2_aid_gdp', 'infl', 'food_prod', 'food_imp_net_gdp',
        'log_gdp', 'log_gdp_pc', 'gdp_pc_grw', 'gov_score', 'e_pt_coup', 'disast_affect', 'fight', 'prec_z', 'net_dens']].to_numpy()
A = df['ad_sdg2_aid'].to_numpy()
Y = df['change_msfi'].to_numpy()

n = X.shape[0]
p = X.shape[1]

data = np.concatenate([Y.reshape(n,1), A.reshape(n,1), X],axis=1)

# Data standardization: min-max scaler
scaler = MinMaxScaler()
data_scaled = scaler.fit_transform(data[:,2:])
data_train = np.concatenate([data[:,0:2], data_scaled], axis=1)

# Real world - test
df = pd.read_csv('data/food_df_rob.csv')
year = 2019
df = df[df['year'] == year]
population = df['pop'].values

# Prepare real world data
X = df[['ad_exc2_aid_gdp', 'infl', 'food_prod', 'food_imp_net_gdp',
        'log_gdp', 'log_gdp_pc', 'gdp_pc_grw', 'gov_score', 'e_pt_coup', 'disast_affect', 'fight', 'prec_z', 'net_dens']].to_numpy()
A = df['ad_sdg2_aid'].to_numpy()
Y = df['change_msfi'].to_numpy()


n = X.shape[0]
p = X.shape[1]

data2 = np.concatenate([Y.reshape(n,1), A.reshape(n,1), X],axis=1)

# Data standardization: min-max scaler
scaler = MinMaxScaler()
data_scaled2 = scaler.fit_transform(data2[:,2:])
data_test = np.concatenate([data2[:,0:2], data_scaled2], axis=1)

# Hyperpar list
hyper_opt_list = open("hyperpars/hyperpars_opt_real.txt", "r")
hyper_opt_list = hyper_opt_list.read()
hyper_opt = ast.literal_eval(hyper_opt_list)
opt_hyperpars = hyper_opt[-5]

# Set all seeds
np.random.seed(123)
torch.manual_seed(123)

runs = 10
b_coef = np.empty((8,runs))
sig_hat = np.empty((1,runs))
a_coef = np.empty((6,runs))
df_bae = np.empty((57,9,runs))

# Estimate model
for i in range(10):
    data_train_bae, mod_BAE = BAE(data_train, opt_hyperpars)    
    device = torch.device('cpu')  
    X = torch.from_numpy(data_test[:,2:].astype(np.float32))
    X = X.to(device)
    X_reduced, _, _ = mod_BAE(X)
    X_reduced = X_reduced.cpu().detach().numpy()
    data_test_bae = np.concatenate([data_test[:,0:2],X_reduced], axis=1)       
    df_bae[:,:,i] = data_test_bae         
    aid_max = np.max(data_train[:,1])
    aid_min = np.min(data_train[:,1])
    aid_random = np.random.uniform(aid_min, aid_max, (data_train_bae.shape[0], opt_hyperpars['m_scw']))
    data_train_scw = SCw(data_train_bae, aid_random, opt_hyperpars)
    b, s, a = GPS(data_train_scw)
    b_coef[:,i:(i+1)], sig_hat[:,i:(i+1)], a_coef[:,i:(i+1)] = b, s, a
    
    

# %%

#Prediction across space
T_values = np.linspace(np.min(df.iloc[:, 4]), np.max(df.iloc[:, 4]), 100)

# Get results
res_table = np.empty(shape=(57, 100, 10))
for l in range(10):
    for i in range(len(T_values)):
        df_bae[:,1,l] =  T_values[i]
        pred_vals = GPS_pred(df_bae[:,1:,l], b_coef[:, l], sig_hat[:,l], a_coef[:, l])
        res_table[:,i,l] = pred_vals
    

# %%
runs = np.arange(1, 11)  
countries = np.arange(1, 58)  

country_list = []
t_value_list = []
value_list = []
run_list = []

# Iterate through each country, t_value, and run
for country in countries:
    for i, t_value in enumerate(T_values):  
        for run in runs:
            country_list.append(country)
            t_value_list.append(t_value)
            value_list.append(res_table[country-1, i, run-1])  
            run_list.append(run)
df_reset = df.reset_index(drop=True)
# Create the DataFrame
res_df = pd.DataFrame({
    'country': country_list,
    't_value': t_value_list,
    'value': value_list,
    'run': run_list
})

res_df['country_name'] = res_df['country'].apply(lambda x: df_reset.iloc[x-1]['country'] if pd.notna(x) else np.nan)
res_df.to_csv('outputs/treat_res_curves.csv', index=False)

