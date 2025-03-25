
#%%
# Load packages

import numpy as np
import pandas as pd
import scipy
import ast
from sklearn.preprocessing import MinMaxScaler
import torch
from main import *

#%% Prepare Data

# Real-world - train
df = pd.read_csv('data/food_df_rob.csv')
aid_sds = df.groupby('country')['ad_sdg2_aid'].apply(np.std).values

year = 2018
df = df[df['year'] == year]
population_previous = df['pop'].values
msfi_rate_previous = df['prev_msfi'].values

# Prepare real world data
X = df[['ad_exc2_aid_gdp', 'infl', 'food_prod', 'food_imp_net_gdp',
        'log_gdp', 'log_gdp_pc', 'gdp_pc_grw', 'gov_score', 'e_pt_coup', 'disast_affect', 'fight', 'prec_z', 'net_dens']].to_numpy()
A = df['ad_sdg2_aid']
A = A.to_numpy()
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
msfi_rate_current = df['prev_msfi'].values
population = df['pop'].values
pop_curr = df['pop'].values

# Prepare real world data
X = df[['ad_exc2_aid_gdp', 'infl', 'food_prod', 'food_imp_net_gdp',
        'log_gdp', 'log_gdp_pc', 'gdp_pc_grw', 'gov_score', 'e_pt_coup', 'disast_affect', 'fight', 'prec_z', 'net_dens']].to_numpy()
A = df['ad_sdg2_aid']
A = A.to_numpy()
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

# Set seeds
np.random.seed(123)
torch.manual_seed(123)

# Estimate model
data_train_bae, mod_BAE = BAE(data_train, opt_hyperpars)    
device = torch.device('cpu')  
X = torch.from_numpy(data_test[:,2:].astype(np.float32))
X = X.to(device)
X_reduced, _, _ = mod_BAE(X)
X_reduced = X_reduced.cpu().detach().numpy()
data_test_bae = np.concatenate([data_test[:,0:2],X_reduced], axis=1)                                     
aid_max = np.max(data_train[:,1])
aid_min = np.min(data_train[:,1])
aid_random = np.random.uniform(aid_min, aid_max, (data_train_bae.shape[0], opt_hyperpars['m_scw']))
data_train_scw = SCw(data_train_bae, aid_random, opt_hyperpars)
b_coef, sig_hat, a_coef = GPS(data_train_scw)

# Optimal Allocation

#%%

def optimal_allocation(data_test, b_coef, sig_hat, a_coef, msfi_rate_previous, population):
    
    n = data_test.shape[0]
    A_obs = data_test[:,1:2]
    budget = np.sum(A_obs)
    covariate_data = data_test[:,2:]
    Y_obs_pred = np.sum(msfi_rate_previous/100*(1-GPS_pred(data_test[:,1:], b_coef, sig_hat, a_coef))*population)

    def optim_fn(A_prop, covariate_data, b_coef, sig_hat, a_coef, msfi_rate_previous, population):
        n = covariate_data.shape[0]
        Y_pred = np.sum((msfi_rate_previous/100)*(1-GPS_pred(np.concatenate([A_prop.reshape((n,1)), covariate_data],axis=1), 
                                                      b_coef, sig_hat, a_coef))*population)
        return Y_pred
    
    def constraint(A_prop, budget):
        return  budget - np.sum(A_prop) 
    
    cons = [{'type':'ineq', 'fun': constraint, 'args': [budget]}]
    
    lower_bounds = A_obs.flatten() - aid_sds
    upper_bounds = A_obs.flatten() + aid_sds
    
    lower_bounds = np.maximum(lower_bounds, 0)
    
    bounds = scipy.optimize.Bounds(lower_bounds, upper_bounds)
    
    total_cases = (msfi_rate_previous/100)*population
    
    A_init = budget*(total_cases/np.sum(total_cases))
    
    A_opt = scipy.optimize.minimize(fun=optim_fn, x0=A_init, method='SLSQP',
                                    args=(covariate_data, b_coef, sig_hat, a_coef, msfi_rate_previous, population), 
                                    bounds=bounds, constraints=cons).x
    Y_opt_pred = np.sum((msfi_rate_previous/100)*(1-GPS_pred(np.concatenate([A_opt.reshape((n,1)), covariate_data],axis=1), 
                                                      b_coef, sig_hat, a_coef))*population)
    
    return A_opt, Y_opt_pred, Y_obs_pred

A_opt, Y_opt_pred, Y_obs_pred = optimal_allocation(data_test_bae, b_coef, sig_hat, a_coef, msfi_rate_previous, population_previous)

#%% With initial bounds

def optimal_allocation_unbound(data_test, b_coef, sig_hat, a_coef, msfi_rate_previous, population):
    
    n = data_test.shape[0]
    A_obs = data_test[:,1:2]
    budget = np.sum(A_obs)
    covariate_data = data_test[:,2:]
    Y_obs_pred = np.sum(msfi_rate_previous/100*(1-GPS_pred(data_test[:,1:], b_coef, sig_hat, a_coef))*population)

    def optim_fn(A_prop, covariate_data, b_coef, sig_hat, a_coef, msfi_rate_previous, population):
        n = covariate_data.shape[0]
        Y_pred = np.sum((msfi_rate_previous/100)*(1-GPS_pred(np.concatenate([A_prop.reshape((n,1)), covariate_data],axis=1), 
                                                      b_coef, sig_hat, a_coef))*population)
        return Y_pred
    
    def constraint(A_prop, budget):
        return  budget - np.sum(A_prop) 
    
    cons = [{'type':'ineq', 'fun': constraint, 'args': [budget]}]
    
    A_std = np.std(A_obs)
    A_max = np.max(A_obs)
    bounds = scipy.optimize.Bounds(np.zeros(n), (A_max+A_std)*np.ones(n))
    
    total_cases = (msfi_rate_previous/100)*population
    
    A_init = budget*(total_cases/np.sum(total_cases))
    
    A_opt = scipy.optimize.minimize(fun=optim_fn, x0=A_init, method='SLSQP',
                                    args=(covariate_data, b_coef, sig_hat, a_coef, msfi_rate_previous, population), 
                                    bounds=bounds, constraints=cons).x
    Y_opt_pred = np.sum((msfi_rate_previous/100)*(1-GPS_pred(np.concatenate([A_opt.reshape((n,1)), covariate_data],axis=1), 
                                                      b_coef, sig_hat, a_coef))*population)
    
    return A_opt, Y_opt_pred, Y_obs_pred


A_opt_unbound, Y_opt_pred_unbound, Y_obs_pred_unbound = optimal_allocation_unbound(data_test_bae, b_coef, sig_hat, a_coef, msfi_rate_previous, population_previous)

#%% Save Results

A_storage = []
A_storage.append(A_opt)
A_storage.append(A_opt_unbound)
A_storage.append(A)
A_alloc = pd.DataFrame(np.transpose(A_storage), columns= ["A_opt", "A_opt_unbound", "A_factual"])
df.reset_index(drop=True, inplace=True)
A_alloc["country"] = df["country"]
A_alloc.to_csv("outputs/optim_allocation_A.csv")

Y_storage = []
Y_storage.append(Y_opt_pred)
Y_storage.append(Y_obs_pred)
Y_storage.append(Y_opt_pred_unbound)
Y_storage.append(np.sum(msfi_rate_current/100*pop_curr))

with open("outputs/optim_allocation.txt", "w") as file:
    for item in Y_storage:
        file.write(f"{item}\n")


#%% # Bootstrap CI

# Bootstrap intervals
boot_samples = 20
n = data_train.shape[0]
Y_obs_pred, Y_opt_pred = np.zeros(boot_samples), np.zeros(boot_samples)
for b in range(boot_samples):
    print(b)
    b_ind = np.random.choice(n, n, replace=True)
    data_train_bae, mod_BAE = BAE(data_train[b_ind,:], opt_hyperpars)    
    device = torch.device('cpu')  
    X = torch.from_numpy(data_test[:,2:].astype(np.float32))
    X = X.to(device)
    X_reduced, _, _ = mod_BAE(X)
    X_reduced = X_reduced.cpu().detach().numpy()
    data_test_bae = np.concatenate([data_test[:,0:2],X_reduced], axis=1)                                     
    aid_max = np.max(data_train[:,1])
    aid_min = np.min(data_train[:,1])
    aid_random = np.random.uniform(aid_min, aid_max, (data_train_bae.shape[0], opt_hyperpars['m_scw']))
    data_train_scw = SCw(data_train_bae, aid_random, opt_hyperpars)
    b_coef, sig_hat, a_coef = GPS(data_train_scw)
    
    A_opt , Yopt, Yobs = optimal_allocation(data_test_bae, b_coef, sig_hat, a_coef, msfi_rate_previous, population)
    Y_opt_pred[b], Y_obs_pred[b] = Yopt, Yobs

# %% Export and save

boots_df = pd.DataFrame({
    'Y_obs_pred': Y_obs_pred,
    'Y_opt_pred': Y_opt_pred
})

# Export the DataFrame to a CSV file
boots_df.to_csv('outputs/optim_allocation_boots.csv', index=False)

# %%
