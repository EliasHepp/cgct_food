

#%% Load packages 

import numpy as np
import pandas as pd
import ast
import torch
from main import *
from eval_cf_MISE import *

#%% Data 

# Data train
df = pd.read_csv('data/Sim_data_train.csv')
data_train = df.to_numpy()

# Data test counterfactual
data_cf = np.load('data/data_cf.npz')
A_cf = data_cf['A_cf']
Y_cf = data_cf['Y_cf']
X_cf = data_cf['X_cf']

# Hyperpar list
hyper_opt_list = open("hyperpars/hyperpars_opt_sim_ablation.txt", "r")
hyper_opt_list = hyper_opt_list.read()
hyper_opt = ast.literal_eval(hyper_opt_list)

# Convert hyperpar_opt_list so that its values are iterable
for i in range(len(hyper_opt)):
    for key in hyper_opt[i].keys():
        hyper_opt[i][key] = [hyper_opt[i][key]]

#%% Ablation study: Point estimate and variance (averaged over 10 runs)

models = ['cgct_lm_nocfgen', 'cgct_nn_nocfgen', 'cgct_dr_nocfgen', 'cgct_gps_nocfgen', 'cgct_rf_nocfgen']

# Set all seeds
np.random.seed(123)
torch.manual_seed(123)

# Get results
res_table = np.empty(shape=(5,10))
for l in range(10):
    test_loss = []
    for i, model in enumerate(models):
        print('i=',i)   
        cv_results = eval_MISE(data_train, X_cf, A_cf, Y_cf, model, hyper_opt[i])
        test_loss.append(cv_results[0]['loss'])
    res_table[:,l] = np.array(test_loss)

# %%
#Export results

res_df = pd.DataFrame(np.transpose(res_table), columns=models)
res_df.insert(0, "measure", [f"run {i+1}" for i in range(len(res_table.T))])

stats = {
    "measure": ["mean", "median", "sd"],
    **{model: [res_df[model].mean(), res_df[model].median(), res_df[model].std()] for model in res_df.columns if model != "measure"}
}

stats_df = pd.DataFrame(stats)
result_df = pd.concat([res_df, stats_df], ignore_index=True)

result_df.to_csv("outputs/ablation_cf.csv")
# %%
