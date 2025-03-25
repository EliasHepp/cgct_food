
import optuna
import numpy as np
import pandas as pd
import torch
import random
from main import *

def CV_hyperpar_search(data_train, data_val, model_type, name, n_trials=50, n_jobs=1):
    
    SEED = 123
    def seed_everything(seed=SEED):
        np.random.seed(seed)
        random.seed(seed)
        torch.manual_seed(seed)
        if torch.cuda.is_available():
            torch.cuda.manual_seed_all(seed)
        
    seed_everything()    
    
    """
    Perform Bayesian optimization for hyperparameter tuning.

    Args:
        data_train (array): Training data.
        data_val (array): Validation data.
        model (str): Model type to optimize ('lm', 'nn', 'dr', 'cf', 'sci', 'cgct_*').
        n_trials (int): Number of optimization trials.

    Returns:
        dict: Best hyperparameters and the corresponding loss.
    """
    device = torch.device('cpu')

    def RMSE(Y_pred, Y_true):
        """Compute Root Mean Squared Error."""
        return np.sqrt(np.mean((Y_pred - Y_true) ** 2))

    def objective(trial):
        # Suggest hyperparameter values based on the model
        if model_type == 'lm':  # Linear Model
            alpha_lm = trial.suggest_float('alpha_lm', 0.05, 5, log = True)
            order_lm = trial.suggest_int('order_lm', 0, 2)
            hyperpars = {'alpha_lm': alpha_lm, 'order_lm': order_lm}
            model = LM(data_train, hyperpars)
            Y_pred = LM_pred(data_val[:,1:], model)

        elif model_type == 'nn':  # Neural Network
            layer_size_nn = trial.suggest_categorical('layer_size_nn', [53, 27, 14])
            lr_nn = trial.suggest_float('lr_nn', 0.0001, 0.001, log=True)
            drop_nn = trial.suggest_float('drop_nn', 0, 0.2)
            n_epochs_nn = trial.suggest_int('n_epochs_nn', 100, 300)
            b_size_nn = trial.suggest_categorical('b_size_nn', [22, 11, 6])
            hyperpars = {
                'layer_size_nn': layer_size_nn,
                'lr_nn': lr_nn,
                'drop_nn': drop_nn,
                'n_epochs_nn': n_epochs_nn,
                'b_size_nn': b_size_nn
            }
            model = NN(data_train, hyperpars)
            Y_pred = NN_pred(data_val[:, 1:], model)

        elif model_type == 'gps':  # Generalized Propensity Score
            b_coef, sig_hat, a_coef = GPS(data_train)
            Y_pred = GPS_pred(data_val[:, 1:], b_coef, sig_hat, a_coef)

        elif model_type == 'dr':  # Dose-Response Networks
            layer_size_dr = trial.suggest_categorical('layer_size_dr', [53, 27, 14])
            rep_size_dr = trial.suggest_categorical('rep_size_dr', [22, 11, 6])
            lr_dr = trial.suggest_float('lr_dr', 0.0001, 0.001, log =  True)
            drop_dr = trial.suggest_float('drop_dr', 0, 0.2)
            n_epochs_dr = trial.suggest_int('n_epochs_dr', 100, 300)
            b_size_dr = trial.suggest_categorical('b_size_dr', [22, 11, 6])

            hyperpars = {
                'layer_size_dr': layer_size_dr,
                'rep_size_dr': rep_size_dr,
                'lr_dr': lr_dr,
                'drop_dr': drop_dr,
                'n_epochs_dr': n_epochs_dr,
                'b_size_dr': b_size_dr,
                'E': 5 
            }
            model, sep = DRNet(data_train, hyperpars)
            Y_pred = DRNet_pred(data_val[:,1:], model, sep)

        elif model_type == 'sci':  # SCIGAN
            alpha_sci = trial.suggest_int('alpha_sci', 1, 1)
            layer_size_sci = trial.suggest_categorical('layer_size_sci', [53, 27, 14])
            lr_sci = trial.suggest_float('lr_sci', 0.0001, 0.001, log = True)
            n_epochs_sci = trial.suggest_int('n_epochs_sci', 100, 300)
            b_size_sci = trial.suggest_categorical('b_size_sci', [22, 11, 6])
            msci = trial.suggest_int('m_sci', 3, 7)
            
            layer_size_scinn = trial.suggest_categorical('layer_size_scinn', [53, 27, 14])
            lr_scinn = trial.suggest_float('lr_scinn', 0.00001, 0.001, log=True)
            n_epochs_scinn = trial.suggest_int('n_epochs_scinn', 100, 300)
 
            aid_max = np.max(data_train[:,1])
            aid_min = np.min(data_train[:,1])           

            hyperpars = {
                                                'alpha_sci': alpha_sci,
                                                'layer_size_scinn': layer_size_scinn,
                                                'lr_scinn': lr_scinn,
                                                'n_epochs_scinn': n_epochs_scinn,
                                                'dosage_samples_sci': msci,
                                                'noise_dim_sci': msci,
                                                'layer_size_sci': layer_size_sci,
                                                'lr_sci': lr_sci,
                                                'n_epochs_sci': n_epochs_sci,
                                                'b_size_sci': b_size_sci,
                                                'aid_max': aid_max,
                                                'aid_min': aid_min
                                            }
            
            model = SCIGAN(data_train, hyperpars)
            Y_pred = SCIGAN_pred(data_val[:,1:], model)
            
        elif model_type == 'cf':
                subforest_size = 4
                n_estimators_options = [i * subforest_size for i in range(10, 101)]  # Example range
                n_estimators = trial.suggest_categorical('n_estimators', n_estimators_options)
                min_samples_leaf = trial.suggest_int('min_samples_leaf', 1, 15)

                params_CF = {
                    'n_estimators': n_estimators,
                    'min_samples_leaf': min_samples_leaf,
                }
                
                cf_model, Y_pred, _ = CAUSFOR(data_train, params_CF)
                
                score = cf_model.score(Y = data_val[:,0], T= data_val[:,1], X= data_val[:,2:])
        
        elif model_type == 'rf':
                subforest_size = 4
                n_estimators_options = [i * subforest_size for i in range(10, 101)]  # Example range
                n_estimators = trial.suggest_categorical('n_estimators', n_estimators_options)
                min_samples_leaf = trial.suggest_int('min_samples_leaf', 1, 15)

                params_RF = {
                    'n_estimators': n_estimators,
                    'min_samples_leaf': min_samples_leaf,
                }
                
                model, _, _ = REGFOR(data_train, params_RF)
                Y_pred = REGFOR_pred(data_val[:,1:], model)

        elif model_type.startswith('cgct'):  # CGCT Models (e.g., cgct_lm, cgct_nn, etc.)
            layer_size_bae = trial.suggest_categorical('layer_size_bae', [14, 10, 7])
            rep_size_bae = trial.suggest_categorical('rep_size_bae', [10, 7, 4])
            drop_bae = trial.suggest_float('drop_bae', 0, 0.2)
            lr_bae = trial.suggest_float('lr_bae', 0.0001, 0.001, log = True)
            n_epochs_bae = trial.suggest_int('n_epochs_bae', 100, 300)
            b_size_bae = trial.suggest_categorical('b_size_bae', [22, 11, 6])
            alpha_bae = trial.suggest_float('alpha_bae', 0.05, 5)

            params_BAE = {
                                        'layer_size_bae': layer_size_bae,
                                        'rep_size_bae': rep_size_bae,
                                        'drop_bae': drop_bae,
                                        'lr_bae': lr_bae,
                                        'n_epochs_bae': n_epochs_bae,
                                        'b_size_bae': b_size_bae,
                                        'alpha_bae': alpha_bae
                                    }

            data_train_bae, mod_BAE = BAE(data_train, params_BAE)

            # Counterfactuals
            X = torch.from_numpy(data_val[:, 2:].astype(np.float32)).to(device)
            X_reduced, _, _ = mod_BAE(X)
            X_reduced = X_reduced.cpu().detach().numpy()
            data_val_bae = np.concatenate([data_val[:, 0:2], X_reduced], axis=1)
            
            m_scw = trial.suggest_int('m_scw', 3, 5)
            alpha_scw = trial.suggest_float('alpha_scw', 0.05, 5)
            order_scw = trial.suggest_int('order_scw', 1, 1)
                                            
            aid_max = np.max(data_train[:,1])
            aid_min = np.min(data_train[:,1])
            aid_random = np.random.uniform(aid_min, aid_max, (data_train_bae.shape[0], m_scw))
                                                
            params_SCw = {
                'alpha_scw': alpha_scw,
                'order_scw': order_scw,
                'm_scw': m_scw
                }
                                                
            data_train_scw = SCw(data_train_bae, aid_random, params_SCw)

            if model_type == 'cgct_gps': # CGCT with GPS
                b_coef, sig_hat, a_coef = GPS(data_train_scw)
                Y_pred = GPS_pred(data_val_bae[:, 1:], b_coef, sig_hat, a_coef)

            elif model_type == 'cgct_cf':  # CGCT with Causal Forest

                subforest_size = 4
                n_estimators_options = [i * subforest_size for i in range(10, 101)]  # Example range
                n_estimators = trial.suggest_categorical('n_estimators', n_estimators_options)
                min_samples_leaf = trial.suggest_int('min_samples_leaf', 1, 15)

                params_CF = {
                    'n_estimators': n_estimators,
                    'min_samples_leaf': min_samples_leaf,
                }
                cf_model, Y_pred, _ = CAUSFOR(data_train_scw, params_CF)
                
                score = cf_model.score(Y = data_val_bae[:, 0], T = data_val_bae[:, 1], X = data_val_bae[:, 2:])
                
            elif model_type == 'cgct_rf':
                subforest_size = 4
                n_estimators_options = [i * subforest_size for i in range(10, 101)]  # Example range
                n_estimators = trial.suggest_categorical('n_estimators', n_estimators_options)
                min_samples_leaf = trial.suggest_int('min_samples_leaf', 1, 15)

                params_RF = {
                    'n_estimators': n_estimators,
                    'min_samples_leaf': min_samples_leaf,
                }
                
                model, _, _ = REGFOR(data_train_scw, params_RF)
                Y_pred = REGFOR_pred(data_val_bae[:, 1:], model)

        else:
            raise ValueError(f"Model {model} is not supported.")

        # Compute RMSE as loss
        if model_type.startswith('cgct') and 'cf' not in model_type:
            Y_true = data_val_bae[:, 0]
            loss = RMSE(Y_pred, Y_true)
        if model_type == "cf" or model_type == "cgct_cf":
            loss = np.sqrt(score)
        else:
            Y_true = data_val[:, 0]
            loss = RMSE(Y_pred, Y_true)
        
        #return np.mean(cv_losses)
        return loss
    
    stud_name = "get_hp_" + name

    # Use Optuna to optimize the objective function
    study = optuna.create_study(study_name=stud_name, storage = f"sqlite:///opt_studies/{stud_name}.db", direction='minimize', sampler=optuna.samplers.TPESampler(seed=123))
    study.optimize(objective, n_trials=n_trials, n_jobs=n_jobs)

    # Get best hyperparameters
    best_hyperparams = study.best_params
    best_loss = study.best_value

    return best_hyperparams, best_loss
