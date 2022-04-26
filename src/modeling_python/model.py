from utils.preprocess_utils import *
from utils.bandwidth_delay_utils import *
from utils.modeling_utils import *
from utils.hyperparameter_tuning_utils import *
from utils.utils import *
import pandas as pd


def preprocess_data(best_hyperparameters, config):
    """
    Generate the training dataset
    :param best_hyperparameters: dictionary - of the model hyperparameters
    :param config: dictionary - script configurations
    :return dmat: pandas dataframe - parameter for training (plus outputcombination columns + output)
    :return fmat: pandas dataframe - exogenous variables for training (plus outputcombination columns)
    :return mmo: dictionary of pandas dataframes of output combinations and mmo matrices
    """
    # Get tables from vertica
    impression_table, conversion_table, factor_table = get_vertica_data()

    # Mock data
    dmat = pd.DataFrame({'week': ["2020-01-01", "2020-01-08"],
                         'lob': ["Life", "Di"],
                         'state': ["NY", "CT"],
                         'segment': ["New", "New"],
                         'P_TV': [200, 1000],
                         'P_Audio_Terrestrial': [100, 488],
                         'conversions': [10, 20]})
    fmat = pd.DataFrame({'week': ["2020-01-01", "2020-01-08"],
                         'lob': ["Life", "Di"],
                         'state': ["NY", "CT"],
                         'segment': ["New", "New"],
                         'CPI': [20, 39],
                         'CCI': [20, 40]})
    mmo = {"Life_NY_New": pd.DataFrame(data={"P_TV": [1, 0],
                                             "P_Audio_Terrestrial": [0, 0]},
                                       index=["TV_Brand_CA", "TV_Brand_TX"]),
           "Life_CT_New": pd.DataFrame(data={"P_TV": [1, 0],
                                             "P_Audio_Terrestrial": [1, 1]},
                                       index=["TV_Brand_CA", "TV_Brand_TX"])}
    return dmat, fmat, mmo


def hyperparameter_tuning(config):
    """
    :param config: dictionary - script configurations that are not hyperparameters
    :return best_hyperparmaters: dict - the turned hyperparameter set
    """
    # hyperparameter_dict: dictionary with each key and a list of values for all possible values
    best_hyperparameters = {"hyper1": None, "hyper2": None}
    return best_hyperparameters


def model(dmat, fmat, best_hyperparameters, config):
    """
    :param dmat: pandas dataframe - parameters for training (plus outputcombination columns + output)
    :param fmat: pandas dataframe - exogenous variables for training (plus outputcombination columns
    :param best_hyperparmaters: dict - the turned hyperparameter set
    :param config: dictionary - script configurations that are not hyperparameters
    :return parameter_coeffs: pandas dataframe - model betas
    :return combination_coeffs: pandas dataframe - input level coefficients
    :return fold_mape: dictionary - mean absolute percent error across folds
    :return fold_R2: dictionary - R^2 across folds
    """
    # cross validation - start with a package
    combination_coeffs = None
    parameter_coeffs = None
    fold_mape = {1: 0.11, 2: 0.12, 3: 0.13, 4: 0.12, 5: 0.12}
    fold_R2 = {1: 0.82, 2: 0.83, 3: 0.81, 4: 0.80, 5: 0.81}
    return parameter_coeffs, combination_coeffs, fold_mape, fold_R2


def report_metrics(parameter_coeffs, combination_coeffs):
    """
    :param parameter_coeffs: pandas dataframe - model betas
    :param combination_coeffs: pandas dataframe - input level coefficients
    :return attribution: pandas dataframe - output combinations, input combinations, weeks, attribution
    :return attr_base: pandas datafame - attribution for different LOBs
    """
    attribution = None
    attr_base = None
    return attribution, attr_base


def main():
    """
    Main MMM modeling script
    """
    # -- GET CONFIG --
    config = import_config("config.yaml")

    # -- HYPERPARAMETER TUNING --
    if config["tune_hyperparameter_set"]:
        best_hyperparameters = hyperparameter_tuning(config)
    else:
        best_hyperparameters = import_config(
            "hyperparameters.yaml")

    # -- PREPROCESS DATA --
    dmat, fmat, mmo = preprocess_data(best_hyperparameters, config)

    # -- MODELING --
    parameter_coeffs, combination_coeffs, fold_mape, fold_R2 = model(
        dmat, fmat, best_hyperparameters, config)

    # -- REPORT METRICS --
    attribution, attr_base = report_metrics(
        parameter_coeffs, combination_coeffs)
    print("Attribution: ", attribution)
    print("Attribution: ", attr_base)


if __name__ == "__main__":
    main()
