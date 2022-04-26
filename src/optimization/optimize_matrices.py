from scipy.optimize import minimize
from utils.optimization_input_utils import OptimizationParameters, import_config
from utils.optimization_utils import add_channel_campaign_constraints, \
    add_budget_constraints, objective_function
from generate_data import generate_data_from_config
from utils.optimization_output_utils import report_output
import numpy as np
from datetime import datetime
# Custom utility functions
from mmpac import *
import logging


def impression_lower_bounds(impression_init):
    """
    (min, max) pairs for each element in impressions, defining the bounds
    on that parameter. impressions cannot be ==0 due to multiplicative approach
    :params impression_init: array to initilize impressions
    """
    return tuple(([(1e-7, None)] * len(impression_init)))


def optimize(params, config):
    """
    Main optimization function for scenario planner
    This formula uses scipy.minimize to optimize the non-linear programming
    problem. https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.minimize.html
    :param params: dictionary of optimization inputs
    :param config: optimization_config.yaml options
    :param B_custom: custom input for the budget (otherwise uses data["budget"])
    :param spend_factor: A fraction multiplied to all spend in that optimization
    """
    spend_factor = 1 / config["number_weeks_in_quarter"]
    startime = datetime.now()

    # Define objective function
    def obj_fun(impression): return objective_function(
        impression, params.alpha, params.S)

    # Get constraints
    constraints = []
    constraints = add_budget_constraints(config["budget_constraint_type"], constraints,
                                         params, spend_factor)
    constraints = add_channel_campaign_constraints(config["channel_campaign_constraint"],
                                                   constraints, params, spend_factor)
    # Define impression bounds
    bnds = impression_lower_bounds(params.impression_init)

    # Optimize
    logging.info("optimizing %i 'impression's" % len(params.impression_init))
    logging.info("with %i output combinations..." % len(params.alpha))
    # ---- MAIN METHOD: Sequential Least Squares Programming (SLSQP) -----
    res = minimize(obj_fun, params.impression_init, bounds=bnds,
                   constraints=constraints, method='SLSQP',
                   jac=config["jac_method"],
                   options={'disp': True, 'maxiter': config["max_iterations"],
                            'ftol': config["ftol"], 'iprint': 2})
    total_time = "Total time: " + str(datetime.now() - startime)
    logging.info(total_time)
    return res


def run_optimization(config_file_path):
    """
    Main MMM Optimization Script
    Pulls in the configurations and runs the approiate optimization
    """
    logging.basicConfig(filename='optimization.log',
                        #encoding='utf-8', 
                        level=logging.INFO)

    config = import_config(config_file_path)

    if(config["update_data"]):
        # Creates the json file that's used in the optimization i.e. config["output_fp"]
        generate_data_from_config(config)

    # Read in optimization parameters
    params = OptimizationParameters(json_file_path=config["output_fp"])

    res = optimize(params, config)
    report_output(res, params, config)


if __name__ == "__main__":
    run_optimization("optimization_config.yaml")
