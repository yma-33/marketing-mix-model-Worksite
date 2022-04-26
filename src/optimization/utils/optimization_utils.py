
import logging
import numpy as np


def objective_function(impression, alpha, S):
    """
    Objective function for number of conversions with exogenous variables (C)
    y = C1 * (m1^alpha11 * m2^alpha12*...) + C2 * \
              (m1*alpha12 * m2^alpha22*...) + ...
    :param impression: current optimized channel_campaign_state impressions
    :param alpha: matrix of output_combinations x channel_campaign_state combinations
    :param S: matrix to convert impression array to impression matrix (output_combinations x channel_campaign_state)
    :param output_factor: vector (1 x output_combinations) of some vector
                             multiplied by each output combination
    """
    impressions_tothe_alpha = np.power(np.dot(S, np.diag(impression)), alpha)
    conversions_per_output = np.nanprod(impressions_tothe_alpha, axis=1)
    obj_func = conversions_per_output.sum() * -1
    return obj_func


def budget_constraint(impression, CPI, T):
    """
    Equality constraint for the budget based on the current values of M (impressions).
    We have the cost per impression (CPI) for each channel_campaign combination
    and multiply that by the aggregate impressions for each state for that channel_campaign.
    budget = CPI_1 * (m1 + m2 + m3) + ... + CPI_N (m9 + m10 + m11) + ...
    :param impression: current optimized channel_campaign_state impressions (1 x channel_campaign_state)
    :param CPI: median/average dollar cost per impression (1 x channel_campaign)
    :param T: matrix to aggregate channel_campaign_state ms by channel_campaign
    """
    return impression @ (T * CPI).sum(axis=1)


def lower_or_equality_constraint(impression, CPI, T, lower_or_equality):
    """
    Lower bound or equality constraint (both will have the same syntax)
    for channel_campaign budget based on the current values of impressions.
    We have the cost per impression (CPI) for each channel_campaign combination
    and multiply that by the aggregate impressions for that channel_campaign.
    example: (where n = total number of channel_campaigns)
    cpi_1 * (impression_1 + impression_2  + impression_3) >= lower_1
    cpi_2 * (impression_4 + impression_5 + impression_6) >= lower_2
    ....
    cpi_n * (impression_x + impression_y + impression_z) >= lower_n
    :param impression: current optimized channel_campaign_state impressions (1 x channel_campaign_state)
    :param CPI: average dollar cost per impression (1 x channel_campaign)
    :param T: matrix to aggregate channel_campaign_state to channel_campaign
    :return: array of lower bound for each channel_campaign (1 x channel_campaign)
    """
    return impression @ T * CPI - lower_or_equality


def upper_constraint(impression, CPI, T, upper):
    """
    Upper bound for channel_campaign budget based on the current values of impressions.
    We have the cost per impression (CPI) for each channel_campaign combination
    and multiply that by the aggregate impressions for that channel_campaign.
    example: (where n = total number of channel_campaigns)
    cpi_1 * (impression_1 + impression_2  + impression_3) <= upper_1
    cpi_2 * (impression_4 + impression_5 + impression_6) <= upper_2
    ....
    cpi_n * (impression_x + impression_y + impression_z) <= upper_n
    :param impression: current optimized channel_campaign_state impressions (1 x channel_campaign_state)
    :param CPI: average dollar cost per impression (1 x channel_campaign)
    :param T: matrix to aggregate channel_campaign_state to channel_campaign
    :return: array of lower bound for each channel_campaign (1 x channel_campaign)
    """
    return upper - impression @ T * CPI


def add_budget_constraints(budget_constraint_type, constraints, params, spend_factor):
    """
    Optimization constraints for the SLSQP model in the standard Scipy format:
    constraint1 = {'type' : 'ineq', 'fun' : constraint1_function }
    constraint2  = {'type' : 'eq', 'fun' : constraint2_function }
    constraints = [constraint1, constraint2, ... , constraintN]
    where: 'ineq' is an inequality constraint
           'eq' is equality constraint
    :param budget_constraint_type: 'total', 'category', or 'none'
    :param constraints: list of SLSQP constraints
    :param params: class of optimization parameters
    :param spend_factor: fraction of spend per week
    :return constraints: list of updated constraints
    """
    if budget_constraint_type == "total":
        budget_total = params.update_budget_spend_factor(
            params.budget, spend_factor)
        constraints += [
            {"type": "eq", "fun": lambda impression: budget_constraint(
                impression, params.CPI, params.T) - budget_total}
        ]
    elif budget_constraint_type == "category":
        B_Social = params.update_budget_spend_factor(
            params.B_Social, spend_factor)
        B_CRM = params.update_budget_spend_factor(
            params.B_CRM, spend_factor)
        B_Search = params.update_budget_spend_factor(
            params.B_Search, spend_factor)
        B_Brand = params.update_budget_spend_factor(
            params.B_Brand, spend_factor)
        constraints += [
            {"type": "eq", "fun": lambda impression: budget_constraint(
                impression, params.CPI, params.T_Social) - B_Social},
            {"type": "eq", "fun": lambda impression: budget_constraint(
                impression, params.CPI, params.T_CRM) - B_CRM},
            {"type": "eq", "fun": lambda impression: budget_constraint(
                impression, params.CPI, params.T_Search) - B_Search},
            {"type": "eq", "fun": lambda impression: budget_constraint(
                impression, params.CPI, params.T_Brand) - B_Brand}
        ]
    elif budget_constraint_type == "none":
        logging.info("Ignoring budget constraint...")
    else:
        raise ValueError(
            "Budget constraint must be 'total', 'category', or 'none'")
    return constraints


def add_channel_campaign_constraints(channel_campaign_constraint, constraints, params, spend_factor):
    """
    Optimization constraints for the SLSQP model in the standard Scipy format:
    constraint1 = {'type' : 'ineq', 'fun' : constraint1_function }
    constraint2  = {'type' : 'eq', 'fun' : constraint2_function }
    constraints = [constraint1, constraint2, ... , constraintN]
    where: 'ineq' is an inequality constraint
           'eq' is equality constraint
    NOTE: all constraints must be in format where they are either >=0 or =0.
    :param channel_campaign_constraint: either 'upper_lower', 'equality' or 'none
    :param constraints: list of SLSQP constraints
    :param params: class of optimization parameters
    :param spend_factor: fraction of spend per week
    :return constraints: list of updated constraints
    """
    # ---- Channel_campaign combination constraints -----
    upper = params.update_budget_spend_factor(params.upper, spend_factor)
    lower = params.update_budget_spend_factor(params.lower, spend_factor)

    if channel_campaign_constraint == "upper_lower":  # Add the individual channel_campaign constraints
        constraints += [
            # Lower bound constraint
            {"type": "ineq", "fun": lambda impression: lower_or_equality_constraint(
                impression, params.CPI, params.T, lower)},
            # Upper bound constraint
            {"type": "ineq", "fun": lambda impression: upper_constraint(
                impression, params.CPI, params.T, upper)}
        ]
    elif channel_campaign_constraint == "equality":
        equality_variable = params.upper  # using the upper bound as an equality constraint
        constraints += [
            # equality channel campaign constraint
            {"type": "eq", "fun": lambda impression: lower_or_equality_constraint(
                impression, params.CPI, params.T, equality_variable)}
        ]
    elif channel_campaign_constraint == "none":
        logging.info("Ignoring channel campaign budget constraints...")
    else:
        raise ValueError(
            "channel_campaign constraint must be 'upper_lower', 'equality', or 'none'")
    return constraints
