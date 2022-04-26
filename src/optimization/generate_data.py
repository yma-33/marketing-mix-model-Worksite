from utils.generate_data_utils import *
import datetime as dt
import numpy as np
import os  # For getting environment variables
import json
from mmpac import *
from numpyencoder import NumpyEncoder
import logging


def generate_data_from_config(config):
    """
    This function generates the optimization json needed to run
    the constrained MMM optimization
    """
    # Pull in necessary configurations
    B_dict = config["B_dict"]
    B_Social_dict = config["B_Social_dict"]
    B_Brand_dict = config["B_Brand_dict"]
    B_CRM_dict = config["B_CRM_dict"]
    B_Search_dict = config["B_Search_dict"]
    parent_dic = config["parent_dict"]
    combCoeffs_file_path = config["combCoeffs_file_path"]
    combCoeffs_method = config["data_method"]
    output_alphas = config["output_alphas"]
    output_fp = config["output_fp"]
    time_period = config["time_period"]
    cpi_agg_method = config["cpi_agg_method"]
    optimize_all_weeks_at_once = config["optimize_all_weeks_at_once"]
    data_method = config["data_method"]
    stakeholder_file_path = config["stakeholder_file_path"]

    # Connect to vertica
    vertica_setup(
        server='aws_qa',  # Or "mm_prod" or "aws_prod"
        user=os.environ['mmm_user'],
        password=os.environ['mmm_pass'])

    if time_period not in B_Social_dict:
        logging.debug(f"{time_period} should be one of {str(B_dict)}")
        budget = B_dict[time_period]
        B_Social = 0
        B_Brand = 0
        B_CRM = 0
        B_Search = 0
    else:
        budget = B_dict[time_period]
        B_Social = B_Social_dict[time_period]
        B_Brand = B_Brand_dict[time_period]
        B_CRM = B_CRM_dict[time_period]
        B_Search = B_Search_dict[time_period]

    # Current time periods
    curr_year = int(time_period.split("-")[0])
    curr_quarter = int(time_period.split("-")[1][-1])
    # Get lower/upper bound column names
    curr_yr_qtr = "Q" + str(curr_quarter) + "_" + str(curr_year)
    lower_col = curr_yr_qtr + "_BUDGET_MIN"
    upper_col = curr_yr_qtr + "_BUDGET_MAX"

    # Get bounds from stakeholders from either a filepath or vertica
    stakeholder_bounds = pull_in_stakeholder_bounds(
        data_method, stakeholder_file_path)

    # Inputs to the objective function

    alpha_output_names, alpha, impression_names, S, ch_camp_names = read_in_coefficients(
        combCoeffs_file_path, output_alphas, upper_col, stakeholder_bounds,
        combCoeffs_method)

    # Initial values of impressions to optimize on
    impression_init = np.ones(len(impression_names)) * 10000

    # Get channel_campaign combinations for constraints and output
    ch_camp_comb, T = get_channel_campaign_mappings(
        impression_names, ch_camp_names)
    T_Social = get_channel_campaign_mappings(
        impression_names, ch_camp_names, parent_dic, "Social")
    T_Brand = get_channel_campaign_mappings(
        impression_names, ch_camp_names, parent_dic, "Brand")
    T_CRM = get_channel_campaign_mappings(
        impression_names, ch_camp_names, parent_dic, "CRM")
    T_Search = get_channel_campaign_mappings(
        impression_names, ch_camp_names, parent_dic, "Search")

    # Cost per impression array by channel campaign combination
    CPI = get_CPI(ch_camp_comb, cpi_agg_method, curr_year, curr_quarter)

    # Upper and lower bounds for current time
    upper, lower, cat = get_upper_lower_bounds(
        ch_camp_comb, lower_col, upper_col, stakeholder_bounds)

    # Remove spend from categories where lower is not included in objective function
    # Get bounds from channel_campaigns excluded from the model
    excl_ch_campaign_comb, excl_ch_campaign_cat, excl_ch_campaign_lower, \
        B_Social, B_Brand, B_CRM, B_Search = get_excluded_lower_bounds(
            stakeholder_bounds, lower_col, ch_camp_comb,
            B_Social, B_Brand, B_CRM, B_Search)

    # Stack matrices by week
    if optimize_all_weeks_at_once:
        S, alpha, impression_init, impression_names, T = stack_objective_function_data(
            curr_year, curr_quarter, S, alpha, impression_init, impression_names, T)

    # Check the data
    check_upper_bounds(cat, upper, B_Social, B_Brand, B_CRM, B_Search)

    # Close connection to save resources after everything done
    vertica_disconnect()
    data_dict = [{"alpha_output_names": alpha_output_names,
                  "impression_names": impression_names,
                 "channel_campaigns": ch_camp_comb,
                  "T": T,
                  "T_Social": T_Social,
                  "T_Brand": T_Brand,
                  "T_CRM": T_CRM,
                  "T_Search": T_Search,
                  "S": S,
                  "alpha": alpha,
                  "lower": lower,
                  "upper": upper,
                  "CPI": CPI,
                  "budget": budget,
                  "B_Social": B_Social,
                  "B_Brand": B_Brand,
                  "B_CRM": B_CRM,
                  "B_Search": B_Search,
                  "impression_init": impression_init,
                  "cat": cat,
                  "excl_ch_campaign_comb": excl_ch_campaign_comb,
                  "excl_ch_campaign_cat": excl_ch_campaign_cat,
                  "excl_ch_campaign_lower": excl_ch_campaign_lower
                  }]

    with open(output_fp, "w") as outfile:
        json.dump(data_dict,
                  outfile,
                  sort_keys=True,
                  ensure_ascii=False,
                  cls=NumpyEncoder)
    return data_dict
