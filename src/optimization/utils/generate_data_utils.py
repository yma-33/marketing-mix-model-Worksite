import pandas as pd
import numpy as np
from utils.error_handling import LowerBoundError
from mmpac import *
import datetime as dt
import logging


def read_in_coefficients(file_path, output_alphas, upper_col, stakeholder_bounds,
                         get_file_from="vertica", vertica_file_path="../sql/combinationCoeffs.sql"):
    """
    Reads in the attribution coefficients (alphas) generated from the MMM
    model.
    :param file_path: path/to/combinationCoeffs.csv generated from final_model.R
    :param output_alphas: list of alpha dimensions i.e. ["lob", "segment", "state"]
    :param get_file_from: method of getting the alphas (file_path, vertica, mock_data)
    :return alpha_output_names: list of output combination names i.e. lob_state_segment
    :return alpha: matrix of attribution coefficients (output combination x input combination)
    :return impression_names: list of input combination names
    :return S: matrix that maps input combinations to each output combination
                this assigns a 1 to every time an input combination appears in an
                output comination
    """
    # Get combinationCoeffs (alphas) from model
    if get_file_from == "file_path":
        df = pd.read_csv(file_path)
        df = df.merge(stakeholder_bounds, left_on='channel_campaign',
                      right_on='CHANNEL_CAMPAIGN')
        df = df[(df[upper_col] > 0) & (df["coeff"] > 0)]
        logging.info(f"using alphas from: {file_path} ")
    elif get_file_from == "vertica":
        sql_text = read_sql(vertica_file_path)
        df = get_query(sql_text.format(budget_max_column=upper_col))
    elif get_file_from == "mock_data":
        sql_text = read_sql("../optimization/sql/mock_combinationCoeffs.sql")
        df = get_query(sql_text.format(budget_max_column=upper_col))

    # Split output dimensions
    df[['lob', 'state', 'segment']] = df['output'].str.split('_',
                                                             2,
                                                             expand=True)
    # Create an output combination column
    if len(output_alphas) > 1:
        df["output_comb"] = df[output_alphas].agg('_'.join, axis=1)
    else:
        df["output_comb"] = df[output_alphas]

    # Get channel_campaign combinations
    ch_camp_df = df[["input", "channel_campaign"]].copy().drop_duplicates()

    # Pivot for every output dimension
    df = df.pivot(index='output_comb', columns='input', values='coeff')

    # Get optimization inputs
    alpha = df.values.tolist()  # attribution coefficients from the model
    impression_names = df.columns.values.tolist()  # input combination names
    alpha_output_names = df.index.values.tolist()  # output combination names

    # Get channel_campaign combinations (ch_camp_comb)
    get_ch_camp_comb = pd.DataFrame({"impression_names": impression_names}).merge(
        ch_camp_df, how="left", left_on="impression_names", right_on="input").drop_duplicates()
    m_names_before_pivot = get_ch_camp_comb["input"].values.tolist()
    ch_camp_comb = get_ch_camp_comb["channel_campaign"].values.tolist()
    if impression_names != m_names_before_pivot:
        raise ValueError("INPUT ORDER DOESN'T MATCH")

    # Map input combinations to outputs
    S = df.where(df.isnull(), 1).fillna(0).values.tolist()

    return alpha_output_names, alpha, impression_names, S, ch_camp_comb


def get_channel_campaign_mappings(impression_names, ch_camp_names,
                                  parent_dic=None, parent=None):
    """
    :param impression_names: list of names of channel_campaign state
    :param ch_camp_names: list of channel_campaign names for each m_name
    :param parent_dic: dictionary that maps channel_campaign to category
    :param parent: the current category (if any)
    :return channel_campaign_combinations: list of channel_campaign names
    :return T: matrix that maps impression_names to channel_campaign_combinations
    """
    # Map a '1' to every time a channel_campaign_state is in a channel_campaign
    # either in general or for a specific category
    if parent is None:
        map = [1] * len(impression_names)
    else:
        map = [1 if x.split('_')[0] in parent_dic[parent]
               else 0 for x in impression_names]

    map_ch_cp = pd.DataFrame({
        'impression_names': impression_names,
        'ch_camp_names': ch_camp_names,
        'map': map})

    # Pivot to a matrix of (channel_campaign_state x channel_campaign)
    map_ch_cp = map_ch_cp.pivot(index='impression_names',
                                columns='ch_camp_names',
                                values='map').fillna(0)
    channel_campaign_combinations = map_ch_cp.columns.values.tolist()
    T = map_ch_cp.values.tolist()

    if parent is None:
        return channel_campaign_combinations, T
    else:
        return T


def get_upper_lower_bounds(ch_camp_comb, lower_col, upper_col, df_bounds):
    """
    Get the upper bounds and lower bounds for each channel_campaign combination.
    :param ch_camp_comb: channel_campaign combinations
    :param lower_col: column name of lower bound
    :param upper_col: column name of upper bound
    :param df_bounds: dataframe of stakeholder bounds
    :return upper: list of channel_campaign upper bounds
    :return lower: list of channel_campaign lower bounds
    :return cat: list of channel_campaign categories
    :return df_lower_extr: dataframe of excluded lower bounds
    """
    # Iterate through channel_campagin combinations to get channel_campaign
    # lower and upper bounds
    upper = []
    lower = []
    cat = []
    channel_campaign_col = "CHANNEL_CAMPAIGN"
    category_col = "CATEGORY"
    for i in ch_camp_comb:
        if i in list(df_bounds[channel_campaign_col].values):
            curr_row_bounds = df_bounds[df_bounds[channel_campaign_col] == i]
            cat += [curr_row_bounds[category_col].values[0]]
            lower += [curr_row_bounds[lower_col].values[0]]
            upper += [round(curr_row_bounds[upper_col].values[0], 0)]
        else:  # no bounds for this channel_campaign combination
            lower += [0]
            upper += [0]
            cat += ["NaN"]
    return upper, lower, cat


def check_upper_bounds(cat, upper, B_Social, B_Brand, B_CRM, B_Search):
    """
    Ensure that the sum of the model-significant channel_campaign combinations
    upper bound for each category is at least >= the budget for that category
    otherwise the optimization will NEVER CONVERGE due to infeasibility
    :param cat: list of categories for each channel_campaign
    :param upper: list of upper budgets for each channel_campaign
    :param B_Social, B_Brand, B_CRM, B_Search: budget for categories
    """
    # Get the total possible spend for each channel_campaign category
    category_col = "category"
    upper_col = "upper"
    cat_df = pd.DataFrame({category_col: cat, upper_col: upper})
    total_upper = cat_df.groupby(category_col)[
        upper_col].agg("sum").reset_index()

    B_dict = {"Social": B_Social,
              "Brand": B_Brand,
              "CRM": B_CRM,
              "Search": B_Search}

    # Iterate through each category and ensure the sum of the upper bounds
    # for that category is greater than or equal to the total budget available
    # to that category.
    for cat_i in total_upper[category_col].values:
        logging.info(f"Checking bounds for {cat_i}...")
        sum_of_upper_for_cat = total_upper[total_upper[category_col]
                                           == cat_i][upper_col].values[0]
        total_category_budget = B_dict[cat_i]
        if sum_of_upper_for_cat < total_category_budget:
            raise LowerBoundError(cat_i, sum_of_upper_for_cat,
                                  total_category_budget)


def get_CPI(ch_camp_comb, cpi_agg_method, curr_year, curr_quarter):
    """
    Generates a list of the cost per impression (CPI) for each channel_campaign
    combination based on CPI.sql
    :param ch_camp_comb: The list of channel_campaign combinations
    :param cpi_agg_method: method of deriving the cost per impression
    :param curr_year: current year of the optimization
    :param curr_quarter: current quarter of the optimization
    :return CPI: list of cost per impression for each channel_campaign combination
    """
    # Get different time dimensions for different CPI calculations
    year_quarter_df = get_query(sql=read_sql("sql/year_quarter.sql"))
    all_years = tuple(year_quarter_df["year"].unique())
    all_quarters = tuple(year_quarter_df["quarter"].unique())
    previous_yr_sql = "(" + str(curr_year - 1) + ")"
    curr_quarter_sql = "(" + str(curr_quarter) + ")"

    sql_text = read_sql("sql/CPI.sql")
    # Current quarter CPI from previous year
    CPI_table_curr_qtr = get_query(sql_text.format(sql_year=previous_yr_sql,
                                                   sql_quarter=curr_quarter_sql))
    # Aggregate CPI for all quarters that match current
    CPI_table_all_qtr = get_query(sql_text.format(sql_year=all_years,
                                                  sql_quarter=curr_quarter_sql))
    # Get total CPI across all years/quarters
    CPI_table_tot = get_query(sql_text.format(sql_year=all_years,
                                              sql_quarter=all_quarters))
    # config cpi_agg_method selects what to do for CPI
    if cpi_agg_method == "curr_quarter":
        CPI_table_select = CPI_table_curr_qtr
    elif cpi_agg_method == "all_quarters":
        CPI_table_select = CPI_table_all_qtr
    elif cpi_agg_method == "total":
        CPI_table_select = CPI_table_tot

    # Get the CPI from the selected method. If it doesn't exists,
    # we default to the total aggregation method
    ch_camp_col = "comb"  # channel_campaign column
    cpi_col = "CPI"
    CPI = []
    for i in ch_camp_comb:
        if i not in CPI_table_select[ch_camp_col].values:
            select_row = CPI_table_tot[CPI_table_tot[ch_camp_col] == i]
        else:
            select_row = CPI_table_select[CPI_table_select[ch_camp_col] == i]
        if len(select_row) > 0:
            if select_row[cpi_col].iloc[0] == 0:
                CPI += [CPI_table_tot[CPI_table_tot[ch_camp_col] == i]
                        [cpi_col].iloc[0]]
            else:
                CPI += [select_row[cpi_col].iloc[0]]
        else:
            logging.debug(f"{i} CPI not found...defaulting to 0")
            CPI += [0]

    return CPI


def stack_objective_function_data(curr_year, curr_quarter, S, alpha, impression_init, impression_names, T):
    """
    Transform objective function matrices for optimization_type: "multiple_optimization"
    :param curr_year: current optimization year
    :param curr_quarter: current optimization quarter
    :param S: matrix that maps impressions (impression_init) to output names (impression_names)
    :param alpha: matrix of alpha coefficients
    :param impression_init: impressions array that is being optimized
    :param impression_names: names of the impressions array (channel_campaign_state)
    """
    start_date = dt.datetime(curr_year, 3 * curr_quarter - 2, 1)
    end_date = dt.datetime(curr_year + 3 * curr_quarter // 12,
                           3 * curr_quarter % 12 + 1, 1) + dt.timedelta(days=-1)
    num_weeks = abs(end_date - start_date).days // 7
    logging.info("Stacking matrices by %i weeks..." % num_weeks)
    # stack arrays onto themselves for T, S, alpha,
    S = np.tile(S, (1, num_weeks))
    alpha = np.tile(alpha, (1, num_weeks))
    impression_init = np.tile(impression_init, num_weeks)
    impression_names = np.tile(impression_names, num_weeks)
    T = np.tile(T, (num_weeks, 1))
    return S, alpha, impression_init, impression_names, T


def get_excluded_lower_bounds(df_bounds, lower_col, ch_camp_comb,
                              B_Social, B_Brand, B_CRM, B_Search):
    """
    If a lower bound is nonzero but deemed insignificant by our model,
    the optimization will save the lower bound in a dataframe that's
    added to the model results.
    :param df_bounds: dataframe with the original channel_campaign bounds
    :param lower_col: column name with the lower bounds
    :param ch_camp_comb: channel_campaign combinations in the optimization
    :param B_Social, B_Brand, B_CRM, B_Search: budgets for categories
    :return extra_lower_df: dataframe of lower bounds that were excluded
        by the model that need to be included in the optimization output
    """

    # Find any channel_campaign combinations that have a non-zero lower bound
    ch_camp_col = "CHANNEL_CAMPAIGN"  # channel_campaign column name
    cat_col = "CATEGORY"  # category column name
    nonzero_lower_bound = df_bounds[df_bounds[lower_col]
                                    > 0][ch_camp_col].values

    # Find any channel_campaign lower bounds that are not in the channel_campaign
    # combinations of the optimization (that were deemed insignificant by the
    # model)
    excluded_lwr_bounds = list(set(nonzero_lower_bound) - set(ch_camp_comb))

    # If there are lower bounds excluded from the model, the subtract that
    # budget from the current budget category
    excl_ch_campaign_comb = []
    excl_ch_campaign_cat = []
    excl_ch_campaign_lower = []
    if len(excluded_lwr_bounds) > 0:
        extra_lower_df = df_bounds[
            (df_bounds[ch_camp_col].isin(excluded_lwr_bounds)) &
            (df_bounds[lower_col] > 0)]
        df_lower_extr = extra_lower_df[[ch_camp_col, cat_col, lower_col]]
        excluded_spend_df = df_lower_extr.groupby(cat_col).sum()
        if "Social" in excluded_spend_df.index:
            B_Social -= excluded_spend_df[excluded_spend_df.index ==
                                          "Social"].values[0][0]
        if "Brand" in excluded_spend_df.index:
            B_Brand -= excluded_spend_df[excluded_spend_df.index ==
                                         "Brand"].values[0][0]
        if "CRM" in excluded_spend_df.index:
            B_CRM -= excluded_spend_df[excluded_spend_df.index ==
                                       "CRM"].values[0][0]
        if "Search" in excluded_spend_df.index:
            B_Search -= excluded_spend_df[excluded_spend_df.index ==
                                          "Search"].values[0][0]
        excl_ch_campaign_comb = df_lower_extr[ch_camp_col].values
        excl_ch_campaign_cat = df_lower_extr[cat_col].values
        excl_ch_campaign_lower = df_lower_extr.iloc[:, 2].values
    return excl_ch_campaign_comb, excl_ch_campaign_cat, excl_ch_campaign_lower, \
        B_Social, B_Brand, B_CRM, B_Search


def pull_in_stakeholder_bounds(data_method, file_path):
    """
    Reads in the stakeholder bounds.
    :param data_method: method of retrieving stakeholder bounds dataframe
    :param file_path: path/to/stakeholderbounds.csv
    :lower_col: column name of lower bound in stakeholder bounds
    :upper_col: column name of upper bound in stakeholder bounds
    :return df_bounds: dataframe of stakeholder bounds
    """
    if data_method == "file_path":
        df_bounds = pd.read_csv(file_path)
    else:
        df_bounds = get_query(read_sql("sql/stakeholder_bounds.sql"))
    return df_bounds
