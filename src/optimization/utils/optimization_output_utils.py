import numpy as np
import pandas as pd
import os
from utils.optimization_utils import budget_constraint, objective_function
import logging


def write_output(optimal_impressions, params, excl_df, file_path):
    """
    Writes the output of the optimization to a csv
    :param channel_campaigns: channel_campaign names
    :param T: matrix to aggregate channel_campaign_state ms by channel_campaign
    :param x: the optimal impression values (impression)
    :param CPI: median/average dollar cost per impression (1 x channel_campaign)
    :param file_path: file path to save the output
    :param upper: upper bounds of spend for channel_campaign
    :param lower: lower bounds of spend for channel_campaign
    """
    output_df = pd.DataFrame({
        "channel_campaigns": params.channel_campaigns,
        "category": params.cat,
        "impressions": np.dot(optimal_impressions, params.T),
        "CPI": params.CPI,
    })

    output_df["spend"] = output_df["impressions"] * output_df["CPI"]
    output_df["spend"] = output_df["spend"].apply(lambda x: round(x))

    bound_df = pd.DataFrame({"channel_campaigns": params.channel_campaigns,
                             "lower": params.lower, "upper": params.upper})
    excl_df["impressions"] = None
    excl_df["CPI"] = None
    excl_df = excl_df[["channel_campaigns",
                       "category", "impressions", "CPI", "spend"]]
    output_df = pd.concat([output_df, excl_df], ignore_index=True)
    full_df = pd.merge(output_df, bound_df, how="left", on="channel_campaigns")
    full_df = full_df.sort_values(by="spend", ascending=False)
    full_df.to_csv(file_path, index=False)


def make_output_file_path(output_file_path):
    """
    Create a subdirectory in the optimization repository for output
    :param output_file_path: file path to subdirectory
    """
    if not os.path.exists(output_file_path):
        os.makedirs(output_file_path)


def get_budget_performance(budget_dict, params, optimal_impressions):
    """
    Get the total spend for each budget category to ensure
    the constraint is met 
    :param budget_dict: dictionary of budget information
    :param params: optimization parameters
    :param optimal_impressions: the optimal impressions for each input combination
    """
    # Get performance metrics for the budgets
    for cat in budget_dict.keys():
        t_matrix = budget_dict[cat]["T"]
        budget_value = budget_constraint(
            optimal_impressions, params.CPI, t_matrix)
        budget_dict[cat]["optimized_value"] += budget_value
    return budget_dict


def conversion_per_output(x, params):
    """
    Find the conversions for each output combination
    from the objective function (without summing by output combination)
    :param optimal_impressions: the optimal impressions for each input combination
    :param params: optimization parameters
    :return conversions_per_output: optimal conversions aggregated per output
    """
    impressions_tothe_alpha = np.power(
        np.dot(params.S, np.diag(x)), params.alpha)
    conversions_per_output = np.nanprod(impressions_tothe_alpha, axis=1)
    return conversions_per_output


def aggregate_weekly_results(res, config, params):
    """
    Aggregate the optimization response (res) by week
    to get the quarterly metrics
    :param res: optimization response
    :param config: optimization configuration dictionary
    :param params: optimization parameters
    """
    # Iterate for every week in a quarter
    # to get the optimal budget for that week and aggregate
    if config["optimize_all_weeks_at_once"]:
        num_iter = range(len(res))
    else:
        num_iter = range(config["number_weeks_in_quarter"])
        res = [res] * config["number_weeks_in_quarter"]

    # Initialize reporting metrics
    impression_df = pd.DataFrame(params.impression_names,
                                 columns=["impression_names"])
    sales_per_output_df = pd.DataFrame(params.alpha_output_names,
                                       columns=["output_combination_names"])
    budget_metrics = params.budget_dict(spend_factor=1)
    total_marketing_sales = 0
    total_optimal_impressions = np.zeros(len(params.impression_init))
    sales_per_output_combination = np.zeros(len(params.alpha_output_names))

    for i in num_iter:
        # Get optimal impressions from optimized object
        optimal_impressions = res[i].x  # x = optimal impressions

        # -- Aggregate performance metrics by week
        # Get total spend for each category
        budget_metrics = get_budget_performance(
            budget_metrics, params, optimal_impressions)

        # Get total marketing sales
        total_marketing_sales += objective_function(
            optimal_impressions, params.alpha, params.S) * -1

        # Get optimal impressions per week
        impression_df[i] = optimal_impressions

        # Aggregate optimal impressions to get total optimal impressions
        total_optimal_impressions = np.add(optimal_impressions,
                                           total_optimal_impressions)
        sales_per_output_combination = np.add(sales_per_output_combination,
                                              conversion_per_output(optimal_impressions, params))

    sales_per_output_df["total_sales"] = sales_per_output_combination
    return budget_metrics, total_marketing_sales, impression_df, \
        total_optimal_impressions, sales_per_output_df


def add_back_excluded_spend(params, budget_metrics):
    """
    If a channel_campaign has a minimum spend but it is not deemed
    'significant' by the model, it is not optimized, but it needs
    to be added back to the reported metrics with this function
    :param params: optimization parameters
    :param budget_metrics: dictionary with total spend per category
    """
    excl_df = pd.DataFrame(
        {
            "channel_campaigns": params.excl_ch_campaign_comb,
            "category": params.excl_ch_campaign_cat,
            "spend": params.excl_ch_campaign_lower,
        }
    )
    excluded_spend_df = excl_df.groupby("category").sum()
    total_excluded_spend = excluded_spend_df.sum()[0]
    if total_excluded_spend > 0:
        budget_metrics["total"]["optimized_value"] += total_excluded_spend
    for cat in excluded_spend_df.index:
        budget_value = excluded_spend_df[excluded_spend_df.index ==
                                         cat].values[0][0]
        budget_metrics[cat]["optimized_value"] += budget_value
    return budget_metrics, excl_df


def log_results(budget_metrics, total_marketing_sales, base):
    """
    Log the summary metrics for reference.
    :param param budget_metrics: dictionary with total spend per category
    :param total_marketing_sales: total quarterly sales due to marketing
    :param base: sales due to non-marketing efforts
    """
    # Print out the optimization results on the command line
    for key in budget_metrics:
        logging.info("{}: ${:,.0f}".format(key, round(
            budget_metrics[key]["optimized_value"])))
    logging.info(f"Marketing Sales: {total_marketing_sales: ,.0f}")
    total_sales = total_marketing_sales + base
    logging.info(f"Total Sales: {total_sales: ,.0f} ")


def report_output(res, params, config):
    """
    Main function to aggregate weekly output into quarterly output
    :param res: optimal result object from scipy minimize
    :param params: optimization parameters class
    :param config: configurations defined in optimiation_config.yaml
    """
    # Make output directory if not exists
    output_file_path = config["output_channel_camp_file_path"]
    make_output_file_path(output_file_path)

    budget_metrics, total_marketing_sales, impression_df, \
        total_optimal_impressions, sales_per_output_df = aggregate_weekly_results(
            res, config, params)

    # Add excluded category spend back to the category
    budget_metrics, excl_df = add_back_excluded_spend(params, budget_metrics)
    log_results(budget_metrics, total_marketing_sales, config["base"])

    # ------- Write out CSVs -------
    # CSV: Shows the spend breakdown of each channel_campaign
    write_output(total_optimal_impressions, params, excl_df, output_file_path +
                 "optimization_output.csv")

    # CSV: The optimal channel_campaign_state combinations for each optimization
    impression_df.to_csv(output_file_path + "optimal_impression_by_week.csv")

    # CSV: Gives the optimal number of conversions for each output combination
    sales_per_output_df.to_csv(output_file_path + "conversions_breakdown.csv")
