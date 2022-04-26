
import yaml
import json
import numpy as np


def read_json(file_path):
    """
    Pulls in json file and creates a python dictionary
    :param file_path: file path for the optimization json file
    :output: A dictionary with format {(channel, campaign) : {"alpha =0self.data[0][".3, "lwr"...}
    """
    with open(file_path) as json_file:
        data = json.load(json_file)
    return data


def import_config(relative_fp):
    """
    Pulls in json file and creates a python dictionary
    :param file_path: file path for the optimization json file
    :output: A dictionary with format {(channel, campaign) : {"alpha =0self.data[0][".3, "lwr"...}
    """
    with open(relative_fp, "r") as stream:
        config = yaml.safe_load(stream)
    return config


class OptimizationParameters:
    """
    Class to hold all of the optimization parameters from the optimization_data.json 
    file.
    """

    def __init__(self, json_file_path="optimization_data.json"):
        self.budget_categories = ["total", "Social", "Brand", "CRM", "Search"]
        self.data = read_json(json_file_path)
        self.impression_names = self.data[0]["impression_names"]
        self.channel_campaigns = self.data[0]["channel_campaigns"]
        self.lower = self.data[0]["lower"]
        self.upper = self.data[0]["upper"]
        self.budget = self.data[0]["budget"]
        self.B_Social = self.data[0]["B_Social"]
        self.B_Brand = self.data[0]["B_Brand"]
        self.B_CRM = self.data[0]["B_CRM"]
        self.B_Search = self.data[0]["B_Search"]
        self.impression_init = self.data[0]["impression_init"]
        self.alpha_output_names = self.data[0]["alpha_output_names"]
        self.cat = self.data[0]["cat"]
        self.excl_ch_campaign_comb = self.data[0]["excl_ch_campaign_comb"]
        self.excl_ch_campaign_cat = self.data[0]["excl_ch_campaign_cat"]
        self.excl_ch_campaign_lower = self.data[0]["excl_ch_campaign_lower"]
        # Get numpy data
        self.alpha = np.asarray(self.data[0]["alpha"])
        self.CPI = np.asarray(self.data[0]["CPI"])
        self.S = np.asarray(self.data[0]["S"])
        self.T = np.asarray(self.data[0]["T"])
        self.T_Social = np.asarray(self.data[0]["T_Social"])
        self.T_Brand = np.asarray(self.data[0]["T_Brand"])
        self.T_CRM = np.asarray(self.data[0]["T_CRM"])
        self.T_Search = np.asarray(self.data[0]["T_Search"])

    def update_budget_spend_factor(self, budget, spend_factor):
        """
        update the budget with a 'spend factor' this will multiply
        the budget by some fraction to represent a current week's spend
        :param budget: total budget to be spent 
        :param spend_factor: fraction of budget spent in a given week
        """
        if spend_factor > 1 or spend_factor < 0:
            raise ValueError("Spend factor must be between 0 and 1")
        elif spend_factor != 1:
            return np.multiply(budget, spend_factor)
        else:
            return budget

    def budget_dict(self, spend_factor):
        """"
        Create a dictionary of the budgets, the T matrix that maps to
        channel, campaign categories, and the budget for that category 
        :param spend_factor: fraction of budget spent in a given week
        """
        categories = self.budget_categories
        budget_matrices = [self.T, self.T_Social,
                           self.T_Brand, self.T_CRM, self.T_Search]
        total_budgets = [self.budget, self.B_Social, self.B_Brand,
                         self.B_CRM, self.B_Search]
        budget_metrics = {}
        for i in range(len(categories)):
            budget_dict = {"optimized_value": 0,  # used in the report_output function
                           "T": budget_matrices[i],
                           "budget": self.update_budget_spend_factor(total_budgets[i],
                                                                     spend_factor)}
            budget_metrics[categories[i]] = budget_dict
        return budget_metrics
