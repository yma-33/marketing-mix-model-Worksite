import math
import numpy as np
import os
from mmpac import s3_setup, disk_to_s3
# Custom utility functions
from optimize_matrices import impression_lower_bounds, run_optimization
from utils.optimization_utils import objective_function, budget_constraint, \
    lower_or_equality_constraint, upper_constraint
from utils.error_handling import UpperBoundError, LowerBoundError
from utils.optimization_input_utils import OptimizationParameters, \
    import_config, read_json
import pandas as pd
import pytest


def test_run_optimization():
    """
    Integration test which runs the entire optimization with mock data
    defined in the test_optimization_config.yaml and test_data.json
    and ensures the total budget is met
    """
    run_optimization("tests/test_optimization_config.yaml")
    output_df = pd.read_csv("tests/output/test/optimization_output.csv")
    total_spend = sum(output_df["spend"].values)
    assert round(total_spend, 0) == 4000


def test_import_config():
    """
    config is imported as a dictionary
    """
    test_file = "tests/test_optimization_config.yaml"
    config = import_config(test_file)
    assert(type(config) is dict)


@pytest.mark.parametrize(
    "error",
    [(pytest.param(UpperBoundError, marks=pytest.mark.xfail(raises=UpperBoundError))),
     (pytest.param(LowerBoundError, marks=pytest.mark.xfail(raises=LowerBoundError)))
     ]
)
def test_LowerBoundError(error):
    """
    UpperBoundError and LowerBoundError must raise exceptions
    """
    budget_cat = "Brand"
    upper_bound = 100
    real_lower = 300
    with pytest.raises(error):
        raise error(budget_cat, upper_bound, real_lower)


def test_Error_strings():
    """
    Test custom error strings are as expected 
    """
    budget_cat = "Brand"
    upper_bound = 100
    real_lower = 300
    lower_output = LowerBoundError(
        budget_cat, upper_bound, real_lower).__str__()
    expected_lower = f"""\nINFEASIBLE SOLUTION BOUND ERROR:
                category total upper bound: "Brand" for the
                current quarter $300.0  must be < $100.0 
                due to the given upper bounds to have a feasible solution.
                Please update the budget for "Brand" in the optimization_config.yaml"""
    upper_output = UpperBoundError(
        budget_cat, upper_bound, real_lower).__str__()

    expected_upper = f"""\n"Brand" category total upper bound 
                $100.0 should be <= $300.0"""
    assert all([lower_output == expected_lower,
               upper_output == expected_upper])


@ pytest.mark.parametrize(
    "impression_input,expected_array",
    [([1, 1, 1, 1], ((1e-7, None), (1e-7, None), (1e-7, None), (1e-7, None))),
     ([1, 2, 3], ((1e-7, None), (1e-7, None), (1e-7, None))),
     ([1, 2], ((1e-7, None), (1e-7, None)))]
)
def test_impression_lower_bounds(impression_input, expected_array):
    bound_output = impression_lower_bounds(impression_input)
    assert bound_output == expected_array


@ pytest.mark.parametrize(
    "budget,spend_factor,expected",
    [(10, .2, 2),
     (20, .1, 2),
     (1, .9, 0.9),
     (10.5, .03, 0.315),
     (pytest.param(10, 2, 20, marks=pytest.mark.xfail(raises=ValueError))),
     ]
)
def test_update_budget_spend_factor(budget, spend_factor, expected):
    params = OptimizationParameters("tests/test_data.json")
    eval = params.update_budget_spend_factor(budget, spend_factor)
    assert eval == expected


# Test the optimization parameters

class TestOptimizationParameters:
    def setup_method(self):
        """
        Pull in all the outputs of the generate_data function
        """
        self.json_file_path = "tests/test_data.json"
        self.params = OptimizationParameters(self.json_file_path)

    def test_read_json(self):
        json = read_json(self.json_file_path)[0]
        assert(type(json) is dict)

    def test_budget_dict(self):
        budget_dict = self.params.budget_dict(0.2)
        all_true = []
        # Test all T matrices
        all_true += [np.array_equal(budget_dict["total"]["T"], self.params.T),
                     np.array_equal(
                         budget_dict["Social"]["T"], self.params.T_Social),
                     np.array_equal(
                         budget_dict["Brand"]["T"], self.params.T_Brand),
                     np.array_equal(
                         budget_dict["Search"]["T"], self.params.T_Search),
                     np.array_equal(budget_dict["CRM"]["T"], self.params.T_CRM)]
        # Test all Budgets
        all_true += [(budget_dict["total"]["budget"]
                      == self.params.budget * 0.2),
                     (budget_dict["Social"]["budget"]
                      == self.params.B_Social * 0.2),
                     (budget_dict["CRM"]["budget"]
                      == self.params.B_CRM * 0.2),
                     (budget_dict["Search"]["budget"]
                      == self.params.B_Search * 0.2),
                     (budget_dict["Brand"]["budget"]
                      == self.params.B_Brand * 0.2)
                     ]
        assert all(all_true)

    def test_alpha_shape(self):
        assert self.params.alpha.shape == self.params.S.shape

    def test_m_names_shape(self):
        assert len(self.params.impression_names) == len(
            self.params.impression_init)

    def test_channel_campaigns_shape(self):
        assert len(self.params.channel_campaigns) == len(self.params.T[0, :])

    def test_CPI_shape(self):
        assert (
            len(self.params.CPI)
            == len(self.params.lower)
            == len(self.params.upper)
            == len(self.params.T[0, :])
        )

    def test_objective_func(self):
        # TEST 1: saving matricies as variables instead of a nested approach
        impression = range(1, len(self.params.impression_init) + 1)
        dot_S = np.dot(self.params.S, np.diag(impression))
        powr = np.power(dot_S, self.params.alpha)
        npd = np.nanprod(powr, axis=1)
        obj_function_test_1 = npd.sum() * -1

        # TEST 2: Iterate by S and get the objective function that way
        obj_function_test_2 = 0
        for i in range(len(self.params.S)):
            obj_function_test_2 += np.nanprod(
                np.power(
                    np.dot(self.params.S[i, :], np.diag(impression)
                           ), self.params.alpha[i, :]
                )
            )
        obj_function_test_2 *= -1

        # TEST 3: Iterate without S
        obj_function_test_3 = 0
        m_prod = 1
        for k in range(len(self.params.alpha[:, 0])):  # output combinations
            for j in range(len(self.params.alpha[0, :])):  # input combinations
                if not math.isnan(self.params.alpha[k][j]):
                    m_prod *= impression[j] ** self.params.alpha[k][j]
            obj_function_test_3 += m_prod
            m_prod = 1
        obj_function_test_3 *= -1

        # ACTUAL objective function
        obj_function_real = objective_function(
            impression, self.params.alpha, self.params.S
        )

        assert (
            obj_function_test_1
            == obj_function_test_2
            == obj_function_real
            == obj_function_test_3
        )

    def test_budget_constraint(self):
        """
        Test the budget_constraint() method against manual
        calculation
        """
        impression = range(1, len(self.params.impression_init) + 1)
        manual_budget = (
            self.params.CPI[0] * (impression[0] + impression[1])
            + self.params.CPI[1] * impression[2]
        )
        actual_budget = budget_constraint(
            impression, self.params.CPI, self.params.T)
        assert manual_budget == actual_budget

    def test_lower_or_equality_constraint(self):
        """
        Test the lower_or_equality_constraint() method against manual
        calculation
        """
        impression = range(1, len(self.params.impression_init) + 1)

        manual_bounds = np.array(
            [
                self.params.CPI[0] * (impression[0] + impression[1]) -
                self.params.lower[0],
                self.params.CPI[1] * impression[2] - self.params.lower[1],
            ]
        )
        method_bounds = lower_or_equality_constraint(
            impression, self.params.CPI, self.params.T, self.params.lower)
        assert all(manual_bounds == method_bounds)

    def test_upper_constraint(self):
        """
        Test the upper_constraint() method against manual
        calculation
        """

        impression = range(1, len(self.params.impression_init) + 1)

        manual_bounds = np.array(
            [
                self.params.upper[0] - self.params.CPI[0] *
                (impression[0] + impression[1]),
                self.params.upper[1] - self.params.CPI[1] * impression[2],
            ]
        )
        method_bounds = upper_constraint(impression, self.params.CPI,
                                         self.params.T, self.params.upper)
        assert all(manual_bounds == method_bounds)
