from utils.optimization_utils import add_budget_constraints, \
    add_channel_campaign_constraints
from utils.optimization_input_utils import OptimizationParameters
import pytest


@ pytest.mark.parametrize(
    "budget_constraint_type,number_of_constraints",
    [("total", 1),
     ("category", 4),
     ("none", 0),
     (pytest.param("invalid_input", 0, marks=pytest.mark.xfail(raises=ValueError)))
     ]
)
def test_add_budget_constraints(budget_constraint_type, number_of_constraints):
    params = OptimizationParameters(json_file_path="tests/test_data.json")
    const = add_budget_constraints(budget_constraint_type, [], params, 1)
    assert len(const) == number_of_constraints


@ pytest.mark.parametrize(
    "channel_campaign_constraint,number_of_constraints",
    [("upper_lower", 2),
     ("equality", 1),
     ("none", 0),
     (pytest.param("invalid_input", 0, marks=pytest.mark.xfail(raises=ValueError))),
     ]
)
def test_add_channel_campaign_constraints(channel_campaign_constraint, number_of_constraints):
    params = OptimizationParameters(json_file_path="tests/test_data.json")
    const = add_channel_campaign_constraints(
        channel_campaign_constraint, [], params, 1)
    assert len(const) == number_of_constraints
