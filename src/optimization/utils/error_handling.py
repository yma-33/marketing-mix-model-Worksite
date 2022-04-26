class Error(Exception):
    """Base class for exceptions in this module."""
    pass


class UpperBoundError(Error):
    def __init__(self, budget_cat, upper_bound, real_upper):
        self.budget_cat = budget_cat
        self.upper_bound = upper_bound
        self.real_upper = real_upper

    def __str__(self):
        return f"""\n"{self.budget_cat}" category total upper bound 
                ${self.upper_bound:,.1f} should be <= ${self.real_upper:,.1f}"""


class LowerBoundError(Error):
    def __init__(self, budget_cat, upper_bound, real_lower):
        self.budget_cat = budget_cat
        self.upper_bound = upper_bound
        self.real_lower = real_lower

    def __str__(self):
        return f"""\nINFEASIBLE SOLUTION BOUND ERROR:
                category total upper bound: "{self.budget_cat}" for the
                current quarter ${self.real_lower:,.1f}  must be < ${self.upper_bound:,.1f} 
                due to the given upper bounds to have a feasible solution.
                Please update the budget for "{self.budget_cat}" in the optimization_config.yaml"""
