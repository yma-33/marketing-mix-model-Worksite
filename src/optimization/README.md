### MMM Scenario Planner Optimization

#### Optimization Method

The optimization method is described in detail in the Mix Media Model writeup on Confluence.

We selected `scipy.minimize()` to perform constrained optimization (**Note** although our problem is maximization, negative minimization is mathematically equivalent). Scipy selects the SLSQL minimization algorithm due to the constraints.

### Running optimization with model update:

1. save all csvs in final_model
1. calculate 'base' in R and update in config.yaml
1. update the budgets in `stakeholder_bounds.csv` and upload to Vertica
1. update the config.yaml
1. run `python3 optimize_matricies.py` (make sure you're in the virtual environment)

#### Running the Code

Be sure to update `optimization_config.yaml` with configurations for the optimization.

```bash
py optimize_matricies.py
```

#### Running the Unit Tests

```bash
pytest tests/
```
