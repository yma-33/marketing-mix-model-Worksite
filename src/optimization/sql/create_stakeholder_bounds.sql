-- stakeholder_bounds
CREATE TABLE IF NOT EXISTS mix_media_marketing.stakeholder_bounds (
    CHANNEL_CAMPAIGN varchar,
    CATEGORY varchar,
    Q3_2020_BUDGET_MIN float,
    Q4_2020_BUDGET_MIN float,
    Q1_2021_BUDGET_MIN float,
    Q2_2021_BUDGET_MIN float,
    Q3_2021_BUDGET_MIN float,
    Q4_2021_BUDGET_MIN float,
    Q3_2020_BUDGET_MAX float,
    Q4_2020_BUDGET_MAX float,
    Q1_2021_BUDGET_MAX float,
    Q2_2021_BUDGET_MAX float,
    Q3_2021_BUDGET_MAX float,
    Q4_2021_BUDGET_MAX float
);
TRUNCATE TABLE mix_media_marketing.stakeholder_bounds;
COPY mix_media_marketing.stakeholder_bounds
FROM LOCAL '/Users/MM*****/Documents/github/marketing_mix_model/src/optimization/stakeholder_bounds.csv' PARSER fcsvparser();
-- Replace file path with local stakeholder_bounds.csv directory ^