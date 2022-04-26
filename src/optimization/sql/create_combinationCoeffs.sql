
-- combinationCoeffs
CREATE TABLE IF NOT EXISTS mix_media_marketing.optimal_combinationCoeffs (
    output varchar,
    input varchar,
    coeff float,
    channel_campaign varchar
);
TRUNCATE TABLE mix_media_marketing.optimal_combinationCoeffs;
COPY mix_media_marketing.optimal_combinationCoeffs
FROM LOCAL '/Users/MM*****/Documents/github/marketing_mix_model/src/data/q4_model_refresh/combinationCoeffs.csv' PARSER fcsvparser();
-- Replace file path with local combinationCoeffs.csv directory ^