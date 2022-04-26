------------------------------------------------------------------
-- external data consolidation
DROP TABLE IF EXISTS mix_media_marketing.external_hist;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.external_hist AS
WITH tmp AS (
SELECT a.new_week AS week, a.year_iso, a.state, a.total_cnt AS cust_vol,
b.agent_score, c.awareness, c.consideration, c.affinity,
d.cci, e.cpi, f.gas, g.unrate
FROM mix_media_marketing.customer_volume a
LEFT JOIN mix_media_marketing.agent_score_hist_filter b
ON a.new_week = b.week
AND a.state = b.state
LEFT JOIN mix_media_marketing.brand_hist_raw c
ON a.YEAR = c.year
LEFT JOIN mix_media_marketing.cci_hist_raw d
ON a.YEAR = split_part(d.week, '-', 1)
AND a.MONTH = split_part(d.week, '-', 2)::int::varchar
LEFT JOIN mix_media_marketing.cpi_hist_raw e
ON a.YEAR = year(e.week::date)
AND a.MONTH = month(e.week::date)
LEFT JOIN mix_media_marketing.gas_hist_raw f
ON a.YEAR_iso = year_iso(f.week::date)
AND a.week_iso = week_iso(f.week::date)
LEFT JOIN mix_media_marketing.unrate_hist_raw g
ON a.YEAR = year(g.week::date)
AND a.MONTH = month(g.week::date)
)
SELECT * FROM tmp 
