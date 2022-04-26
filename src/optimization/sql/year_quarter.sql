SELECT DISTINCT year_iso(week::date) AS year,
quarter(week::date) AS quarter
FROM mix_media_marketing.impression_spend