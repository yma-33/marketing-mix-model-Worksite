SELECT TO_DATE(week,'YYYY-IW') AS Week, *
FROM mix_media_marketing.external_hist_2021Q3 
WHERE week != '2017-39'