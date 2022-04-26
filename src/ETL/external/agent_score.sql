------------------------------------------------------------------
-- agent score
DROP TABLE IF EXISTS mix_media_marketing.agent_score_hist;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.agent_score_hist AS
WITH tmp AS (
select
      afv.ADVSR_SID
      ,advsr.BP_ID
      ,advsr.FULL_NM
      ,split_part(advsr.HM_ZIPCDE, '-', 1)::int::varchar AS zip
    ,afv.ADVSR_CLS_DESC
      ,afv.SLS_RPTG_LOB_CDE
      ,afv.CYCLE_DT

    ,sum(afv.WTD_PREM_AMT) WTD_PREM_AMT -- agent score
from PROD_USIG_CRCOG_DM_RPTG_VW.PREMIUM_FACT_VW afv
left join prod_usig_crcog_dm_rptg_vw.ADVSR_VW advsr
   ON advsr.ADVSR_SID = afv.ADVSR_SID
---Make sure this is MMFA channel
left join PROD_USIG_CRCOG_DM_RPTG_VW.AGCY_AFFLTN_VW d
    on afv.AGCY_AFFLTN_ID = d.AGCY_AFFLTN_ID
where d.DTRB_CHNL_CDE='CAS'
--protection product only
and afv.SLS_RPTG_LOB_CDE IN ('LIFE','DI','LTC')
--AND afv.CYCLE_DT::date BETWEEN '2017-10-01' AND '2020-09-30'
AND advsr.HM_ZIPCDE NOT IN ('', 'UNKWN', 'UNK', 'M2M 3Z7', 'V7T 1G8', 'K2E 6Y4', 'L4J 3L5', 'M3C 2T2', 'R7B 3N4', 'T5J 3R8',
'131 50', 'IPI4EE', 'N6A 1K7', 'V0C 1X0', 'L6M 2H5', 'V8Z 7J1')
group by
      afv.ADVSR_SID
      ,advsr.BP_ID
      ,advsr.FULL_NM
      ,advsr.HM_ZIPCDE
    ,afv.ADVSR_CLS_DESC
      ,afv.SLS_RPTG_LOB_CDE
      ,afv.CYCLE_DT
)
,zip_state AS (
SELECT zip, state
FROM mix_media_marketing.csv_zip_state
GROUP BY 1,2
)
,joined AS (
SELECT a.*, b.state
FROM tmp a
JOIN zip_state b
ON a.zip = b.zip
)
SELECT CONCAT(concat(year_iso(CYCLE_DT::date), '-'), week_iso(CYCLE_DT::date)) AS week,
state, avg(WTD_PREM_AMT) AS agent_score
FROM joined
WHERE CYCLE_DT::date BETWEEN '2017-10-01' AND '2021-09-30'
GROUP BY 1,2

------------------------------------------------------------
-- filter state only in US
DROP TABLE IF EXISTS mix_media_marketing.agent_score_hist_filter;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.agent_score_hist_filter AS
SELECT a.*
FROM mix_media_marketing.agent_score_hist a
JOIN (SELECT state FROM mix_media_marketing.state_2letter
GROUP BY 1) b
ON a.state = b.state
