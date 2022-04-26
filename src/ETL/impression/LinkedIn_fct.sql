DROP TABLE IF EXISTS mix_media_marketing.LinkedIn_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.LinkedIn_fct AS
select * from mix_media_marketing.linkedin GROUP BY 1 ORDER BY 1;
