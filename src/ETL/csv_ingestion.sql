----------------------------------------
-- DMA to state mapping
DROP TABLE IF EXISTS mix_media_marketing.county_dma;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.county_dma
(STATE varchar, COUNTY varchar, STATEFP int, CNTYFP float, CNTYTVHH float, DMAINDEX float, DMA varchar);
COPY mix_media_marketing.county_dma FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/DMA/county_dma.csv' PARSER fcsvparser();

-----------------------------------------
-- dma to dma code
DROP TABLE IF EXISTS mix_media_marketing.dma_state_factor_code;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.dma_state_factor_code
(dma varchar, code varchar);
COPY mix_media_marketing.dma_state_factor_code FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/DMA/dma_state_factor_code.csv' PARSER fcsvparser();

---------------------------------------------------------------
-- Email and direct mail mapping dictionary
---------------------------------------------------------------
DROP TABLE IF EXISTS mix_media_marketing.csv_email_dmail_campaign_prod;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.csv_email_dmail_campaign_prod
(date_added varchar, campaign_name varchar, purpose varchar, CTA varchar, product varchar, line_of_business varchar, hierarchy_1 varchar,
hierarchy_2 varchar, hierarchy_3 varchar, hierarchy_4 varchar, medium varchar, campaign_id varchar);
COPY mix_media_marketing.csv_email_dmail_campaign_prod FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/MMFA/Email_Print/Campaign_product_map table 12.09.20.csv' PARSER fcsvparser();

---------------------------------------------------------------
-- Social campaign to product mapping
---------------------------------------------------------------
DROP TABLE IF EXISTS mix_media_marketing.csv_social_campaign_prod;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.csv_social_campaign_prod
(campaign varchar, product varchar);
COPY mix_media_marketing.csv_social_campaign_prod FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/MMFA/Paid_social/campaign2prod/camp2prod_121020.csv' PARSER fcsvparser()

---------------------------------------------------------------
-- SEM campaign to product mapping
---------------------------------------------------------------
DROP TABLE IF EXISTS mix_media_marketing.csv_sem_campaign_prod;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.csv_sem_campaign_prod
(campaign varchar, product varchar);
COPY mix_media_marketing.csv_sem_campaign_prod FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/MMFA/SEM/SEM Product Translation_byCampaign_120420.csv' PARSER fcsvparser()

-----------------------------------------
-- state name to 2 letter state
DROP TABLE IF EXISTS mix_media_marketing.state_2letter;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.state_2letter
(state_name varchar, state varchar);
COPY mix_media_marketing.state_2letter FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/DMA/state_2letter.csv' PARSER fcsvparser();

-----------------------------------------
-- state county population
DROP TABLE IF EXISTS mix_media_marketing.state_county_population;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.state_county_population
(state_name varchar, county varchar, population float);
COPY mix_media_marketing.state_county_population FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/DMA/state_county_population.csv' PARSER fcsvparser();

----------------------------------------
-- state and population
DROP TABLE IF EXISTS mix_media_marketing.state_population;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.state_population
(state varchar, population float);
COPY mix_media_marketing.state_population FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/DMA/state_population.csv' PARSER fcsvparser();

-----------------------------------------
-- region state mapping
DROP TABLE IF EXISTS mix_media_marketing.csv_region_state;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.csv_region_state
(state varchar, region varchar);
COPY mix_media_marketing.csv_region_state FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/DMA/region.csv' PARSER fcsvparser();

-----------------------------------------
-- zip state mapping
DROP TABLE IF EXISTS mix_media_marketing.csv_zip_state;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.csv_zip_state
(zip varchar, state varchar);
COPY mix_media_marketing.csv_zip_state FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/DMA/zip_state.csv' PARSER fcsvparser();

----------------------------------------
-- Create mix_media_marketing.csv_dma_code_mapping;
DROP TABLE IF EXISTS mix_media_marketing.csv_dma_code_mapping;
CREATE TABLE IF NOT EXISTS mix_media_marketing.csv_dma_code_mapping (
    dma_name_raw varchar,
    dma_name varchar,
    dma_code int,
    state varchar
);

COPY mix_media_marketing.csv_dma_code_mapping
FROM
    LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/DMA/dma_code_mapping.csv' PARSER fcsvparser(
        TYPE = 'traditional',
        delimiter = ','
    );
