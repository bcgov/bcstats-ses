# this file is used for read and save data to decimal database
# since we have data from statscan website using API, this decimal database has not been used for data loading yet.

if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(odbc, tidyverse, DBI, dbplyr,nanoarrow, arrow)

# odbcListDrivers()
# https://www.nickvasile.com/2020/05/13/error-invalid-descriptor-index/
# https://docs.snowflake.com/developer-guide/odbc/odbc-windows
# get a connection
# SQL server use windows authentication, as long as it is on your office laptop, it should be fine. Not test in mac yet.
# require VPN 2 connected.
# server information is stored in config. 
con <-dbConnect(odbc(),
                 Driver = config::get("Driver"),
                 Server = config::get("Server"),
                 Database = config::get("Database"),
                 # UID = "JDUAN",
                 Trusted_Connection = "True"
                )






# create a function for the preprocess
# SQL server requires the long text fields at the end of the select clause
read_sql_query = function(con, tbl_name, table_prefix, query = NULL) {
  # grab all column names
  all_cols <- odbcListColumns(con, tbl_name)
  # all_cols <- odbc::dbListFields(conn= con, name = tbl_name)
  # extract ntext columns
  long_cols = all_cols %>%
    dplyr::filter(type == "ntext") %>%
    pull(name)
  # extract text columns
  other_cols = all_cols %>%
    dplyr::filter(type == "text") %>%
    pull(name)
  # put ntext at the end
  long_cols = unique(c(other_cols, long_cols))
  
  # reorder the selection to not error
  if (is.null(query)){
    tab = dplyr::tbl(con, sql(glue::glue("SELECT *  FROM {table_prefix}.[{tbl_name}]"))) %>%
      dplyr::select(-tidyselect::any_of(long_cols),
                    tidyselect::everything(),
                    tidyselect::any_of(long_cols))
  } else {
    tab = dplyr::tbl(con, sql(query)) %>%
      dplyr::select(-tidyselect::any_of(long_cols),
                    tidyselect::everything(),
                    tidyselect::any_of(long_cols))
  }

}





# The commented code is an example for using decimal database
# query = glue::glue("SELECT *  FROM {table_prefix}.[FCT_CENSUS_2021_BC_DA]")
# 
# print("Use read_sql_query function to fetch the table")
# 
# 
# my_tbl = read_sql_query(
#   con,
#   tbl_name,
#   table_prefix,
#   query)
# 
# 
# print("The samples from table:FCT_CENSUS_2021_BC_DA:")
# my_tbl %>% glimpse()
# 
# print("If you see the samples, it means the SQL Server connection works. \n If you don't see the samples, it has problem to connect to the SQL Server.")
# 
# print("You can check out the query as well")
# 
# my_tbl %>% show_query()

# Show how many tables in database
dbListTables(con)
# many tables.
# list columns/fields in one table
dbListFields(con, "FCT_BC_POP_CSD_20012022")

# to understand the table and column names


# This is an example for loading a chunk at a time.
# pop_tbl = tbl(src = con, sql(pop_query))
# pop_tbl %>% glimpse()
# [Microsoft][ODBC SQL Server Driver]Invalid Descriptor Index


# res <- dbSendQuery(con, pop_query)
# res

# Or a chunk at a time

# while (!dbHasCompleted(res)) {
#   chunk <- dbFetch(res, n = 5)
#   print(nrow(chunk))
# }


########################################################################################################
#  # population estimates data from decimal database: source is Brett's team, Jonathan. 
# Brett/Jonathan provides CSD level population growth data with gender, age group infor.
# We only take the total number, 
########################################################################################################



pop_estimate_sql = read_sql_query(
  con,
  table_prefix,
  tbl_name = "FCT_BC_POP_CSD_20012022")

pop_estimate_sql %>% glimpse()

pop_estimate_cd = pop_estimate_sql %>% 
  count(CD) %>% 
  collect()
# 29 CDs

pop_estimate_csd = pop_estimate_sql %>% 
  count(CSD) %>% 
  collect()
# 737 CSDs, each has 66 data points, year, gender, combinations

pop_estimate_year = pop_estimate_sql %>% 
  count(Year) %>% 
  collect()
# 2001 to 2022

pop_estimate_sql %>% 
filter(CSD == "5901003") %>% 
  select(-starts_with("AGE"))

# 
pop_estimate_sex = pop_estimate_sql %>% 
  count(Sex) %>% 
  collect()
# 1 male, 2 is female, 3 is total


pop_estimate_type = pop_estimate_sql %>% 
  count(Type) %>% 
  collect()


pop_estimate_level = pop_estimate_sql %>% 
  count(Level) %>% 
  collect()



########################################################################################################
#  # DA median house price in decimal database 
# this table has postal code or address detail, so we can aggregate them up to DA level.


########################################################################################################


# A folio number is a unique identifier assigned to a property for tax assessment and billing purposes
# folio_address_query =  glue::glue("select 
# 	*, 
# 	case when substring(postal_code,4,1) <> '' then postal_code else concat(substring(postal_code,1,3),
# 	substring(postal_code,5,3)) end as PC
# from 
# 	{table_prefix}.[FCT_bca_folio_addresses_20240609]")
# 
# 
# folio_address_sql = read_sql_query(
#   con,
#   tbl_name = "FCT_bca_folio_addresses_20240609",
#   table_prefix,
#   folio_address_query)
# 
# folio_address_sql %>% glimpse()

# to do the aggregation, we need to join a couple of tables, which cannot be done in R easily, so we create a view in decimal database, and revieve the data from that view. 


# In Canada, the term "conveyance price" typically refers to the purchase price or sale price of a property in a real estate transaction. 
# The conveyance price is:
#   The amount agreed upon between the buyer and seller for the transfer of property ownership
# Used as the basis for calculating various fees and taxes associated with the property transaction

# dbExecute(con, glue::glue("DROP VIEW {table_prefix_new}.DA_MEDIAN_HOUSE_PRICE_VIEW;"))
# dbExistsTable(con, glue::glue("{table_prefix_new}.DA_MEDIAN_HOUSE_PRICE_VIEW;")) 
# this shows FALSE?


# DA_2021 is in short form such as 0154, and we should use long form such as 59010154, and it is the first 8 digits in DB_2021,


med_house_price_view_query = glue::glue("
create view DA_MEDIAN_HOUSE_PRICE_VIEW AS 
select distinct 
	substring(a.CONVEYANCE_DATE,1,4) as YEAR,
  [CD_2021],
  [CSD_2021],
	DA_2021,
	
	substring(d.[DB_2021],1,8) as DA_CODE, 
	PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY CONVEYANCE_PRICE) OVER (PARTITION BY substring(d.[DB_2021],1,8), substring(a.CONVEYANCE_DATE,1,4)) AS MedianPrice
  , MUN_NAME_2021 
from 
	{table_prefix}.[FCT_BCA_SALES_2024_06] a 
	left join {table_prefix}.[FCT_bca_folio_descriptions_20240609] b on a.FOLIO_ID=b.FOLIO_ID
	left join (
select 
	*, 
	case when substring(postal_code,4,1) <> '' then postal_code else concat(substring(postal_code,1,3),
	substring(postal_code,5,3)) end as PC
from 
	{table_prefix}.[FCT_bca_folio_addresses_20240609]
) c on a.FOLIO_ID=c.FOLIO_ID
	left join {table_prefix}.[FCT_TMF_202312] d on c.PC=d.POSTALCODE
	left join {table_prefix}.[DIM_TMF_CD_CMA_2021] on CMACA_2021=CMA
where 
	a.CONVEYANCE_TYPE_DESCRIPTION = 'Improved Single Property Transaction'
	and b.ACTUAL_USE_DESCRIPTION in ('Single Family Dwelling','Residential Dwelling with Suite',
	'Row Housing (Single Unit Ownership)','Duplex, Strata Side by Side',
	'Duplex Non-Strata Side by Side or Front / Back','Duplex, Strata Front / Back','Strata-Lot Residence (Condominium)')
	and substring(a.CONVEYANCE_DATE,1,6)>='200001'
    AND 	d.[DB_2021] is not null
-- order by 	d.[DB_2021],yr
;")


dbExecute(con, med_house_price_view_query)


med_house_price_tbl = read_sql_query(con,  "DA_MEDIAN_HOUSE_PRICE_VIEW", table_prefix_new)
  # tbl(con,  "DA_MEDIAN_HOUSE_PRICE_VIEW")

med_house_price_tbl %>% glimpse()
# med_house_price_tbl = dbReadTable(con, name = "DA_MEDIAN_HOUSE_PRICE_VIEW")
  # dbGetQuery(con, statement =  glue::glue("select top 100 * from {table_prefix_new}.DA_MEDIAN_HOUSE_PRICE_VIEW"))


# med_house_price_tbl %>% 
#   count(yr)
# 
# med_house_price_tbl %>% 
#   count(MUN_NAME_2021)

med_house_price_df = med_house_price_tbl %>% collect()








# code the column names manually. 
# the best way to table summary metrics by DA

# ses_colname_query = glue::glue("
# select geo_name,ALT_GEO_CODE,
# 
# sum(case when CHARACTERISTIC_ID = '1' then c1_count_total end) as POP,
# 
# (sum(case when CHARACTERISTIC_ID in ('14','15') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('8') and c1_count_total>0 then c1_count_total end)) as PCT_15_24,
# 
# sum(case when CHARACTERISTIC_ID = '115' then c1_count_total end) as MED_AT_INC,
# 
# sum(case when CHARACTERISTIC_ID = '345' then c1_count_total end)/100 as LIM_AT,
# 
# sum(case when CHARACTERISTIC_ID = '6' then c1_count_total end) as POP_DENS,
# 
# sum(case when CHARACTERISTIC_ID in ('35','37') then c1_count_total end)/100 as DEP_RAT,
# 
# (sum(case when CHARACTERISTIC_ID in ('42') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('41') and c1_count_total>0 then c1_count_total end)) as DET_HOMES,
# 
# (sum(case when CHARACTERISTIC_ID in ('86') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('78') and c1_count_total>0 then c1_count_total end)) as SING_PAR,
# 
# sum(case when CHARACTERISTIC_ID = '151' then c1_count_total end)/100 as GOV_TRANS,
# 
# (sum(case when CHARACTERISTIC_ID in ('1403') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1402')  and c1_count_total>0 then c1_count_total end)) as PCT_INDIG,
# 
# (sum(case when CHARACTERISTIC_ID in ('1416') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1414')  and c1_count_total>0 then c1_count_total end)) as PCT_RENT,
# 
# (sum(case when CHARACTERISTIC_ID in ('1537') and c1_count_total is not null then c1_count_total else 0 end) / sum(case when CHARACTERISTIC_ID in ('1')  and c1_count_total>0  then c1_count_total end)) as PCT_NPR,
# 
# (sum(case when CHARACTERISTIC_ID in ('1684') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1683')  and c1_count_total>0 then c1_count_total end)) as PCT_VISMIN,
# 
# (sum(case when CHARACTERISTIC_ID in ('1996') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1995')  and c1_count_total>0 then c1_count_total end)) as PCT_NOGRAD,
# 
# (sum(case when CHARACTERISTIC_ID in ('2224') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2223')  and c1_count_total>0 then c1_count_total end)) as LAB_PART_RATE,
# 
# (sum(case when CHARACTERISTIC_ID in ('2226') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2224')  and c1_count_total>0 then c1_count_total end)) as LAB_UNEMP_RATE,
# 
# (sum(case when CHARACTERISTIC_ID in ('2249') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2246')  and c1_count_total>0 then c1_count_total end)) as PCT_MGMT_OCC
# 
# from {table_prefix}.[FCT_CENSUS_2021_BC_DA]
# 
# where geo_name=ALT_GEO_CODE
# 
# group by geo_name,ALT_GEO_CODE
# 
# having sum(case when CHARACTERISTIC_ID = '1' then c1_count_total end)>0
# ")
# 
# 
# ses_colname_db = read_sql_query(
#   con,
#   tbl_name,
#   stringr::str_to_upper(ses_colname_query) # all field/column names are uppercase in DBPLYR
#   )
# 
# # Column `CHARACTERISTIC_NAME` doesn't exist
# ses_colname_db %>% glimpse()
# 
# ses_colname_df = ses_colname_db %>% collect()
# #
# CHARACTERISTIC_ID_NAME = geo_59490120 %>% 
#   count(CHARACTERISTIC_ID , CHARACTERISTIC_NAME)
# 
# CHARACTERISTIC_ID_NAME = CHARACTERISTIC_ID_NAME %>% 
#   mutate(CHARACTERISTIC_ID_CHAR = str_pad( CHARACTERISTIC_ID, 4, side = 'left', pad = "0" )) %>% 
#   arrange(CHARACTERISTIC_ID_CHAR)
# 
# house_realted_list = CHARACTERISTIC_ID_NAME %>% filter(str_detect(str_to_lower(CHARACTERISTIC_NAME), pattern = "suit"))
# 
# CHARACTERISTIC_ID_NAME_SES = CHARACTERISTIC_ID_NAME %>% 
#   filter(
#     CHARACTERISTIC_ID %in% c("1", # pop
#                              "6", # pop_dens
#                              '8', #
#                              '14','15', # age 15 24, 
#                              '35','37',  # dep_rat
#                               "41",'42', # det_homes
#                              "50" , # Total - Private households by household size - 100% data
#                              "57", # Average household size
#                              "78", "86", #sing_par
#                              '115', # med_at_inc
#                              "151", # gov_trans
#                              
#                              "244", # Median after-tax income of household in 2020 ($)
#                              '345', # lim_at
#                              
#                              "381", # Gini index on adjusted household after-tax income
#                              "382", # P90/P10 ratio on adjusted household after-tax income
#                              "1402", "1403", # pc_indig
#                              "1414", "1416", # pct_rent
#                              
#                              "1470", # Total - Households 'spending 30% or more of income on shelter costs' or 'not suitable' or 'major repairs needed'
#                              "1472", # Not suitable only: too crowded
#                              "1473", # Major repairs needed only
#                              "1476", #  'Not suitable' and 'major repairs needed'
#                              "1477", # 'Spending 30% or more of income on shelter costs' and 'not suitable' and 'major repairs needed'
#                              "1537", # pct_npr "1"
#                              "1683",  "1684", # pct_vismin
#                              "1995", "1996",  #pct_nograd
#                              "2223", "2224", # labor_part_rate
#                              "2226", #labor_unemp_rate "2224"
#                              "2246", "2249" #pct_mgmt_occ
#                              
#                              )
#   )
# 
# CHARACTERISTIC_ID_NAME_SES = CHARACTERISTIC_ID_NAME_SES %>% 
#   mutate(CHARACTERISTIC_ID_CHAR = str_pad( CHARACTERISTIC_ID, 4, side = 'left', pad = "0" )) %>% 
#   arrange(CHARACTERISTIC_ID_CHAR)


# house cma add_rownames(
#   
# 
#   )







