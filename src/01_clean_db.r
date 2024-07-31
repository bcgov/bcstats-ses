if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(odbc, tidyverse, DBI, dbplyr)
# get a connection
# SQL server use windows authentication, as long as it is on your office laptop, it should be fine. Not test in mac yet.
# require VPN 2 connected.
# server information is stored in config. 
con <-dbConnect(odbc(),
                 Driver = config::get("Driver"),
                 Server = config::get("Server"),
                 Database = config::get("Database"),
                 Trusted_Connection = "True")

# create a function for the preprocess
# SQL server requires the long text fields at the end of the select clause
read_sql_query = function(con, tbl_name, query) {
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
  tab = dplyr::tbl(con, sql(query)) %>%
    dplyr::select(-tidyselect::any_of(long_cols),
                  tidyselect::everything(),
                  tidyselect::any_of(long_cols))
}



# example:
table_prefix = config::get("table_prefix")

print("Sepecify the table name")
tbl_name = "FCT_CENSUS_2021_BC_DA"
print("Put your query in a string")


query = glue::glue("SELECT *  FROM {table_prefix}.[FCT_CENSUS_2021_BC_DA]")

print("Use read_sql_query function to fetch the table")
my_tbl = read_sql_query(
  con,
  tbl_name,
  query)


print("The samples from table:FCT_CENSUS_2021_BC_DA:")
my_tbl %>% glimpse()

print("If you see the samples, it means the SQL Server connection works. \n If you don't see the samples, it has problem to connect to the SQL Server.")

print("You can check out the query as well")

# my_tbl %>% show_query()

GEO_LEVEL_list = my_tbl %>% count(GEO_LEVEL) %>% collect()

# GEO_LEVEL                 n
# <chr>                 <int>
# 1 Census division  
# 2 Census subdivision 
# 3 Country            
# 4 Dissemination area 
# 5 Province           

print("Now we can use dbplyr and dplyr to create new columns or get summarization.")

print("For example, we can collect first five rows from our query")

my_tbl %>% head(n=5) %>% collect()

# to understand the table and column names
new_query = glue::glue("select GEO_NAME,ALT_GEO_CODE,CHARACTERISTIC_ID,CHARACTERISTIC_NAME, C1_COUNT_TOTAL
from {table_prefix}.[FCT_CENSUS_2021_BC_DA]
where ALT_GEO_CODE = '59490120'
-- order by cast (CHARACTERISTIC_ID as int), C1_COUNT_TOTAL, GEO_NAME -- order does not work in DBPLYR
")


geo_59490120 = read_sql_query(
  con,
  tbl_name,
  new_query)

geo_59490120 %>% glimpse()

geo_59490120 = geo_59490120 %>%
# filter(ALT_GEO_CODE == '59490120') %>%
# select(ALT_GEO_CODE,CHARACTERISTIC_ID , C1_COUNT_TOTAL,CHARACTERISTIC_NAME,GEO_NAME) %>% 
# arrange(CHARACTERISTIC_ID  , C1_COUNT_TOTAL, GEO_NAME) %>% # arrange does not work for MS SQL server.
collect()

# code the column names manually. 
# the best way to table summary metrics by DA

ses_colname_query = glue::glue("
select geo_name,ALT_GEO_CODE,

sum(case when CHARACTERISTIC_ID = '1' then c1_count_total end) as POP,

(sum(case when CHARACTERISTIC_ID in ('14','15') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('8') and c1_count_total>0 then c1_count_total end)) as PCT_15_24,

sum(case when CHARACTERISTIC_ID = '115' then c1_count_total end) as MED_AT_INC,

sum(case when CHARACTERISTIC_ID = '345' then c1_count_total end)/100 as LIM_AT,

sum(case when CHARACTERISTIC_ID = '6' then c1_count_total end) as POP_DENS,

sum(case when CHARACTERISTIC_ID in ('35','37') then c1_count_total end)/100 as DEP_RAT,

(sum(case when CHARACTERISTIC_ID in ('42') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('41') and c1_count_total>0 then c1_count_total end)) as DET_HOMES,

(sum(case when CHARACTERISTIC_ID in ('86') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('78') and c1_count_total>0 then c1_count_total end)) as SING_PAR,

sum(case when CHARACTERISTIC_ID = '151' then c1_count_total end)/100 as GOV_TRANS,

(sum(case when CHARACTERISTIC_ID in ('1403') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1402')  and c1_count_total>0 then c1_count_total end)) as PCT_INDIG,

(sum(case when CHARACTERISTIC_ID in ('1416') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1414')  and c1_count_total>0 then c1_count_total end)) as PCT_RENT,

(sum(case when CHARACTERISTIC_ID in ('1537') and c1_count_total is not null then c1_count_total else 0 end) / sum(case when CHARACTERISTIC_ID in ('1')  and c1_count_total>0  then c1_count_total end)) as PCT_NPR,

(sum(case when CHARACTERISTIC_ID in ('1684') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1683')  and c1_count_total>0 then c1_count_total end)) as PCT_VISMIN,

(sum(case when CHARACTERISTIC_ID in ('1996') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1995')  and c1_count_total>0 then c1_count_total end)) as PCT_NOGRAD,

(sum(case when CHARACTERISTIC_ID in ('2224') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2223')  and c1_count_total>0 then c1_count_total end)) as LAB_PART_RATE,

(sum(case when CHARACTERISTIC_ID in ('2226') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2224')  and c1_count_total>0 then c1_count_total end)) as LAB_UNEMP_RATE,

(sum(case when CHARACTERISTIC_ID in ('2249') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2246')  and c1_count_total>0 then c1_count_total end)) as PCT_MGMT_OCC

from {table_prefix}.[FCT_CENSUS_2021_BC_DA]

where geo_name=ALT_GEO_CODE

group by geo_name,ALT_GEO_CODE

having sum(case when CHARACTERISTIC_ID = '1' then c1_count_total end)>0
")


ses_colname_db = read_sql_query(
  con,
  tbl_name,
  stringr::str_to_upper(ses_colname_query) # all field/column names are uppercase in DBPLYR
  )

# Column `CHARACTERISTIC_NAME` doesn't exist
ses_colname_db %>% glimpse()

ses_colname_df = ses_colname_db %>% collect()
#
CHARACTERISTIC_ID_NAME = geo_59490120 %>% 
  count(CHARACTERISTIC_ID , CHARACTERISTIC_NAME)

CHARACTERISTIC_ID_NAME = CHARACTERISTIC_ID_NAME %>% 
  mutate(CHARACTERISTIC_ID_CHAR = str_pad( CHARACTERISTIC_ID, 4, side = 'left', pad = "0" )) %>% 
  arrange(CHARACTERISTIC_ID_CHAR)

house_realted_list = CHARACTERISTIC_ID_NAME %>% filter(str_detect(str_to_lower(CHARACTERISTIC_NAME), pattern = "suit"))

CHARACTERISTIC_ID_NAME_SES = CHARACTERISTIC_ID_NAME %>% 
  filter(
    CHARACTERISTIC_ID %in% c("1", # pop
                             "6", # pop_dens
                             '8', #
                             '14','15', # age 15 24, 
                             '35','37',  # dep_rat
                              "41",'42', # det_homes
                             "50" , # Total - Private households by household size - 100% data
                             "57", # Average household size
                             "78", "86", #sing_par
                             '115', # med_at_inc
                             "151", # gov_trans
                             
                             "244", # Median after-tax income of household in 2020 ($)
                             '345', # lim_at
                             
                             "381", # Gini index on adjusted household after-tax income
                             "382", # P90/P10 ratio on adjusted household after-tax income
                             "1402", "1403", # pc_indig
                             "1414", "1416", # pct_rent
                             
                             "1470", # Total - Households 'spending 30% or more of income on shelter costs' or 'not suitable' or 'major repairs needed'
                             "1472", # Not suitable only: too crowded
                             "1473", # Major repairs needed only
                             "1476", #  'Not suitable' and 'major repairs needed'
                             "1477", # 'Spending 30% or more of income on shelter costs' and 'not suitable' and 'major repairs needed'
                             "1537", # pct_npr "1"
                             "1683",  "1684", # pct_vismin
                             "1995", "1996",  #pct_nograd
                             "2223", "2224", # labor_part_rate
                             "2226", #labor_unemp_rate "2224"
                             "2246", "2249" #pct_mgmt_occ
                             
                             )
  )

CHARACTERISTIC_ID_NAME_SES = CHARACTERISTIC_ID_NAME_SES %>% 
  mutate(CHARACTERISTIC_ID_CHAR = str_pad( CHARACTERISTIC_ID, 4, side = 'left', pad = "0" )) %>% 
  arrange(CHARACTERISTIC_ID_CHAR)


house cma add_rownames(
  

  )






