create view DA_MEDIAN_HOUSE_PRICE_VIEW AS 
select distinct 
	substring(a.CONVEYANCE_DATE,1,4) as yr,
	DA_2021,
	MUN_NAME_2021,
	substring(d.[DB_2021],1,8) as DB_CODE, 
	PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY CONVEYANCE_PRICE) OVER (PARTITION BY DA_2021,substring(CONVEYANCE_DATE,1,4)) AS MedianPrice
from 
	[FCT_BCA_SALES_2024_06] a 
	left join .[FCT_bca_folio_descriptions_20240609] b on a.FOLIO_ID=b.FOLIO_ID
	left join (
select 
	*, 
	case when substring(postal_code,4,1) <> '' then postal_code else concat(substring(postal_code,1,3),
	substring(postal_code,5,3)) end as PC
from 
	.[FCT_bca_folio_addresses_20240609]
) c on a.FOLIO_ID=c.FOLIO_ID
	left join .[FCT_TMF_202312] d on c.PC=d.POSTALCODE
	left join .[DIM_TMF_CD_CMA_2021] on CMACA_2021=CMA
where 
	a.CONVEYANCE_TYPE_DESCRIPTION = 'Improved Single Property Transaction'
	and b.ACTUAL_USE_DESCRIPTION in ('Single Family Dwelling','Residential Dwelling with Suite',
	'Row Housing (Single Unit Ownership)','Duplex, Strata Side by Side',
	'Duplex Non-Strata Side by Side or Front / Back','Duplex, Strata Front / Back','Strata-Lot Residence (Condominium)')
	and substring(a.CONVEYANCE_DATE,1,6)>='200001'
    AND 	d.[DB_2021] is not null
-- order by 	d.[DB_2021],yr
;