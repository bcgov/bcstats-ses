import pandas as pd

# Create a dataframe for the complete table
data = {
    "Full Name": [
        "Canadian Marginalization Index", "Canadian Socio-economic Status Index for Environmental Pollution Studies", 
        "Community Well-Being Index", "Socio-economic Factor Index 2", 
        "Québec Index of Material and Social Deprivation", "Overall Regional Socio-Economic Index", 
        "Vancouver Area Neighbourhood Deprivation Index", "Early Child Development Mapping Project", 
        "Living Conditions Index"
    ],
    "Abbreviation": [
        "Can-Marg", "Chan index", "CWB", "SEFI-2", 
        "Pampalon index", "ORSEI", "VANDIX", "EC-Map", "LCI"
    ],
    "Creators": [
        "Matheson, Dunn, Smith, Moineddin, & Glazier (7)", 
        "Chan, Serrano, Chen, Stieb, Jerrett, Osornio-Vargas", 
        "Penney, O’Sullivan, & Senécal (Aboriginal Affairs and Northern Development Canada)", 
        "Manitoba Centre for Health Policy (20)", 
        "Pampalon, Hamel, & Raymond (Institut national de santé publique du Québec) (19)", 
        "BC Stats (35)", "Bell & Hayes (40)", "Krishnan (University of Alberta) (34)", 
        "Krishnan, Betts, & Wang (University of Alberta) (33)"
    ],
    "Jurisdictions Available": [
        "All of Canada", "All of Canada", "First Nations, Inuit, and other Canadian communities", 
        "All of Canada (originally Manitoba)", "All of Canada (originally Québec)", 
        "British Columbia", "British Columbia (originally Vancouver)", "Alberta", "Alberta"
    ],
    "Years Available": [
        "1991, 1996, 2001, 2006", "2006", "1981, 1991, 1996, 2001, 2006, 2011", 
        "2001, 2006, 2011", "1991, 1996, 2001, 2006, 2011, 2016", "1999, 2012", 
        "2006", "2006", "2006"
    ],
    "Geographic Level": [
        "CT and DA", "DA", "CSD", "DA, CSD, RHA, RHA district, and CA", "EA and DA", 
        "CD and LHA", "DA", "DA", "DA"
    ],
    "Data Source": [
        "Census", "Census", "Census, NHS (2011)", "Census, NHS (2011)", "Census, NHS (2016)", 
        "BC Stats", "Census", "Census", "Census"
    ],
    "Indicators Used": [
        "Residential instability (7 variables), Material Deprivation (6 variables), Dependency (3 variables), Ethnic concentration (2 variables)", 
        "High-material ownership (2 variables), Low-material ownership (2 variables), Socially advantaged (2 variables), Economically advantaged (3 variables), Socially disadvantaged (3 variables), Economically disadvantaged (2 variables), Children’s environmental hazard (2 variables)", 
        "Income (1 variable), Education (2 variables), Housing (2 variables), Labour force activity (2 variables)", 
        "Average household income, Unemployment rate (15+), Proportion of population 15+ without high school graduation, Proportion of single-parent families", 
        "Material component: 3 variables (education, employment, income), Social component: 3 variables (living alone, marital status, single-parent families)", 
        "Human economic hardship (3 variables), Crime (3 variables), Health problems (3 variables), Education concerns (4 variables), Children at risk (6 variables), Youth at risk (4 variables)", 
        "Material wealth (1 variable), Housing (1 variable), Demographics (1 variable), Education (2 variables), Employment (2 variables)", 
        "Economic system (6 variables), Social system (8 variables), Cultural system (4 variables), Child care (3 variables), Vulnerable group membership (2 variables)", 
        "Economic diversity (4 variables), Housing (5 variables), Education (3 variables), Minority population (3 variables), Dependent population (3 variables)"
    ]
}

# Create DataFrame
df = pd.DataFrame(data)

# Save the DataFrame to a CSV file
file_path = '/mnt/data/ABSIs_Canada.csv'
df.to_csv(file_path, index=False)

file_path
