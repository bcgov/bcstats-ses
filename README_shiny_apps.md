# BC Socio-Economic Index Shiny Apps

This repository contains three Shiny applications for visualizing BC Socio-Economic Index data, designed to replicate and enhance the functionality of your PowerBI dashboard.

## Available Apps

### 1. `shiny_ses_app.R` - Basic Version
- Simple implementation with core functionality
- Basic time series, comparison, geographic, and data explorer views
- Uses sample data for demonstration

### 2. `enhanced_ses_app.R` - Enhanced Version  
- Improved UI with better styling and user experience
- Enhanced filters with search capabilities
- More sophisticated visualizations
- Better color schemes and layouts

### 3. `production_ses_app.R` - Production Version (Recommended)
- Attempts to load your actual data files from the `out/` directory
- Falls back to sample data if actual files aren't found
- Includes overview dashboard with summary statistics
- Professional styling and comprehensive functionality
- Matches PowerBI app structure most closely

## Features Implemented

### Time Series Analysis
- Evolution of index values over time
- Multi-line charts showing different indices
- Region and model type filtering
- Interactive plotly visualizations

### Regional Comparison
- Side-by-side comparison of two regions
- Factor contribution analysis
- Bar charts and waterfall-style visualizations
- Detailed comparison tables

### Geographic Explorer
- Interactive map visualization
- Regional rankings and performance
- Customizable display options
- Click-through functionality

### Data Explorer
- Comprehensive data filtering
- Searchable and sortable tables
- CSV download functionality
- Advanced filtering options

## Data Structure Expected

The apps expect long-format CSV files with the following structure:

### Index Data
```
UID, REGION_NAME, CALENDAR_YEAR, AVERAGE_AGE, POPULATION_ESTIMATE, 
INDEX_TYPE, TOTAL, MODEL_TYPE, REGION_LEVEL, INDEX_LABEL
```

### Contribution Data
```
UID, REGION_NAME, CALENDAR_YEAR, AVERAGE_AGE, POPULATION_ESTIMATE, 
INDEX_TYPE, TOTAL, FACTOR, CONTRIBUTION_VALUE, MODEL_TYPE, 
REGION_LEVEL, FACTOR_LABEL, INDEX_LABEL
```

## How to Run

1. **Install Required Packages:**
```r
install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                   "dplyr", "ggplot2", "leaflet", "sf", "readr", 
                   "stringr", "scales", "RColorBrewer", "shinyWidgets"))
```

2. **Run the App:**
```r
# For production version (recommended)
shiny::runApp("production_ses_app.R")

# Or for enhanced version
shiny::runApp("enhanced_ses_app.R")

# Or for basic version
shiny::runApp("shiny_ses_app.R")
```

3. **Using Your Data:**
   - Place your long-format CSV files in the `out/` directory
   - Name them with patterns like `long_format_index_*.csv` and `long_format_contribution_*.csv`
   - The production app will automatically detect and load them

## Customization

### Adding Your Actual Data
1. Update the `load_data()` function in `production_ses_app.R`
2. Modify file paths to match your data location
3. Adjust column names if they differ from the expected structure

### Styling
- Modify the `custom_css` variable to change colors and appearance
- Update color palettes in the plotting functions
- Adjust box layouts and dashboard structure

### Geographic Data
- Replace sample coordinates with actual BC region coordinates
- Add actual shapefiles for proper geographic visualization
- Integrate with bcmaps package for accurate BC boundaries

## Key Differences from PowerBI

### Advantages of Shiny Version:
- More interactive and responsive
- Better mobile compatibility
- Easier to customize and extend
- Open source and free
- Better integration with R ecosystem

### PowerBI Features Not Yet Implemented:
- Drill-through functionality (can be added)
- Advanced DAX-like calculations (can be implemented in R)
- Automatic refresh from data sources (can be added)

## Next Steps

1. **Data Integration:** Connect to your actual data sources
2. **Geographic Enhancement:** Add real BC shapefiles and coordinates  
3. **Performance Optimization:** Implement data caching for large datasets
4. **Advanced Analytics:** Add statistical analysis and forecasting
5. **User Authentication:** Add login system if needed for production deployment

## Support

The apps are designed to be self-contained and should work with sample data out of the box. For production use with your actual data, you may need to adjust file paths and column names to match your specific data structure.