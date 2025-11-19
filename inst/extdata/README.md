# School Data for Measles Model

This directory contains school data files for the measles model in the epiworldRShiny package.

## Data Source

The school data is sourced from the [TACC Measles Dashboard](https://github.com/TACC/measles-dashboard) repository, prepared by the epiENGAGE team at the Texas Advanced Computing Center (TACC). We are grateful to the epiENGAGE team for making this data publicly available.

The data includes measles-mumps-rubella (MMR) vaccination rates for schools across the United States. **Note:** Enrollment data is not provided in the source dataset, so all schools default to 500 students. Users can upload custom CSV files with actual enrollment numbers if available.

## schools_measles.csv

This file contains aggregated school information from multiple US states with MMR vaccination rates.

### Generating the Data

To regenerate or update the school data file, run the R script in the `data-raw` directory:

```r
source("data-raw/download_measles_schools.R")
```

This script will:
1. Download CSV files for all US states from the TACC measles dashboard repository
2. Extract and combine the relevant columns
3. Convert vaccination rates from percentages to decimals (0-1 range)
4. Filter out schools with missing or invalid data
5. Set enrollment to a default value of 500 (not provided in source data)
6. Generate a single combined CSV file

### CSV Format

The CSV file contains the following columns:

| Column | Type | Description | Valid Range |
|--------|------|-------------|-------------|
| state | string | State name | Any valid US state name |
| county | string | County name | Any valid county name |
| school_name | string | Name of the school | Any string |
| school_id | string | Unique school identifier | Format: ST-##### (e.g., CA-00001) |
| vaccination_rate | numeric | MMR vaccination rate | 0.0 to 1.0 (e.g., 0.95 = 95%) |
| num_students | integer | Total number of students (defaults to 500 in source data) | 0 to 50,000 |

### Example

```csv
state,county,school_name,school_id,vaccination_rate,num_students
California,Los Angeles,Lincoln Elementary,CA-00001,0.95,500
Texas,Harris,Madison Elementary,TX-00001,0.85,500
```

**Note:** In the default dataset from TACC, all schools have `num_students` set to 500 since enrollment data is not available in the source. Users can upload custom CSV files with actual enrollment numbers for their specific use cases.

### Usage

Users can either:
1. Use the provided school data file
2. Upload their own CSV file with school data following the same format

The school selector feature will validate the CSV format and data ranges before loading.

### Data Credits

This data is provided by the epiENGAGE project and the TACC Measles Dashboard. When using this data, please cite:

- TACC Measles Dashboard: https://github.com/TACC/measles-dashboard
- Original data sources are documented in the TACC repository

### Data Validation

The download script applies the following validations:
- File size must be reasonable for processing
- All required columns must be present
- `vaccination_rate` must be between 0 and 1
- `num_students` must be between 0 and 50,000
- Removes rows with missing critical vaccination rate data

