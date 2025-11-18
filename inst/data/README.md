# School Data for Measles Model

This directory contains school data files for the measles model in the epiworldRShiny package.

## schools_measles.csv

This is a sample dataset containing school information for use with the measles model's school selector feature. The file includes 8 schools from 2 states (California and Texas), each in different counties.

### CSV Format

The CSV file must contain the following columns:

| Column | Type | Description | Valid Range |
|--------|------|-------------|-------------|
| state | string | State name | Any valid US state name |
| county | string | County name | Any valid county name |
| school_name | string | Name of the school | Any string |
| school_id | string | Unique school identifier | Any unique identifier |
| vaccination_rate | numeric | Vaccination rate | 0.0 to 1.0 (e.g., 0.95 = 95%) |
| num_students | integer | Total number of students | 0 to 50,000 |

### Example

```csv
state,county,school_name,school_id,vaccination_rate,num_students
California,Los Angeles,Lincoln Elementary,CA-LA-001,0.95,450
Texas,Harris,Madison Elementary,TX-HR-001,0.85,520
```

### Usage

Users can either:
1. Use the default sample data provided in `schools_measles.csv`
2. Upload their own CSV file with school data following the same format

The school selector feature will validate the CSV format and data ranges before loading.

### Data Validation

The following validations are applied:
- File size must be less than 5MB
- All required columns must be present
- `vaccination_rate` must be between 0 and 1
- `num_students` must be between 0 and 50,000
- Maximum of 1,000 schools per file
- No NA values in `vaccination_rate` or `num_students` for selected schools
