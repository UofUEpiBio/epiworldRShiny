<div class="alert alert-warning" role="alert">
    <strong>Warning:</strong> This is work in progress. The model does
    not include post-exposure prophylaxis or community transmission.
    We would love to hear your feedback on the model. All the source
    code is available <a href="https://github.com/UofUEpiBio/epiworldRShiny/tree/measlesquarantine">here</a>.
</div>

# Modeling Measles in Schools

This model simulates measles outbreaks in schools and compares **how many fewer cases a quarantine procedure yields**. You can specify the number of people in the school (population size), the number of students initially infected with measles (initial cases), the proportion of students who are vaccinated before the outbreak, and the simulation duration in days. 

## School Selector Feature

The **School Selector** feature allows you to quickly populate the vaccination rate and school size using real-world school data through a cascading selection system:

### How to Use

1. **Select State**: Choose a state from the first dropdown
2. **Select County**: Choose a county from the filtered list
3. **Select School**: Choose a specific school to automatically populate the parameters

When you select a school, the "Population Size" and "Proportion Vaccinated" fields will be automatically updated with the school's data.

### Default Database

The app includes a sample database of schools from California and Texas with the following structure:
- 2 states (California, Texas)
- 4 counties (Los Angeles, San Diego, Harris, Dallas)
- 8 schools with realistic vaccination rates and student counts

### Upload Custom Data

You can upload your own CSV file with school data. The CSV must contain the following columns:
- `state`: State name
- `county`: County name
- `school_name`: Name of the school
- `school_id`: Unique school identifier
- `vaccination_rate`: Vaccination rate (0-1 decimal format, e.g., 0.95 for 95%)
- `num_students`: Total number of students in the school

After uploading, use the **Reset to Default Data** button to return to the built-in sample data.

## Model Details

Note that the model only simulates outbreaks among students within a single school. It does not include transmissions from the students to people in other locations (e.g., other schools, households, the community, etc.) and does not include measles introductions to the school after the initial cases. Learn more about the model at <https://github.com/EpiForeSITE/epiworld-measles>.

