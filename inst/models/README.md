# Models in `epiworldRShiny`

This folder contains the pre-built models available in the app. 

## Model Format

Each model consists of two files:

- `shiny_<model_name>.R`: Contains the model's R code split into two functions:
    - `shiny_<model_name>()`: Runs the model and returns the results
    - `<model_name>_panel()`: Defines the model's UI control panel in the app
- `shiny_<model_name>.md`: Contains the model's documentation which is displayed in the app

**IMPORTANT NOTE:** All models must follow the above format or `epiworldRShiny` won't import them correctly. 

## Custom Models

`epiworldRShiny` allows users to add custom models to the app, either by adding to the `inst/models` folder or by passing a separate folder path to the `run_app()` function using the `custom_models_path` argument.

In either case, your models must follow the format described above.
Pay special attention to ensuring the `<model_name>` is used properly (e.g., the file name matches the function name).