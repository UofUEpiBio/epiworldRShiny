## SEIR Model

This implementation of the Susceptible-Exposed-Infected-Recovered model is comparable to the compartmental version of the same name. At each step, agents randomly interact with each other, regardless of the distance between them.

### Parameters

The SEIR model has the following parameters:

- **Disease Name**: The name of the disease being modeled.
- **% of Population Infected**: The percentage of the population that is infected at the start of the simulation.
- **Transmission probability**: Probability of becoming infected (or exposed, depending on the model) when interacting with an infectious agent.
- **Recovery probability (daily)**: The probability that an individual will recover from the virus on a given day.
- **Incubation Days**: The number of days that an individual is in the exposed state before becoming infectious.
- **Contact rate**: The number of average contacts that an individual has per day.
- **Simulation Time (Days)**: The number of days to simulate.
- **Seed**: The seed for the random number generator.
- **Population Structure**: The population structure to use for the simulation.
- **NPIs**: The non-pharmaceutical interventions to use for the simulation.
