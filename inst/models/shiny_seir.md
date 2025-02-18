## SEIR Network Model

The SEIR Network model is the network version of the Susceptible-Exposed-Infected-Removed model. In this version, agents are embedded in a network. The probability of infection is determined by the number of infected neighbors that an agent has.

### Parameters

The SEIR model has the following parameters:

- **Disease Name**: The name of the disease being modeled.
- **% of Population Infected**: The percentage of the population that is infected at the start of the simulation.
- **Transmission probability**: Probability of becoming infected (or exposed, depending on the model) when interacting with an infectious agent.
- **Recovery probability (daily)**: The probability that an individual will recover from the virus on a given day.
- **Incubation Days**: The number of days that an individual is in the exposed state before becoming infectious.
- **Simulation Time (Days)**: The number of days to simulate.
- **Seed**: The seed for the random number generator.
- **Population Structure**: The population structure to use for the simulation.
- **NPIs**: The non-pharmaceutical interventions to use for the simulation.
