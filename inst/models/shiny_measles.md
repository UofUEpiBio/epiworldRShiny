<div class="alert alert-warning" role="alert">
    <strong>Warning:</strong> This is work in progress. The model does
    not include post-exposure prophylaxis or community transmission.
    We would love to hear your feedback on the model. All the source
    code is available <a href="https://github.com/UofUEpiBio/epiworldRShiny">here</a>. School vaccination data was obtained from epiENGAGE's simulator <a href="https://github.com/TACC/measles-dashboard">here</a>.
</div>

# Modeling Measles in Schools

This model simulates measles outbreaks in schools and compares **how many fewer cases a quarantine procedure yields**. You can specify the number of people in the school (population size), the number of students initially infected with measles (initial cases), the proportion of students who are vaccinated before the outbreak, and the simulation duration in days. 

Note that the model only simulates outbreaks among students within a single school. It does not include transmissions from the students to people in other locations (e.g., other schools, households, the community, etc.) and does not include measles introductions to the school after the initial cases. Learn more about the model parameters and their sources in the [canonical parameter table](https://github.com/UofUEpiBio/measles/blob/main/inst/extdata/measles_parameters.csv) of the [measles R package](https://github.com/UofUEpiBio/measles) (see also the [parameters vignette](https://github.com/UofUEpiBio/measles/blob/main/vignettes/parameters.qmd)).

<details>
<summary><strong>Model assumptions &amp; references</strong></summary>

The table lists every parameter of `measles::ModelMeaslesSchool()` used by this app, including the ones left at the package default. "Value used" is the app's default; you can change most of them in the inputs panel. The canonical, cited table lives in the [measles R package](https://github.com/UofUEpiBio/measles/blob/main/inst/extdata/measles_parameters.csv). The [Utah DHHS Measles Disease Plan](https://epi.utah.gov/wp-content/uploads/Measles-disease-plan.pdf) cited here is the edition last updated March 26, 2026.

Status: ✅ verified against the cited source; 🗣️ team assumption or rationale, confirmed by the authors; ⚠️ pending.

| Parameter | Value used | Package default | Source | Status | Notes / why different |
|---|---|---|---|---|---|
| R0 (target) | 15 | 15 | Guerra et al. 2017, *Lancet Infect Dis*, [doi:10.1016/S1473-3099(17)30307-9](https://doi.org/10.1016/S1473-3099(17)30307-9) | ✅ 🗣️ | Midpoint of the 12–18 range. Not an input: it enters through the contact rate. |
| Population size | 500 | none (required) | Scenario input; school enrollment when available | 🗣️ | Selecting a school with enrollment data overwrites it. |
| Initial cases | 1 | 1 | Scenario input | 🗣️ | |
| Proportion vaccinated | 0.85 | 1 − 1/15 ≈ 0.93 | Scenario input; school vaccination data from the School Selector | 🗣️ | The app starts below the herd-immunity threshold (≈ 93%) to show an outbreak-prone school; selecting a school overwrites it. |
| Transmission probability | 0.99 | 0.9 | Assumption: highly transmissible; the [Utah DHHS Measles Disease Plan](https://epi.utah.gov/wp-content/uploads/Measles-disease-plan.pdf) says "90% of susceptible contacts will develop disease" | 🗣️ ✅ | Per contact. The app fixes it near 1 and calibrates the contact rate to R0 = 15. |
| Contact rate | 15 / 0.99 / 4 ≈ 3.79 per day | 15 / 0.9 / 4 ≈ 4.17 per day | Calibrated to R0 = 15 (R0 = transmission × contact rate × prodromal period) | 🗣️ | Lower than the package default because the app's transmission probability is higher; both give R0 = 15. The slider rounds to 0.01. |
| Vaccine efficacy | 0.97 | 0.97 | [Utah DHHS Measles Disease Plan](https://epi.utah.gov/wp-content/uploads/Measles-disease-plan.pdf) ("~97%", 2 doses); CDC | ✅ | All-or-nothing. Earlier versions used 0.99 (Liu et al. 2015, [doi:10.1186/s12889-015-1766-6](https://doi.org/10.1186/s12889-015-1766-6)). |
| Incubation period | 12 days | 12 days | [Utah DHHS Measles Disease Plan](https://epi.utah.gov/wp-content/uploads/Measles-disease-plan.pdf): exposure to prodrome averages 8–12 days | ✅ | |
| Prodromal period | 4 days | 4 days | [Utah DHHS Measles Disease Plan](https://epi.utah.gov/wp-content/uploads/Measles-disease-plan.pdf): prodrome lasts 2–4 days (range 2–8); contagious 4 days before rash onset | ✅ | |
| Rash period | 3 days | 3 days | [Utah DHHS Measles Disease Plan](https://epi.utah.gov/wp-content/uploads/Measles-disease-plan.pdf): contagious to 4 days after rash onset; infectivity minimal after day 2 of rash | ✅ | Infectious part of the rash only; the visible rash lasts 6–7 days. |
| Days undetected | 2 days | 2 days | Assumption: about 2 days from active case to public health notification | 🗣️ | |
| Hospitalization rate | 0.2 per day | 0.2 per day | Conservative value agreed with Utah DHHS. Recent analyses use 10% (Jones et al. 2026, *NEJM Evid*, [doi:10.1056/EVIDpha2600141](https://doi.org/10.1056/EVIDpha2600141)) | ✅ 🗣️ | A daily **rate**, not a probability: p = h / (h + 1/rash) = 0.2 / (0.2 + 1/3) ≈ 37.5%. Wang et al. 2026, *MMWR*, [doi:10.15585/mmwr.mm7520a1](https://doi.org/10.15585/mmwr.mm7520a1) reports 18.5% in West Texas. |
| Hospitalization duration | 7 days | 7 days | Assumption | 🗣️ | Observed stays are shorter: mean 2.1 nights in Utah (Jones et al. 2026); median 2 days in West Texas (Wang et al. 2026). |
| Quarantine period | 21 days (with quarantine); none (without) | 21 days | [Utah DHHS Measles Disease Plan](https://epi.utah.gov/wp-content/uploads/Measles-disease-plan.pdf): 21 days since last exposure | ✅ | The "without quarantine" scenario passes `quarantine_period = -1`. |
| Quarantine willingness | 1.0 | 1.0 | Assumption (field experience) | 🗣️ | Some analyses use 0.9. |
| Isolation period | 4 days | 4 days | [Utah DHHS Measles Disease Plan](https://epi.utah.gov/wp-content/uploads/Measles-disease-plan.pdf): isolate until 4 days after rash onset | ✅ | |
| Vaccine reduction in recovery | 0 (fixed) | not an argument | Not active | ✅ | The model registers it as "(IGNORED) Vax improved recovery"; it has no effect. |
| Simulation settings | 100 days, 200 simulations, seed 2023 | n/a | App settings | 🗣️ | Not epidemiological parameters. |

</details>
