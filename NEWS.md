# nixtlar 0.6.2 

- Current version of `nixtlar`. See release notes [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.6.2)

# nixtlar 0.6.1 (2024-10-07)

We are excited to announce the release of `nixtlar` version 0.6.0, which integrates the [latest release](https://github.com/Nixtla/nixtla/releases/v0.6.1) of the `TimeGPT` API—v2. This update focuses on what matters most to our users: speed, scalability, and reliability.

**Key updates include**:

- **Data Structures**: `nixtlar` now extends support to `tibbles`, in addition to the previously supported data frames and tsibbles. This broadens the range of data structures that can be used in your workflows. 

- **Date Formats**: For efficiency, `nixtlar` now strictly requires dates to be in the format `YYYY-MM-DD` or `YYYY-MM-DD hh:mm:ss`, either as character strings or date-time objects. For more details, please refer to our [Get Started](https://nixtla.github.io/nixtlar/articles/get-started.html) guide and [Data Requirements](https://nixtla.github.io/nixtlar/articles/data-requirements.html) vignette. 

- **Default ID Column**: In alignment with the Python SDK, `nixtlar` now defaults the `id_col` to `unique_id`. This means you no longer need to specify this column if it is already named `unique_id`. If your dataset contains only one series, simply set `id_col=NULL`. The `id_col` only accepts characters or integers. 

These changes leverage the capabilities of `TimeGPT`'s new API and align `nixtlar` more closely with the Python SDK, ensuring a better user experience. See release notes [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.6.1)

# nixtlar 0.6.0 

- New version that uses `TimeGPT` API—v2. See release notes [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.6.0)

# nixtlar 0.5.4

- Development version. See release notes [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.5.4)

# nixtlar 0.5.3 

- Development version. See release notes [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.5.3)

# nixtlar 0.5.2

# nixtlar 0.5.1

# nixtlar 0.5.0

- Initial CRAN submission.
