<!-- 
Add a project state badge
<!-- badges: start -->
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/license/apache-2-0/)
<!-- badges: end -->


BC community Socio-economic status index
============================

Collect data from StatsCan and other statistics agencies and convert to clean CSV files.


### Data

1. [StatsCan census 2021](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/index-eng.cfm). It includes income, education, vocation, and family information. 
2. Translation Master file(TMF). TMF is a table with different levels of geography to link the data.It is a fact table and comes with a set of dimension tables (lookup tables) for the categorical variables.  
3. [B.C. crime trends and statistics](https://www2.gov.bc.ca/gov/content/justice/criminal-justice/policing-in-bc/publications-statistics-legislation/crime-police-resource-statistics). 
 * It requires a lookup table to link policing jurisdictions and regions in BC. 
4. [BC population projection](https://bcstats.shinyapps.io/popApp/)
5. [BC Wild fire data]()


### Project Status

This project is in very early stages and will be subject to a large amount of change. The existing code has been added as a starting place.


### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/bcstats-ses/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2024 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
