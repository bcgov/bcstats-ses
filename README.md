<!-- 
Add a project state badge
<!-- badges: start -->
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/license/apache-2-0/)
<!-- badges: end -->


BC community Socio-economic status index
============================

The BC community socio-economic status (SES) index evaluates individuals or groups' relative social and economic standing within BC.

The SES index is often used in research, public policy, and social programs to understand disparities in outcomes such as health, education, and employment, and to develop interventions aimed at reducing inequality and promoting social mobility.

### Data

Data used in this project is stored on the LAN. See `src/README.md` for detail.

### Usage

The code for cleaning external data to be used in the development of a SES index are including in the `src` folder. More details for running this code can be found in /src/README.md.  

An R env environment is created for this project using [`renv` package](https://rstudio.github.io/renv/articles/renv.html). You can download and install all the packages this project needs by running `renv::restore()`.


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
