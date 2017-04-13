# omnom

Omnom the eater of APIs

![Alt Text](https://media.giphy.com/media/jgUG5cnss7T9K/giphy.gif)


## Overview

A HATEOAS API browsing webapp written in Clojurescript.


## Features

Omnom is very new - there is a great deal it does not do yet, here are some of the useful bits so far.

* Sends simple GET and POST requests
* Displays/pretty prints responses in a mixture of tables and lists
* Builds requests using an analysis of the API specification effectively setting defaults to make traversing an API painless

See the [issues](https://github.com/rossputin/omnom/issues) for a hint of the roadmap.


## Dependencies

* Latest version of Google Chrome or Safari
* A [Protean](https://github.com/passivsystems/protean) instance running on port 3000/3001
* A HAL HATEOAS API


## API stability

Omnom is very new and will be subject to considerable change until it matures closer to a 1.0.0 release.  We will not strive for backwards compatibility at this time but will preserve old releases as we move forwards with the software.


## Usage

Figwheel build : lein figwheel dev

Production build : lein cljsbuild once prod
