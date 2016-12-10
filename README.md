# rend

A Clojure program designed to use generative (property-based) testing
to validate web browser rendering.

## Usage

* Prerequisites:
  * Install libopencv-dev:
```
sudo apt-get install libopencv-dev
```

  * Generate the opencv maven artifactrs:
```
make deps
```

* Start the webdriver capable browsers that are to be tested

* Update `config.yaml` with browser webdriver connection information

* Start testing:
```
lein run config.yaml sessions.edn
```

## License

Copyright © 2016 Joel Martin

Distributed under the Mozilla Public License either version 2.0 or (at
your option) any later version.
