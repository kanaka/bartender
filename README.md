# Tend

A suite of Clojure tools for generative (property-based) testing using
formal grammars (EBNF) with a focus on web browser rendering.

## Prerequisites

* Install libopencv-dev:
```
sudo apt-get install libopencv-dev
```

* Generate the opencv maven artifactrs:
```
make deps
```

* Download/build the drivers/browsers/servers:
  * Firefox (https://developer.mozilla.org/en-US/Firefox/Headless_mode):
  ```
  curl -L https://github.com/mozilla/geckodriver/releases/download/v0.22.0/geckodriver-v0.22.0-linux64.tar.gz | tar xvzf -
  ```
  * Chrome (https://sites.google.com/a/chromium.org/chromedriver/getting-started):
  ```
  curl -LO https://chromedriver.storage.googleapis.com/2.35/chromedriver_linux64.zip
  unzip chromedriver_linux64.zip; rm chromedriver_linux64.zip
  ```
  * Servo (https://github.com/mozilla/servo):
  ```
  git clone https://github.com/servo/servo
  cd servo
  ./mach build --release
  ```
  * If using BrowserStack you will need to download the local testing
    proxy that allows BrowserStack to load test pages from the local
    system:
  ```
  wget https://www.browserstack.com/browserstaclocal/BrowserStackLocal-linux-x64.zip
  unzip BrowserStackLocal-linux-x64.zip
  ```

## Rend: Running Tests

* Start the drivers/browsers/servers:
  * Firefox:
  ```
  ./geckodriver --port 7000
  ```
  * Chrome:
  ```
  ./chromedriver --port=7001
  ```
  * Servo (https://github.com/mozilla/servo):
  ```
  ./mach run --release -z --webdriver=7002 --resolution=400x300
  ```
  * If using BrowserStack, start the BrowserStack local server:
  ```
  ./BrowserStackLocal --key KEY
  ```
  * Start a separate minimal web server to monitor results at
    `http://localhost:9080/gen/...`:
  ```
  python3 -m http.server 9080
  ```

* Update `config.yaml` with browser webdriver connection information

* Start a test run:
```
lein with-profile rend trampoline run config.yaml
```

## Mend: Update HTML5 and CSS3 Generators

Generate HTML5 EBNF grammar and Clojure generator source:

```
time lein with-profile html5 run --namespace rend.html5-generators --weights data/html5-weights.edn --weights-output data/html5-weights-output.edn --ebnf-output data/html5.ebnf --function html5-generators > src/rend/html5_generators.clj
```

Generate CSS3 EBNF grammar and Clojure generate source:

```
time lein with-profile css3 run --namespace rend.css3-generators --weights data/css3-weights.edn --weights-output data/css3-weights-output.edn --pvs-output data/css3.pvs --ebnf-output data/css3.ebnf --function css3-generators > src/rend/css3_generators.clj
```

Use the generators from a Clojure REPL:

```
lein repl
(require '[rend.generator])
(in-ns 'rend.generator)
(clojure.pprint/pprint (gen/sample (get-html-generator) 5))
```

## Browser Configuration

Here is example yaml configuration file that connects to geckodriver
and chromedriver running locally:

```yaml
quick-check:
    iterations: 20
    seed: 2
    max-size: 100
weights: "weights.edn"
compare:
    method: SQDIFF_NORMED
    threshold: 0.00003
web:
    host: 192.168.88.2
    port: 3000
    dir: "gen"
browsers:
    - id: firefox
      url: "http://localhost:7000"
      capabilities: {"moz:firefoxOptions":
                     {"args": ["--headless"]}}
    - id: chrome
      url: "http://localhost:7001"
      capabilities: {"chromeOptions":
                     {"args": ["--headless"]}}

```

![BrowserStack](imgs/browserstack-logo.png)

Here is the browser section of a yaml configuration file that connects
to three browsers running in BrowserStack:

```yaml
...
browsers:
    - id: bs-chrome-win-62
      url: "https://USER:KEY@hub-cloud.browserstack.com/wd/hub"
      capabilities: {"browserstack.local": true,
                     "browser": "Chrome",
                     "browser_version": "69.0",
                     "os": "Windows",
                     "os_version": "10",
                     "resolution": "1024x768"}
    - id: bs-firefox-win-62
      url: "https://USER:KEY@hub-cloud.browserstack.com/wd/hub"
      capabilities: {"browserstack.local": true,
                     "browser": "Firefox",
                     "browser_version": "62.0",
                     "os": "Windows",
                     "os_version": "10",
                     "resolution": "1024x768"}
    - id: bs-edge-win-17
      url: "https://USER:KEY@hub-cloud.browserstack.com/wd/hub"
      capabilities: {"browserstack.local": true,
                     "browser": "Edge",
                     "browser_version": "17.0",
                     "os": "Windows",
                     "os_version": "10",
                     "resolution": "1024x768"}
```


## Images

The following images are included in gen/static/:

* [File:Books-aj.svg\_aj\_ashton\_01g\*.png](https://commons.wikimedia.org/wiki/Openclipart#/media/File:Books-aj.svg_aj_ashton_01g.png) (Public Domain)
* [B\_stop.svg](https://commons.wikimedia.org/wiki/Openclipart#/media/File:B_stop.svg) (CC0)
* [Eilat_-_Dolphin\_reef\*.jpg](https://commons.wikimedia.org/wiki/Dolphin#/media/File:Eilat_-_Dolphin_reef.jpg) (CC BY-SA 3.0)
* [SpaceX\_Kestrel\_engine2\*.gif](https://commons.wikimedia.org/wiki/File:SpaceX_Kestrel_engine2.gif) (CC BY-SA-3.0)

## License

Copyright © Joel Martin

Distributed under the Mozilla Public License either version 2.0 or (at
your option) any later version.
