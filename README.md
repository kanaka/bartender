# Bartender

***B***rowser ***A***utomated ***R***ender ***T***esti***n***g ***D***riv***er***:
Automated testing of browser rendering engines using Clojure,
generative (property-based) testing, formal grammars (EBNF), and
a consensus test oracle.

Also refer to [Instacheck](https://github.com/kanaka/instacheck) which
is leveraged for the core weighted grammar and property-based testing
engine.

## Prerequisites

* Install libopencv-dev:
```
sudo apt-get install libopencv-dev
```

* Generate the opencv maven artifactrs:
```
make deps
```

* Compile ClojureScript Report page application:

```
lein cljsbuild once send
```

* Download/build the drivers/browsers that you want to test:
  * Firefox (https://developer.mozilla.org/en-US/Firefox/Headless_mode):
  ```
  curl -L https://github.com/mozilla/geckodriver/releases/download/v0.22.0/geckodriver-v0.22.0-linux64.tar.gz | tar xvzf -
  ```
  * Chrome (https://sites.google.com/a/chromium.org/chromedriver/getting-started):
  ```
  curl -LO https://chromedriver.storage.googleapis.com/2.35/chromedriver_linux64.zip
  unzip chromedriver_linux64.zip; rm chromedriver_linux64.zip
  ```
  * Brave (https://github.com/brave/brave-browser/releases/):
  ```
  wget https://github.com/brave/brave-browser/releases/download/v0.56.15/chromedriver-v2.33-linux-x64.zip
  ```
  * Servo (https://github.com/mozilla/servo):
  ```
  git clone https://github.com/servo/servo
  cd servo
  #./mach build --release
  time RUSTFLAGS='-L ./support/linux/gstreamer/gst/lib' ./mach build --release
  ```
  * If using BrowserStack you will need to download the local testing
    proxy that allows BrowserStack to load test pages from the local
    system:
  ```
  wget https://www.browserstack.com/browserstaclocal/BrowserStackLocal-linux-x64.zip
  unzip BrowserStackLocal-linux-x64.zip
  ```

## Running Tests (rend)

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
  * Brave:
  ```
  ./brave/chromedriver --port=7003
  ```
  * If using BrowserStack, start the BrowserStack local server:
  ```
  ./BrowserStackLocal --key KEY
  ```

* Update `config.yaml` with browser webdriver connection information

* Start a test run:
```
lein run tests config.yaml
```

Monitor results at `http://localhost:3000` (3000 is the :web :port
specified in config.yaml).


## Extract/parse weights from an existing web page (wend)

```
time lein run parse --weights-output data/my-page.edn my-page.html
```

This can then be used to generate and test with similarly weighted
pages by updating the config yaml:

```yaml
...
weights:
    start: data/my-pages.edn
...
```

```
lein run tests config.yaml
```

You can then connect to `http://localhost:3000` (config file default)
to view a report/log of the tests as they run.


## How it works

This needs to be greatly expand but here is a very rough outline:

- **mend**: The `mend` module (described below) only needs to be
  executed directly when the HTML5 or CSS3 grammars are updated. It
  performs the following:
  - The HTML5 and CSS3 EBNF grammars from
    [kanaka/html5-css3-ebnf](https://github.com/kanaka/html5-css3-ebnf)
    are translated to Clojure
    [test.check](https://github.com/clojure/test.check) generator code
    using [Instacheck](https://github.com/kanaka/instacheck).
  - [instaparse](https://github.com/Engelberg/instaparse) grammars for
    parsing are also cached to `resources/html5.grammar` and
    `resources/css3.grammar`.
- **rend**: The `rend` module does the following:
  - Load yaml config file specified on the command line
  - Start a web server (for page loading and report UI) 
  - Connect to each configured browser (via webdriver)
  - Run 1 or more test runs. Each run uses quick-check (via
    Instacheck) to do the following:
    - Generate random test cases (HTML5 and CSS3 web pages) of
      increasing size.
    - Load each test case on all browsers, screenshot the results and
      compare.
    - If a difference above the configured threshold is detected then
      the shrinking process begins.
    - New test cases are generated and loaded/compared to try
      and find progressively smaller test cases that still cause
      a rendering difference.
    - If a reduction mode is specified, then after the shrinking
      process is complete, the shrunk test case is parsed to extract
      weights for the case and these weights are used to run
      a reduction algorithm on the weights in preparation for the next
      test run. This reduction step is to try and reduce the
      likelihood of finding the same test again in the next run.
- **send**: The `send` module is
  a [reagent](https://reagent-project.github.io)/React web application
  that dynamically reports on the state of server testing. The app
  keeps its state in sync with the server by connecting with
  WebSockets and receiving state delta messages.
- **wend**: The `wend` module (described above) allows existing test
  cases to be parsed to extract their grammar weight values. The
  extract weights can then be used to try and produce test cases that
  have similar characteristics to the existing test case. Specifically
  this is designed to enable shrinking of existing test cases.


## Browser Configuration

Here is example yaml configuration file that connects to Firefox
(geckodriver) and Chrome (chromedriver) running locally:

```yaml
test:
    runs: 5
start-seed: 2
quick-check:
    iterations: 20
    max-size: 100
weights:
    base: ["resources/html5-weights.edn", "resources/css3-weights.edn"]
    fixed: "resources/fixed-weights.edn"
compare:
    method: SQDIFF_NORMED
    threshold: 0.00003
web:
    host: 127.0.0.1
    port: 3000
    dir: "gen"
browsers:
    firefox:
      url: "http://localhost:7000"
      capabilities: {"moz:firefoxOptions":
                     {"args": ["--headless"]}}
    chrome:
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
    bs-chrome-win-62:
      url: "https://USER:KEY@hub-cloud.browserstack.com/wd/hub"
      capabilities: {"browserstack.local": true,
                     "browser": "Chrome",
                     "browser_version": "69.0",
                     "os": "Windows",
                     "os_version": "10",
                     "resolution": "1024x768"}
    bs-firefox-win-62:
      url: "https://USER:KEY@hub-cloud.browserstack.com/wd/hub"
      capabilities: {"browserstack.local": true,
                     "browser": "Firefox",
                     "browser_version": "62.0",
                     "os": "Windows",
                     "os_version": "10",
                     "resolution": "1024x768"}
    bs-edge-win-17:
      url: "https://USER:KEY@hub-cloud.browserstack.com/wd/hub"
      capabilities: {"browserstack.local": true,
                     "browser": "Edge",
                     "browser_version": "17.0",
                     "os": "Windows",
                     "os_version": "10",
                     "resolution": "1024x768"}
```


## Update HTML5 and CSS3 Generators (mend)

Generate Clojure generator source based on the HTML5 and CSS3 EBNF
grammars from [kanaka/html5-css3-ebnf](https://github.com/kanaka/html5-css3-ebnf).
You should only need to do the following if manual changes have been
made to the EBNF grammars or the grammar fixups/mangles in
`src/mend/grammar.clj`.

```
time lein run translate --mode html --namespace rend.html5-generators --function html5-generators --weights-output resources/html5-weights.edn --clj-output resources/rend/html5_generators.clj
```

Generate Clojure generator source based on the CSS3 EBNF grammar
(warning this requires about 2GB of free memory):

```
time lein run translate --mode css --namespace rend.css3-generators --function css3-generators --weights-output resources/css3-weights.edn --clj-output resources/rend/css3_generators.clj
```

Use the generators from a Clojure REPL:

```
lein repl
(require '[rend.generator])
(in-ns 'rend.generator)
(clojure.pprint/pprint (gen/sample (get-html-generator) 5))
```

## Images

The following images are included in static/:

* [File:Books-aj.svg\_aj\_ashton\_01g\*.png](https://commons.wikimedia.org/wiki/Openclipart#/media/File:Books-aj.svg_aj_ashton_01g.png) (Public Domain)
* [B\_stop.svg](https://commons.wikimedia.org/wiki/Openclipart#/media/File:B_stop.svg) (CC0)
* [Eilat_-_Dolphin\_reef\*.jpg](https://commons.wikimedia.org/wiki/Dolphin#/media/File:Eilat_-_Dolphin_reef.jpg) (CC BY-SA 3.0)
* [SpaceX\_Kestrel\_engine2\*.gif](https://commons.wikimedia.org/wiki/File:SpaceX_Kestrel_engine2.gif) (CC BY-SA-3.0)

## License

Copyright © Joel Martin

Distributed under the Mozilla Public License either version 2.0 or (at
your option) any later version.
