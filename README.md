# Tend

A suite of Clojure tools for generative (property-based) testing using
formal grammars (EBNF) with a focus on web browser rendering.

## Rend

* Prerequisites:
  * Install libopencv-dev:
```
sudo apt-get install libopencv-dev
```

  * Generate the opencv maven artifactrs:
```
make deps
```

* Download/build the webdriver drivers/browsers:
  * Firefox (https://developer.mozilla.org/en-US/Firefox/Headless_mode):
  ```
  curl -L https://github.com/mozilla/geckodriver/releases/download/v0.19.1/geckodriver-v0.19.1-linux64.tar.gz | tar xvzf -
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

* Start the webdriver drivers/browsers:
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
  ./mach run --release -z --webdriver 7002 --resolution 400x300
  ```

* Update `config.yaml` with browser webdriver connection information

* Start a separate minimal web server to monitor results
  at `http://localhost:9080/gen/...`:

```
python3 -m http.server 9080
```

* Start a test run:
```
lein with-profile rend run config.yaml
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

## Mend: EBNF Testing

All the following example use the `test/bc.ebnf` EBNF grammar file
which specifies a simple EBNF for generating commands that can be run
with the bc (arbitrary precission calculator) program.

Generate Clojure generators (one generator per EBNF rule named after
the non-terminal):

```
lein with-profile ebnf run clj test/bc.ebnf --namespace bc.test
```

Generate a single Clojure generator (one generator named `gen-gc`):

```
lein with-profile ebnf run clj test/bc.ebnf --namespace bc.test --function gen-bc
```

Generate 10 and then 100 samples:

```
lein with-profile ebnf run samples test/bc.ebnf tmp/samp%.bc
lein with-profile ebnf run samples test/bc.ebnf --samples 100 tmp/samp%.bc
```

Output the full set of weights to a file, modify the weights file and
then generate 10 samples using the modified weights file:

```
rm tmp/samp*
lein with-profile ebnf run samples test/bc.ebnf --weights-output tmp/bc-weights.edn tmp/samp%.bc
    # tweak 0 and 1 lower (10), +,- to 1000, *,/ to 2000
lein with-profile ebnf run samples test/bc.ebnf --weights tmp/bc-weights.edn tmp/samp%.bc
```

Run the test program using test samples, then update the weights file
to increase the likelihood of 0 numbers (and thus a failure due to
divide by zero):

```
rm tmp/samp*
lein with-profile ebnf run check test/bc.ebnf --weights tmp/bc-weights.edn --sample-dir tmp/ -- test/testbc.sh -q %
    # tweak 0 to increase frequency
lein with-profile ebnf run check test/bc.ebnf --weights tmp/bc-weights.edn --sample-dir tmp/ -- test/testbc.sh -q %
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
