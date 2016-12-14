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

## Images

The following images are included in gen/static/:

* [File:Books-aj.svg\_aj\_ashton\_01g\*.png](https://commons.wikimedia.org/wiki/Openclipart#/media/File:Books-aj.svg_aj_ashton_01g.png) (Public Domain)
* [B\_stop.svg](https://commons.wikimedia.org/wiki/Openclipart#/media/File:B_stop.svg) (CC0)
* [Eilat_-_Dolphin\_reef\*.jpg](https://commons.wikimedia.org/wiki/Dolphin#/media/File:Eilat_-_Dolphin_reef.jpg) (CC BY-SA 3.0)
* [SpaceX\_Kestrel\_engine2\*.gif](https://commons.wikimedia.org/wiki/File:SpaceX_Kestrel_engine2.gif) (CC BY-SA-3.0)

## License

Copyright © 2016 Joel Martin

Distributed under the Mozilla Public License either version 2.0 or (at
your option) any later version.
