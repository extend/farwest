Blank Farwest Project Example
=============================

Getting Started
---------------

Download, install and start [riak](https://github.com/basho/riak).

Download and install rebar and relcool and put them in your PATH.

 *  [rebar](https://github.com/rebar/rebar)
 *  [relcool](https://github.com/erlware/relcool)

Compile the project and all its dependencies:

``` bash
make
```

The release will be generated in `rel/$(PROJECT)/`.

Start the release:

``` bash
cd rel/$(PROJECT)
./bin/$(PROJECT)
```

Open http://localhost:8080 in your favorite browser.

Farwest UI
----------

The Farwest UI features a number of screens allowing you to lookup
or modify the state of the system.

 *  Dashboard
 *  Routes
 *  Templates
 *  User data
 *  User files
