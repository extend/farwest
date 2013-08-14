Farwest
=======

Farwest is a modern web application development platform.

Goals
-----

Farwest allows people with no Erlang knowledge to painlessly
build frontends to Erlang applications.

Farwest also provides UI components for allowing subsequent
users to perform various tasks without the help of a programmer.

Usage
-----

The Farwest project is an empty template you can use to build
applications. By default it provides you with nothing but an
administration UI.

To build, you need Erlang R16B+, relx and GNU Make installed.

``` bash
$ make
```

Farwest temporarily uses Riak for storing content. You need
to setup a working cluster on your local environment by following
the steps found in the documentation for Farwest to work:

 *  http://docs.basho.com/riak/latest/quickstart/

The Farwest server can then be started.

``` bash
$ ./rel/farwest/bin/farwest console
```

You can now access the administration UI at the following URL:

 *  http://localhost:8080/farwest

Farwest is still very unstable, features may or may not work
at this point in time.

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
 *  [Commercial Support](http://ninenines.eu/support)
