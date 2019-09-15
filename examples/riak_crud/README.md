Description
-----------

Tests simple CRUD operations against Riak's HTTP API, logging each response.
Includes a simplified HTTP client abstraction in front of `cohttp`.

Usage
-----

```sh
$ dune build
```

Example output when Riak is off:

![1 fail, 3 skip](https://raw.githubusercontent.com/ibnfirnas/caravan/master/examples/riak_crud/screenshot-riak-off.png)

Children of a failed test are skipped, since they're assumed to depend on state
produced by the parent test.
