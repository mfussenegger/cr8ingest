=========
cr8ingest
=========

A command line tool to ingest data into `CrateDB <https://github.com/crate/crate>`_.


Example
=======

Create a table

::

  >>> crash --hosts localhost:4200 -c "CREATE TABLE t (id int, name string)"
  CONNECT OK
  CREATE OK, 1 row affected  (... sec)


Insert some JSON records::

  >>> echo '{"id": 1, "name": "Trillian"}' | cr8ingest --table t --db-uri "host=localhost user=crate dbname=doc port=5432"
  [("id","integer"),("name","string")]
  "rate: Nothing"
  "concurrency: 15"
  op/s: Infinity  avg duration: ...
  done
