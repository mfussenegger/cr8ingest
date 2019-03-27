=========
cr8ingest
=========

A command line tool to ingest JSON records into `CrateDB <https://github.com/crate/crate>`_.
For a more polished tool see `cr8 <https://github.com/mfussenegger/cr8>`_.


Example
=======

Create a table

::

  >>> crash --hosts localhost:4200 -c "CREATE TABLE t (id int, name string)"
  CONNECT OK
  CREATE OK, 1 row affected  (... sec)


Insert some JSON records::

  >>> echo '{"id": 1, "name": "Trillian"}' | cr8ingest --table t --db-uri "host=localhost user=crate dbname=doc port=5432"
  Columns: [("id","integer"),("name","text")]
  Rate: Nothing
  Concurrency: 15
  1 requests [op/s: ...  avg duration: ... (ms)]
  <BLANKLINE>


Installation
============

Clone the repo and run::

  $> stack install

`stack <https://docs.haskellstack.org/en/stable/README/>`_ must be installed.


Development
===========

::

  $> stack test
