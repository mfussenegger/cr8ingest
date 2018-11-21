#!/usr/bin/env python3

import os
import doctest
import subprocess
import functools
from cr8.run_crate import CrateNode, get_crate


crate_dir = get_crate('latest-nightly')
node = CrateNode(
    crate_dir=crate_dir,
    settings={
        'cluster.name': 'cr8ingest-tests',
        'http.port': '44200-44250',
        'bootstrap.system_call_filter': False
    })


def setup(*args):
    pass


def teardown(*args):
    node.stop()


def transform(s):
    s = s.replace('localhost:4200', node.http_url)
    s = s.replace('port=5432', f'port={node.addresses.psql.port}')
    return (
        r'print(sh("""%s""").stdout.decode("utf-8"))' % s) + '\n'


class Parser(doctest.DocTestParser):

    def parse(self, string, name='<string>'):
        r = super().parse(string, name)
        for s in r:
            if isinstance(s, doctest.Example):
                s.source = transform(s.source)
        return r


def load_tests(loader, tests, ignore):
    env = os.environ.copy()
    env['CR8_NO_TQDM'] = 'True'
    node.start()
    assert node.http_host, "http_url must be available"
    tests.addTests(doctest.DocFileSuite(
        os.path.join('..', 'README.rst'),
        globs={
            'sh': functools.partial(
                subprocess.run,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                timeout=60,
                shell=True,
                env=env
            )
        },
        optionflags=doctest.NORMALIZE_WHITESPACE | doctest.ELLIPSIS,
        setUp=setup,
        tearDown=teardown,
        parser=Parser()
    ))
    return tests


if __name__ == "__main__":
    import unittest
    unittest.main()
