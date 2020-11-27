#!/usr/bin/env python
# -*- coding: utf-8 -*-


"""
scp jelly.cs.unh.edu:trec-car-paragraphs.txt.gz .
gunzip trec-car-paragraphs.txt.gz
scp jelly.cs.unh.edu:~/trec-car/data/enwiki-20161220/release-v2.1.1/benchmarkY1train.v201.page-queries.tsv .
"""

from pathlib import Path
import subprocess
import random

dest = Path('trec-car-paragraphs')

def mk_queries(corpus_dir: Path,
               queries_file: Path,
               corpus_size: int = 10000,
               chunk_size: int = 100,
               queries_per_chunk: int = 10):
    queries = []
    #queries += [ query for query in open('queries').read().split('\n') if query != "" ]
    queries += \
            [ query.split('\t')[1]
              for query in queries_file.read_text().split('\n')
              if query != "" ]

    out = open('commands', 'w')

    for n, path in enumerate(corpus_dir.glob("*")):
        if n > corpus_size: break

        if n % chunk_size == 0:
            for query in random.choices(queries, k=queries_per_chunk):
                out.write(f'?{query}\n')

        out.write(f'+{path}\n')

def main():
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('--corpus-dir', metavar="DIR", type=Path, help="Directory containing corpus shards")
    parser.add_argument('--queries', metavar="FILE", type=Path, help="File containing query list")
    parser.add_argument('--corpus-size', type=int, help="Bound number of documents ingested")
    args = parser.parse_args()

    mk_queries(corpus_dir = args.corpus_dir, queries_file = args.queries, corpus_size = args.corpus_size)

if __name__ == "__main__":
    main()
