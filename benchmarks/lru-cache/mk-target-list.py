#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np
import subprocess
import sys

page_counts = np.genfromtxt(sys.stdin, dtype=None, delimiter=' ', names=['page','n'])
print(f'loaded {len(page_counts)} hits...', file=sys.stderr)

page_counts['n'] = np.minimum(page_counts['n'], 1000)
page_counts = page_counts[page_counts['n'] > 5]
page_counts[page_counts['page'] == u'1146']['n'] = 0
print(f'flattened: {len(page_counts)}', file=sys.stderr)

n = 1000000
probs = page_counts['n'] / sum(page_counts['n'])
titles = np.random.choice(a=page_counts['page'], size=n, p=probs)

print('\n'.join(titles))
