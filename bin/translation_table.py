#!/usr/bin/env python

import sys
from Bio.Data import CodonTable

if len(sys.argv) == 2:
    print CodonTable.unambiguous_dna_by_id[int(sys.argv[1])]
else:
    print "usage:", sys.argv[0], "table_number"
    exit(1)
