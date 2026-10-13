# X100 reference v0.4 (staged)

This directory contains the methodologically frozen 1 m X100 reference tables
for open lowland. It is staged side by side with the historical reference
files. Nothing in this directory is active unless pipeline code explicitly
selects `x100_reference.version = "1m_v0_4"`.

Scope was confirmed by project lead Tessa, as reported by Vegar Bakkestuen:
Naturlig aapne, Semi-naturlig and Vaatmark, excluding alpine zone.

The three `ref*_1m_v0_4.csv` files are operational stratum-level reference
tables. The national 0.6453 m value is a population-weighted summary, not a
single replacement value for every polygon.

Do not rename these files to the historical filenames without a reviewed code
change, regression test and explicit activation decision.
