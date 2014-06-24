haskell-perf-repro
==================

A dummy project to emulate the perf issue I am facing. I've written an
equivalent perl program for comparison. Looks like the perl program runs over
50 times faster! Clearly I am missing something.

```bash
kashyaps-mbp:haskell-perf-repro kashyapck$ make
Building...
[1 of 1] Compiling Main             ( driver.hs, driver.o )
Linking driver ...
Done
kashyaps-mbp:haskell-perf-repro kashyapck$ time ./driver > /dev/null

real	0m6.142s
user	0m5.824s
sys	0m0.313s
kashyaps-mbp:haskell-perf-repro kashyapck$ time ./driver.pl > /dev/null

real	0m0.145s
user	0m0.054s
sys	0m0.090s
```
