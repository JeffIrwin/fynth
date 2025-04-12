
![](https://github.com/JeffIrwin/fynth/workflows/ci/badge.svg)

# fynth: a fortran synthesizer

Project _fynth_ is pronounced as _synth_, because the lowercase letter _S_ is
written as _f_.  [Typesetting](https://github.com/JeffIrwin/cali) is hard ok!

![](doc/fynth.png)

# Build and run

Use [fpm](https://fpm.fortran-lang.org/), the fortran package manager, with commands such as these:
```bash
fpm build
fpm run
```
<!-- fpm test -- does nothing yet -->

With `fpm run`, fynth will write `.wav` audio file(s) in the current directory.
I will probably change this soon to make `fpm test` write sample files, while
`fpm run` should take some kind of user input.

# Disambiguation

There is an unrelated although not dissimilar project also named fynth:  https://github.com/folkertvanheusden/fynth

