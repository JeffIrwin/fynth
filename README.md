
![](https://github.com/JeffIrwin/fynth/workflows/ci/badge.svg)

# fynth: a fortran synthesizer

Project _fynth_ is pronounced as _synth_, because the lowercase letter _S_ is
written as _f_.  [Typesetting](https://github.com/JeffIrwin/cali) is hard ok!

<!-- ![](doc/fynth.png) -->
<img src="https://raw.githubusercontent.com/JeffIrwin/fynth/refs/heads/main/doc/fynth.png" alt="fynth logo" width="50%">

# Build and run

Use [fpm](https://fpm.fortran-lang.org/), the fortran package manager, with commands such as these:
```bash
fpm build
fpm run -- sine.wav --sine 300 1
fpm install
```
<!-- fpm test -- does nothing yet -->

With `fpm install`, the `fynth` binary is installed to the default fpm path,
usually `~/.local/bin`.

## Examples

Write a file `sine.wav` with a sine wave at 300 Hz for 1 s:
```bash
fynth sine.wav --sine 300 1
```

Write a file `square.wav` with a square wave at 300 Hz for 1 s:
```bash
fynth square.wav --square 300 1
```

# Disambiguation

There is an unrelated although not dissimilar project also named fynth:  https://github.com/folkertvanheusden/fynth

