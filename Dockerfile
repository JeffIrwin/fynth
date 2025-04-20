
FROM rockylinux:9

WORKDIR /workdir

RUN dnf update -y
RUN dnf install -y git
#RUN dnf install -y gfortran  # gfortran 11

RUN dnf install -y gcc-toolset-13-gcc-gfortran
ENV PATH="$PATH:/opt/rh/gcc-toolset-13/root/usr/bin/"
RUN gfortran --version

RUN dnf install -y epel-release  # pre-req for gnuplot
RUN dnf install -y gnuplot

ADD https://github.com/fortran-lang/fpm/releases/download/current/fpm-linux-x86_64-gcc-12 ./fpm
RUN chmod +x fpm
RUN mv fpm /usr/local/bin
RUN fpm --version

ARG BRANCH="main"
RUN echo "BRANCH = $BRANCH"

RUN echo 10  # bust cache
RUN git clone https://github.com/jeffirwin/fynth --branch "$BRANCH"
WORKDIR /workdir/fynth

# Unit tests
RUN fpm test --profile debug
RUN fpm test --profile release

RUN fpm run --profile debug -- --version
RUN fpm run --profile debug -- --help
RUN fpm run --profile debug

RUN fpm build --profile release
RUN fpm run --profile release

RUN fpm install --prefix /workdir/ --profile release
ENV PATH="$PATH:/workdir/bin/"

#===========================================================
# Examples.  If these break, docs probably need updated

RUN fynth
RUN fynth lic.wav --licc
RUN fynth sin.wav --sine   300 1
RUN fynth squ.wav --square 300 1
RUN fynth tri.wav --tri    300 1
RUN fynth saw.wav --saw    300 1
RUN fynth noi.wav --noise  300 1

# ADSR envelopes
RUN fynth squ-adsr.wav --square 300 1 --adsr 0.3 0.2 0.5 1.0
RUN fynth tri-adsr.wav --tri    300 1 --adsr 0.3 0.2 0.5 1.0
RUN fynth saw-adsr.wav --saw    300 1 --adsr 0.3 0.2 0.5 1.0

# Two-pole low pass filtering
RUN fynth squ-filt.wav --square 300 1 --two-pole 1500
RUN fynth tri-filt.wav --tri    300 1 --two-pole 1500
RUN fynth saw-filt.wav --saw    300 1 --two-pole 1500

# Combine filter with amplitude envelope
RUN fynth squ-env-filt.wav --squ 300 1 --adsr 0.3 0.2 0.5 1 --2pole 1500
RUN fynth tri-env-filt.wav --tri 300 1 --adsr 0.3 0.2 0.5 1 --2pole 1500
RUN fynth saw-env-filt.wav --saw 300 1 --adsr 0.3 0.2 0.5 1 --2pole 1500

RUN fynth sin.wav sin-copy.wav  # echo input to output
RUN fynth sin.wav sin.csv       # time-domain conversion
RUN fynth sin.wav sin.csv --fft # freq-domain conversion

RUN fynth squ.wav squ-low.wav --low-pass 1600  # FFT filter

#===========================================================

RUN ./scripts/run-plot.sh
RUN ./scripts/run-fft-plot.sh
RUN ./scripts/run-low-pass-plot.sh
RUN ./scripts/run-two-pole.sh

RUN sha256sum *.wav build/*.wav

