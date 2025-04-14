
FROM rockylinux:9

WORKDIR /workdir

RUN dnf update -y
RUN dnf install -y git
#RUN dnf install -y gfortran  # gfortran 11

RUN dnf install -y gcc-toolset-13-gcc-gfortran
ENV PATH="$PATH:/opt/rh/gcc-toolset-13/root/usr/bin/"

RUN gfortran --version

ADD https://github.com/fortran-lang/fpm/releases/download/current/fpm-linux-x86_64-gcc-12 ./fpm
RUN chmod +x fpm
RUN mv fpm /usr/local/bin
RUN fpm --version

ARG BRANCH="main"
RUN echo "BRANCH = $BRANCH"

#RUN echo 4  # bust cache
RUN git clone https://github.com/jeffirwin/fynth --branch "$BRANCH"
WORKDIR /workdir/fynth

RUN fpm run --profile debug -- --version
RUN fpm run --profile debug -- --help
RUN fpm run --profile debug

RUN fpm build --profile release
RUN fpm run --profile release

RUN fpm install --prefix /workdir/ --profile release
ENV PATH="$PATH:/workdir/bin/"

RUN fynth
RUN fynth sin.wav --sine   300 1
RUN fynth squ.wav --square 300 1
RUN fynth noi.wav --noise      1

RUN ./run-plot.sh
RUN ./run-fft-plot.sh

#RUN fpm test --profile debug
#RUN fpm test --profile release

