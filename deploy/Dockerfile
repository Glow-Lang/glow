# Build (from the top of the glow checkout) with:
# docker build -t mukn/glow -f deploy/Dockerfile .
# Push with:
# docker push mukn/glow:latest
# Pull with:
# docker pull mukn/glow:latest

FROM fahree/gerbil-utils
MAINTAINER fare@mukn.io

COPY --chown=user:user . /home/glow
WORKDIR /home/glow
RUN ./build.ss
