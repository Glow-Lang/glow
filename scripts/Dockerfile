# Build (from the top of the glow checkout) with:
# docker build -t mukn/glow -f scripts/Dockerfile .
# Push with:
# docker push mukn/glow:latest
# Pull with:
# docker pull mukn/glow:latest

FROM fahree/gerbil-utils
MAINTAINER fare@mukn.io

RUN \
cd /root ; \
git clone https://gitlab.com/mukn/glow.git ; \
cd glow ; \
git_version="$(glow git-version)" ; \
git reset --hard ${git_version} ; \
echo > /root/glow-env.sh export GERBIL_ETHEREUM_SRC="$(nix path-info -f $nixpkgs gerbilPackages-unstable.gerbil-ethereum.src)"; \
echo >> /root/glow-env.sh export GLOW_HOME=/root/glow GERBIL_APPLICATION_HOME=/root/glow GLOW_SRC=/root/glow ;

# gxi -e '(import :mukn/ethereum/scripts/run-geth-test-net)'
# $GERBIL_ETHEREUM_SRC/scripts/run-geth-test-net.ss

WORKDIR /root/glow
