# Ubuntu Container for development/testing ALS Prolog
#
# Build Container: docker build -t ubuntu-alspro .
# 
# Run tests of git master:
#   docker run -it --rm ubuntu-alspro
#
# Run shell for dev on local workdir:
#   docker run -it --rm -v $(git rev-parse --show-toplevel):/ALSProlog ubuntu-alspro bash

# Running with X11 on Mac
# Setup XQuartz > Preferences > Security to allow all net connections:
#    [ ] Authenticate connections
#    [X] Allow connections from network clients
#
# Run (Note en1 is wireless interface):
# docker run -e DISPLAY=$(ipconfig getifaddr en1):0 \
#    -v $(git rev-parse --show-toplevel):/ALSProlog \
#    -it --rm ubuntu-alspro bash

FROM ubuntu
LABEL maintainer Chuck Houpt <chuck@habilis.net>

ENV DEBIAN_FRONTEND noninteractive

# Install standard dev tools and 32-bit libraries
RUN dpkg --add-architecture i386 && apt-get -y update && apt-get -y install build-essential
RUN apt-get -y install git gcc-multilib ruby ruby-dev zlib1g-dev php tk-dev:i386 libcurl3-dev:i386
ENV LANG C.UTF-8
RUN gem install bundler

# Not strictly necessary, but speeds up builds by pre-installing
RUN gem install jekyll html-proofer

# Default command is to clone and run tests from Github master
CMD git clone --depth 1 https://github.com/AppliedLogicSystems/ALSProlog.git /tmp/alsptest \
&& cd /tmp/alsptest/unix && make test
