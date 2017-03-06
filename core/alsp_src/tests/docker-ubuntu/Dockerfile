# Ubuntu Container for testing ALS Prolog

# Build Container: docker build -t ubuntu-alspro .
# Run tests:       docker run -it --rm ubuntu-alspro

# Run with shell:  docker run -it --rm ubuntu-alspro bash

# Running with X11 on Mac
# Setup XQuartz > Preferences > Security to allow all net connections:
#    [ ] Authenticate connections
#    [X] Allow connections from network clients
# Run: docker run -e DISPLAY=$(ipconfig getifaddr en1):0 -it --rm ubuntu-alspro bash
# (Note en1 is wireless interface)

FROM ubuntu
MAINTAINER Chuck Houpt <chuck@habilis.net>

# Install standard dev tools and 32-bit libraries
RUN dpkg --add-architecture i386 && apt-get -y update && apt-get -y install build-essential
RUN apt-get -y install git gcc-multilib tk-dev:i386

# Startup by cloning and building, followed by command
ENTRYPOINT git clone --depth 1 https://github.com/AppliedLogicSystems/ALSProlog.git \
&& cd ALSProlog/unix && bash "$@"

# Default command is to run tests
CMD make && make test
