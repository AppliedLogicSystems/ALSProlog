# CentOS Container for testing ALS Prolog

# Build Container: docker build -t centos-alspro .
# Run tests:       docker run -it --rm centos-alspro

# Run with shell:  docker run -it --rm centos-alspro bash

# Running with X11 on Mac
# Setup XQuartz > Preferences > Security to allow all net connections:
#    [ ] Authenticate connections
#    [X] Allow connections from network clients
# Run: docker run -e DISPLAY=$(ipconfig getifaddr en1):0 -it --rm centos-alspro bash
# (Note en1 is wireless interface)

FROM centos
MAINTAINER Chuck Houpt <chuck@habilis.net>

# Install standard dev tools and 32-bit libraries
RUN yum -y groupinstall 'Development Tools'
RUN yum -y install glibc-devel.i686 libgcc.i686 tcl-devel.i686 tcl.i686 tk-devel.i686 tk.686

# Oddly, these fonts are needed to run as client
RUN yum -y install dejavu-sans-fonts

# Startup by cloning and building, followed by command
ENTRYPOINT git clone --depth 1 https://github.com/AppliedLogicSystems/ALSProlog.git \
&& cd ALSProlog/unix && make && bash "$@"

# Default command is to run tests
CMD make test
