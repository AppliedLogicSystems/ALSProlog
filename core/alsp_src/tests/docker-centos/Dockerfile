# CentOS Container for development/testing ALS Prolog

# Build Container: docker build -t centos-alspro .
# 
# Run tests of git master:
#   docker run -it --rm centos-alspro
#
# Run shell for dev on local workdir:
#   docker run -it --rm -v $(git rev-parse --show-toplevel):/ALSProlog centos-alspro bash

# Running with X11 on Mac
# Setup XQuartz > Preferences > Security to allow all net connections:
#    [ ] Authenticate connections
#    [X] Allow connections from network clients
#
# Run (Note en1 is wireless interface):
# docker run -e DISPLAY=$(ipconfig getifaddr en1):0 \
#    -v $(git rev-parse --show-toplevel):/ALSProlog \
#    -it --rm centos-alspro bash

FROM centos
LABEL maintainer Chuck Houpt <chuck@habilis.net>

# Install standard dev tools and 32-bit libraries
RUN yum -y groupinstall 'Development Tools'
RUN yum -y install ruby ruby-dev php glibc-devel.i686 libgcc.i686 \
  tcl-devel.i686 tcl.i686 tk-devel.i686 tk.686 \
  libcurl-devel.i686
RUN gem install bundler

# Oddly, these fonts are needed to run as client
RUN yum -y install dejavu-sans-fonts

# Default command is to clone and run tests from Github master
CMD git clone --depth 1 https://github.com/AppliedLogicSystems/ALSProlog.git /tmp/alsptest \
&& cd /tmp/alsptest/unix && make test
