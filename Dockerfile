from gerbil/scheme:latest

MAINTAINER jaimef@linbsd.org
COPY . /root/confluence
ENV PATH "$PATH:/root/gerbil/bin"
ENV GERBIL_HOME "/root/gerbil"
RUN cd /root/confluence && ./build.ss static
RUN cp /root/confluence/confluence /bin/confluence
RUN rm -rf /root/gerbil /root/gambit
CMD /bin/bash
