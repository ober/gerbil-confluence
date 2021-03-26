FROM jaimef/alpine:static

MAINTAINER jaimef@linbsd.org

COPY . /root/confluence
ENV PATH "$PATH:/root/gerbil/bin"
ENV GERBIL_HOME "/root/gerbil"
RUN gxpkg install github.com/ober/oberlib
RUN gxpkg link confluence /root/confluence
RUN gxpkg build confluence
COPY /root/.gerbil/bin/confluence /bin/confluence
CMD ["confluence"]
