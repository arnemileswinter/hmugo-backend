FROM alpine:latest
LABEL de.arnemileswinter.images.authors="arnemileswinter@gmail.com"

RUN apk --update add git less openssh && \
        rm -rf /var/lib/apt/lists/* && \
        rm /var/cache/apk/*

RUN apk --update add hugo && \
        rm -rf /var/lib/apt/lists/* && \
        rm /var/cache/apk/*

RUN apk --update add libc6-compat gmp

RUN git config --global user.email "author@example.com" && \
    git config --global user.name "hmugo"

COPY ssh-config /root/.ssh

WORKDIR /site
ENV API_KEY=changeme

COPY build/hmugo /bin/hmugo

RUN chmod 755 /bin/hmugo

EXPOSE 80/tcp
CMD hmugo