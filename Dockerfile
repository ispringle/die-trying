FROM clfoundation/sbcl:latest

RUN apt-get update && \
    apt-get install -y curl && \
    curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)' \
         --quit && \
    rm quicklisp.lisp && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY die-trying.asd /app/die-trying.asd
COPY main.lisp /app/main.lisp
COPY build.lisp /app/build.lisp
COPY www/ /app/www/

RUN sbcl --script /app/build.lisp