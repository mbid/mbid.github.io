FROM ghcr.io/mbid/rumpelpod/dev

USER root

RUN apt-get update && apt-get install -y --no-install-recommends \
        git \
        bash \
        ca-certificates \
        python3 \
        pandoc \
        texlive-latex-base \
        texlive-latex-recommended \
        texlive-latex-extra \
        texlive-fonts-recommended \
        texlive-xetex \
        dvisvgm \
    && rm -rf /var/lib/apt/lists/*

RUN id -u user >/dev/null 2>&1 || useradd --create-home --shell /bin/bash user

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

USER user

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["sleep", "infinity"]
