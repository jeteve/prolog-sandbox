FROM mcr.microsoft.com/devcontainers/base:jammy
RUN apt-get update && \
    apt-get install -y software-properties-common && \
    apt-add-repository ppa:swi-prolog/stable && \
    apt-get update && \
    apt-get install -y swi-prolog
