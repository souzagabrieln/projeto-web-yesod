# ---------- Etapa 1: compilacao ----------
# Imagem com as ferramentas para compilar o projeto com o Stack.
FROM debian:bookworm-slim AS build

# A imagem Debian vem sem locale UTF-8, e as ferramentas do GHC
# (happy, alex, o proprio compilador) falham ao ler arquivos com
# acentos. C.UTF-8 ja vem instalado, so precisa ser ativado.
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl git build-essential xz-utils \
        libgmp-dev libffi-dev zlib1g-dev libnuma-dev pkg-config \
        gnupg netbase

# Instala o Stack (o script usa o apt, por isso a lista de pacotes
# so e apagada depois dele)
RUN curl -sSL https://get.haskellstack.org/ | sh \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src

# Primeiro so os arquivos de configuracao: assim o Docker reaproveita
# o cache das dependencias quando apenas o codigo muda.
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack setup --install-ghc \
 && stack build --only-dependencies -j2

# Agora o codigo do projeto
COPY . .
RUN stack build -j2 \
 && mkdir -p /out \
 && cp "$(stack path --local-install-root)/bin/ProjetoWeb" /out/

# ---------- Etapa 2: execucao ----------
# Imagem final pequena, so com o executavel e os arquivos estaticos.
FROM debian:bookworm-slim

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates libgmp10 libffi8 zlib1g libnuma1 netbase \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /out/ProjetoWeb /app/ProjetoWeb
COPY static /app/static

# O banco SQLite fica em /data. No Render, monte um disco persistente
# nesse caminho para os dados nao se perderem a cada deploy.
RUN mkdir -p /data
ENV DB_PATH=/data/proj.sqlite3
ENV PORT=3000

EXPOSE 3000
CMD ["/app/ProjetoWeb"]
