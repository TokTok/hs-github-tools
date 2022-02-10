# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-github-tools"]
COPY --chown=builder:users . /work/hs-github-tools
RUN ["stack", "build", "github-tools"]
