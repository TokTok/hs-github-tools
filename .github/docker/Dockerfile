# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["git", "clone", "https://github.com/TokTok/hs-github-tools", "/work"]
RUN ["stack", "build"]
RUN ["rm", "-rf", "/work"]
COPY . /work/
RUN ["stack", "build"]
