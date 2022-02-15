FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-github-tools"]
COPY --chown=builder:users . /work/hs-github-tools/
RUN ["stack", "install", "github-tools", "--fast", "--ghc-options", "-j8"]

FROM scratch
COPY --from=build /home/builder/.local/ /
