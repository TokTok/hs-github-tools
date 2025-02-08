FROM toxchat/haskell:hs-github-tools AS build
FROM ubuntu:20.04

# hadolint ignore=DL3008
RUN apt-get update \
 && DEBIAN_FRONTEND="noninteractive" apt-get install -y --no-install-recommends \
 ca-certificates \
 git \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /bin/webservice /app/
CMD ["/app/webservice"]
