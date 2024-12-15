# Dockercontainer based on haskell image + ghcup + hls + Haskell version as variable
ARG HASKELL_VERSION

FROM haskell:${HASKELL_VERSION}

# Install ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- --install

# Ensure ghcup is available in the shell
ENV PATH="/root/.ghcup/bin:${PATH}"
RUN ghcup install hls
RUN cabal update && cabal install scotty-0.22 --lib

