FROM ghcr.io/matsubara0507/ubuntu-for-haskell:docker
ARG local_bin_path
COPY ${local_bin_path} /usr/local/bin
ENTRYPOINT ["/usr/local/bin/jobworker-runner"]
