FROM nixos/nix:latest AS builder

WORKDIR /react-trace
COPY . .

CMD ["nix", "develop", "--extra-experimental-features", "nix-command", "--extra-experimental-features", "flakes"]
