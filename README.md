
# Opam-build

Extract and optionally execute the build commands from an opam file.

For example, run the build commands for this project.

```sh
dune exec -- opam-build opam-build.opam --exec --name opam-build
```

A more useful example would be:

```sh
$ opam source ao.0.2.4
Successfully extracted to /home/mtelvers/ao.0.2.4
$ cd ao.0.2.4
$ opam-build ao.opam --name ao --exec
```
