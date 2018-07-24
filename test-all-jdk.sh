#! /usr/bin/env bash
set -e

if ! awk '{ split($3, a, "-"); if (a[1] < "17.05") exit 1 }' < <(docker --version); then
    >&2 echo 'this script requires docker 17.05 or greater'
    exit 1
fi

ns=jgpc42
insn=$ns/insn
deps=$insn.deps
openjdk6=$ns/openjdk:6

jdks=(
    "$openjdk6"
    openjdk:7
    openjdk:8
    openjdk:9
    openjdk:10
)

mkdir -p target
dockerfile=$(mktemp -p target)

docker build -t "$deps" .

cat <<EOF > "$dockerfile"
FROM ubuntu:14.04
RUN apt-get update && apt-get install -y openjdk-6-jdk
EOF
docker build -t "$openjdk6" -f "$dockerfile" .

for jdk in "${jdks[@]}"; do
    tag=$insn.${jdk#$ns/}
    [[ "$jdk" = *6 ]] && ver=2.7.1 || ver=
    cat <<EOF > "$dockerfile"
    FROM $jdk
    COPY . /root
    COPY --from=$deps /root /root
    WORKDIR /root
    ENV LEIN_ROOT=1
    CMD ["./lein$ver", "test-all"]
EOF
    docker build -t "$tag" -f "$dockerfile" .
    docker run --rm "$tag"
done

rm -f "$dockerfile"
