#! /usr/bin/env zsh

set -v

curl -w '\n' localhost:8080/foo/false
curl -w '\n' localhost:8080/bar 
curl -w '\n' -H "Content-Type: application/json" -d '["a"]' localhost:8080/baz
