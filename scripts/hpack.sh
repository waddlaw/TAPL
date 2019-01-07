#!/bin/bash

hpack

for file in subs/*; do
  hpack ${file}
done