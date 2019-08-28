#!/bin/bash

ormolu --mode inplace $(find app bench src subs -type f -name "*.hs" -not -path '.git' -not -path '*.dist-newstyle' -not -path '*.stack-work*')