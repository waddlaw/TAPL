#!/bin/bash
find . -type f -name "*hs" -not -path '.git' -not -path '*.stack-work*' -exec stylish-haskell -i {} \;