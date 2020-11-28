.PHONY: lint
lint:
			$(eval FILES := $(shell find . -type f -name "*.hs" -not -path "*/.git/*" -not -path "*/dist-newstyle/*" -not -path "*/.stack-work/*" -not -path "*/memo/*"))
			hlint $(FILES)

.PHONY: cabal-fmt
cabal-fmt:
			$(eval CABALS := $(shell find . -type f -name "*.cabal" -not -path "*/.git/*" -not -path "*/dist-newstyle/*" -not -path "*/.stack-work/*" -not -path "*/memo/*"))
			cabal-fmt -i $(CABALS)
			cabal-fmt -i -n cabal.project

.PHONY: fmt
fmt:
			$(eval FILES := $(shell find . -type f -name "*.hs" -not -path "*/.git/*" -not -path "*/dist-newstyle/*" -not -path "*/.stack-work/*" -not -path "*/memo/*"))
			ormolu -o -XTypeApplications -o -XBangPatterns --mode inplace $(FILES)