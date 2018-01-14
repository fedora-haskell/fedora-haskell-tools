help:
	@echo -n "devel targets: git-tag sdist version git-push upload copy"
	@if [ -f Makefile.local ]; then echo " help-local"; else echo ""; fi

sdist:
	./make-dist $(VERSION)

upload:
	cabal upload dist/$(NAME)-$(VERSION).tar.gz

NAME= fedora-haskell-tools
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

version:
	@echo $(VERSION)

git-tag:
	git tag $(VERSION)

git-push:
	git push
	git push --tags

copy:
	cp -p dist/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/$(NAME)/master

-include Makefile.local
