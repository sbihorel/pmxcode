PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGDATE = `date +%Y-%m-%d`
PKGYEAR = `date +%Y`

all: check

build:
	mkdir -p builds
	@rm -rf /tmp/$(PKGNAME)
	cp -R ./ /tmp/$(PKGNAME)
	rm /tmp/$(PKGNAME)/Makefile
	rm -rf /tmp/$(PKGNAME)/dev
	rm -rf /tmp/$(PKGNAME)/builds

	# set tags
	{ \
		cd /tmp/$(PKGNAME) ;\
		for f in `ls R/*.R man/*.Rd DESCRIPTION LICENSE`; do \
			echo "processing $$f" ;\
			sed \
				-e "s/\$$version\$$\?/$(PKGVERS)/g" \
				-e "s/\$$date\$$\?/$(PKGDATE)/g" \
				-e "s/\$$year\$$\?/$(PKGYEAR)/g" \
				< $$f > $$$$; \
			mv $$$$ $$f; \
		done; \
	}
	cd builds; R CMD build /tmp/$(PKGNAME)
	@rm -rf /tmp/$(PKGNAME)

check: build
	R CMD check builds/$(PKGNAME)_$(PKGVERS).tar.gz

install:
	R CMD INSTALL --library=$(R_LIBS_USER) builds/$(PKGNAME)_$(PKGVERS).tar.gz

clean:
	@rm -rf $(PKGNAME).Rcheck
