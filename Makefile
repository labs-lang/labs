.PHONY: all linux osx dir rmsentinels linux_cseq osx_cseq zip_linux

all: linux osx

osx: platform = osx.10.12-x64
osx_cseq: platform = osx.10.12-x64
linux: platform = linux-x64
linux_cseq: platform = linux-x64
zip_linux: platform = linux-x64

sources = $(wildcard **/*.fs)
sliver_sources = $(wildcard sliver/**/*.py) 
templates = $(wildcard LabsTranslate/templates/**/*)

VERSION := $(strip $(shell grep version sliver/__about__.py | grep = | sed 's/"//g' | awk 'NF{print $$NF}'))
RELEASENAME = sliver-v$(VERSION)_$(strip $(subst -,_, ${platform}))

# Always force to re-make py files and templates
rmsentinels :
	@rm -f build/${platform}/sliver.py
	@rm -f build/${platform}/vendor/cseq/cseq.py
	@rm -f build/${platform}/labs/templates/c/main.c

build/%/labs/templates/c/main.c : $(templates)
	@echo Copying templates...
	@cp -r LabsTranslate/templates build/$(platform)/labs;

build/%/labs/LabsTranslate : $(sources)
	@mkdir -p build/$(platform)
	@echo Building LabsTranslate...
	dotnet publish LabsTranslate/LabsTranslate.fsproj -r $(platform) -c Release --self-contained -o build/$(platform)/labs -p:PublishSingleFile=true -p:PublishTrimmed=true ;
	@rm build/${platform}/labs/*.pdb ;

build/%/sliver.py : $(sliver_sources)  build/%/pyparsing.py
	@mkdir -p build/$(platform)
	@echo Copying SLiVER...
	@cp -r sliver/ build/$(platform)/ ;
	@rm build/${platform}/HISTORY-dev ;
	@rm -rf build/${platform}/doc ;
	@rm -rf build/${platform}/..?* ;
	@rm -rf build/${platform}/.[!.]* ;
	

build/%/pyparsing.py :
	@mkdir -p build/$(platform)
	@echo Copying pyparsing.py...
	@cp -r pyparsing.py build/$(platform)/pyparsing.py ;

build/%/click :
	@mkdir -p build/$(platform)
	@echo Copying click...
	@cp -r click/src/click build/$(platform)/click ;
	@cp -r click/LICENSE.rst build/$(platform)/click/ ;
	@cp -r click/README.rst build/$(platform)/click/ ;

build/%/vendor/cseq/cseq.py :
	@mkdir -p build/$(platform)/vendor
	@echo Copying CSeq...
	@cp -r cseq/ build/$(platform)/vendor/cseq ;
	@cp -r sliver/info.py build/$(platform)/vendor/cseq/info.py ;
	@cp -r cseq-modules/* build/$(platform)/vendor/cseq/modules ;

build/%/examples :
	@mkdir -p build/$(platform)
	@echo Copying examples...
	@mkdir -p build/$(platform)/examples ;
	@cp labs-examples/*.labs build/$(platform)/examples/;

build/%/vendor/cbmc-simulator :
	@mkdir -p build/$(platform)/vendor
	@echo Copying backends to vendor/...
	@cp -r linux/* build/$(platform)/vendor/ ;

osx : rmsentinels \
	build/osx.10.12-x64/labs/LabsTranslate \
	build/osx.10.12-x64/labs/templates/c/main.c \
	build/osx.10.12-x64/pyparsing.py \
	build/osx.10.12-x64/click \
	build/osx.10.12-x64/sliver.py \
	build/osx.10.12-x64/examples

linux : rmsentinels \
	build/linux-x64/labs/LabsTranslate \
	build/linux-x64/labs/templates/c/main.c \
	build/linux-x64/pyparsing.py \
	build/linux-x64/click \
	build/linux-x64/sliver.py \
	build/linux-x64/examples \
	build/linux-x64/vendor/cbmc-simulator

osx_cseq: rmsentinels osx \
	build/osx.10.12-x64/vendor/cseq/cseq.py

linux_cseq: rmsentinels linux \
	build/linux-x64/vendor/cseq/cseq.py

zip_linux : linux_cseq
	@rm -rf build/$(RELEASENAME);
	@rm -f build/$(RELEASENAME).zip;
	cp -r build/$(platform) build/$(RELEASENAME)
	cd build && zip -r $(RELEASENAME).zip $(RELEASENAME)
	rm -rf build/$(RELEASENAME)
