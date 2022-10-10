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

VERSION := $(strip $(shell grep version sliver/sliver/app/__about__.py | grep = | sed 's/"//g' | awk 'NF{print $$NF}'))
RELEASENAME = sliver-v$(VERSION)_$(strip $(subst -,_, ${platform}))
BUILD_DIR = build/$(platform)
SLIVER_DIR = $(BUILD_DIR)/sliver

# Always force to re-make py files and templates
rmsentinels :
	@rm -f $(BUILD_DIR)/sliver.py
	@rm -f $(BUILD_DIR)/click/core.py
	@rm -f $(BUILD_DIR)/pyparsing.py
	@rm -f $(SLIVER_DIR)/labs/templates/c/main.c

build/%/sliver/labs/templates/c/main.c : $(templates)
	@echo Copying templates...
	@cp -r LabsTranslate/templates/ $(SLIVER_DIR)/labs/templates/;

build/%/sliver/labs/LabsTranslate : $(sources)
	@mkdir -p $(BUILD_DIR)
	@echo Building LabsTranslate...
	dotnet publish LabsTranslate/LabsTranslate.fsproj -r $(platform) -c Release --self-contained -o $(SLIVER_DIR)/labs -p:PublishSingleFile=true -p:PublishTrimmed=true ;
	@rm $(SLIVER_DIR)/labs/*.pdb ;

build/%/sliver.py : $(sliver_sources)  build/%/pyparsing.py
	@mkdir -p $(BUILD_DIR)
	@echo Copying SLiVER...
	@cp -r sliver/sliver $(dir $@)/ ;
	@cp -r sliver/sliver.py $@ ;
	@cp -r sliver/HISTORY $(dir $@)/ ;
	@cp -r sliver/LICENSE $(dir $@)/ ;
	@cp -r sliver/README.* $(dir $@)/ ;

	@rm -rf build/${platform}/..?* ;
	@rm -rf build/${platform}/.[!.]* ;

build/%/pyparsing.py :
	@mkdir -p build/$(platform)
	@echo Copying pyparsing.py...
	@cp -r pyparsing.py $@ ;

build/%/click/core.py :
	@mkdir -p build/$(platform)
	@echo Copying click...
	@cp -r click/src/click/ $(dir $@) ;
	@cp -r click/LICENSE.rst $(dir $@) ;
	@cp -r click/README.rst $(dir $@) ;

build/%/sliver/cseq/cseq.py :
	@echo Copying CSeq...
	@cp -r cseq/ $(dir $@) ;
	@cp -r sliver/sliver/app/info.py $(dir $@)/info.py ;
	@cp -r cseq-modules/* $(dir $@)/modules ;

build/%/examples :
	@mkdir -p build/$(platform)
	@echo Copying examples...
	@mkdir -p $@ ;
	@cp labs-examples/*.labs $@ ;

build/%/sliver/cbmc/cbmc-simulator :
	@echo Copying cbmc...
	@cp -rf linux/cbmc $(SLIVER_DIR)/ ;

build/linux-x64/sliver/minisat/minisat :
	@echo Copying minisat...
	@cp -rf linux/minisat $(SLIVER_DIR)/ ;

build/osx.10.12-x64/sliver/minisat/minisat :
	@echo Copying minisat...
	@cp -rf osx/minisat $(SLIVER_DIR)/ ;

osx : rmsentinels \
	build/osx.10.12-x64/sliver/labs/LabsTranslate \
	build/osx.10.12-x64/sliver/labs/templates/c/main.c \
	build/osx.10.12-x64/pyparsing.py \
	build/osx.10.12-x64/click/core.py \
	build/osx.10.12-x64/sliver.py \
	build/osx.10.12-x64/examples \
	build/osx.10.12-x64/sliver/minisat/minisat

linux : rmsentinels \
	build/linux-x64/sliver/labs/LabsTranslate \
	build/linux-x64/sliver/labs/templates/c/main.c \
	build/linux-x64/pyparsing.py \
	build/linux-x64/click/core.py \
	build/linux-x64/sliver.py \
	build/linux-x64/examples \
	build/linux-x64/sliver/cbmc/cbmc-simulator \
	build/linux-x64/sliver/minisat/minisat

osx_cseq: rmsentinels osx \
	build/osx.10.12-x64/sliver/cseq/cseq.py

linux_cseq: rmsentinels linux \
	build/linux-x64/sliver/cseq/cseq.py

zip_linux : linux_cseq
	@rm -rf build/$(RELEASENAME);
	@rm -f build/$(RELEASENAME).zip;
	cp -r build/$(platform) build/$(RELEASENAME)
	cd build && zip -r $(RELEASENAME).zip $(RELEASENAME)
	rm -rf build/$(RELEASENAME)
