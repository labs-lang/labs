.PHONY: all linux osx dir rmsliver

all: linux osx

osx: platform = osx.10.10-x64
linux: platform = linux-x64

sources = $(wildcard **/*.fs)
sliver_sources = $(wildcard sliver/**/*.py) 


# Always force to re-make py files
rmsliver :
	@rm -f build/${platform}/sliver.py

build/%/labs/LabsTranslate.dll : $(sources)
	@mkdir -p build/$(platform)
	@echo Building LabsTranslate...
	dotnet publish -r $(platform) -c Release --self-contained -o build/$(platform)/labs
	@cp -r LabsTranslate/templates build/$(platform)/labs;

build/osx.10.10-x64/sliver.py : $(sliver_sources) build/osx.10.10-x64/click
	@mkdir -p build/$(platform)
	@echo Copying SLiVER...
	@cp -r sliver/ build/$(platform)/ ;

build/linux-x64/sliver.py : $(sliver_sources) build/%/click
	@mkdir -p build/$(platform)
	@echo Copying SLiVER...
	@cp -r sliver/ build/$(platform)/ ;

build/%/click :
	@mkdir -p build/$(platform)
	@echo Copying click...
	@cp -r click/click build/$(platform)/click ;

build/%/cseq :
	@mkdir -p build/$(platform)
	@echo Copying CSeq...
	@cp -r cseq/ build/$(platform)/cseq/ ;
	@cp -r sliver/info.py build/$(platform)/cseq/info.py ;

build/%/examples :
	@mkdir -p build/$(platform)
	@echo Copying examples...
	@mkdir -p build/$(platform)/examples ;
	@cp examples/*.labs build/$(platform)/examples/;

build/%/cbmc-simulator:
	@mkdir -p build/$(platform)
	@echo Copying CBMC...
	@cp linux/cbmc/* build/$(platform)/ ;

osx : rmsliver \
	  build/osx.10.10-x64/labs/LabsTranslate.dll \
	  build/osx.10.10-x64/sliver.py \
	  build/osx.10.10-x64/cseq \
	  build/osx.10.10-x64/examples

linux : rmsliver \
	build/linux-x64/labs/LabsTranslate.dll \
	build/linux-x64/sliver.py \
	build/linux-x64/sliver.py \
	build/linux-x64/cseq \
	build/linux-x64/examples \
	build/linux-x64/cbmc-simulator

