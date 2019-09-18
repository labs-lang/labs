.PHONY: all linux osx dir

all: linux osx

osx: platform = osx.10.10-x64
linux: platform = linux-x64

sources = $(wildcard **/*.fs)
sliver_sources = $(wildcard sliver/**/*.py) 

dir :
	@mkdir -p build/$(platform)

build/%/labs/LabsTranslate.dll : $(sources) dir
	@echo Building LabsTranslate...
	dotnet publish -r $(platform) -c Release --self-contained -o ../build/$(platform)/labs
	@cp -r LabsTranslate/templates build/$(platform)/labs;

build/%/sliver.py : $(sliver_sources) build/%/click
	@echo Copying SLiVER...
	@cp -r sliver/ build/$(platform)/ ;

build/%/click : dir
	@echo Copying click...
	@cp -r click/click build/$(platform)/click ;

build/%/cseq : dir
	@echo Copying CSeq...
	@cp -r cseq build/$(platform)/cseq ;

build/%/examples : dir
	@echo Copying examples...
	@mkdir -p build/$(platform)/examples ;
	@cp examples/*.labs build/$(platform)/examples/;

osx : build/osx.10.10-x64/labs/LabsTranslate.dll \
	  build/osx.10.10-x64/sliver.py \
	  build/osx.10.10-x64/cseq \
	  build/osx.10.10-x64/examples

linux : build/linux-x64/labs/LabsTranslate.dll \
	build/linux-x64/sliver.py \
	build/linux-x64/cseq \
	build/linux-x64/examples
