.PHONY: publishlinux linux publishmac mac sliverlinux slivermac

all: linux mac

publishlinux:
	dotnet publish -r linux-x64 -c Release --self-contained -o ../build/linux_x64/labs

sliverlinux:
	cp -r sliver/ build/linux_x64/;

slivermac:
	cp -r sliver/ build/osx_x64/;

linux: publishlinux sliverlinux
	cp -r linux/ build/linux_x64/;
	mv build/linux_x64/libunwind build/linux_x64/labs/libunwind;
	mkdir -p build/linux_x64/examples;
	cp examples/*.labs build/linux_x64/examples/;
	cp -r LabsTranslate/templates build/linux_x64/labs;

publishmac: build/osx_64/labs/LabsTranslate.dll

build/osx_64/labs/LabsTranslate.dll:
	dotnet publish -r osx.10.10-x64 -c Release -o ../build/osx_x64/labs

mac: publishmac slivermac
	mkdir -p build/osx_x64/examples;
	cp examples/*.labs build/osx_x64/examples/;
	cp -r LabsTranslate/templates build/osx_x64/labs;

