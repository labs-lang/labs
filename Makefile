.PHONY: publishlinux linux publishmac mac 

all: linux mac

publishlinux:
	dotnet publish -r linux-x64 -c Release --self-contained -o ../build/linux_x64/labs

linux: publishlinux
	cp -r linux/ build/linux_x64/;
	mv build/linux_x64/libunwind build/linux_x64/core/libunwind;
	cp examples/flock.labs build/linux_x64;

publishmac:
	dotnet publish -r osx.10.10-x64 -c Release -o ../build/osx_x64/labs

mac: publishmac
	cp -r linux/ build/osx_x64/;
	mv build/osx_x64/libunwind build/osx_x64/core/libunwind;
	cp examples/flock.labs build/osx_x64;


