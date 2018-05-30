.PHONY: publishlinux cplinux

all: cplinux

publishlinux:
	dotnet publish -r linux-x64 -c Release --self-contained -o ../build/linux_x64/core

cplinux: publishlinux
	cp -r linux/ build/linux_x64/;
	mv build/linux_x64/libunwind/* build/linux_x64/core/libunwind;
	cp -r examples build/linux_x64;
	rm -rf build/linux_x64/libunwind

