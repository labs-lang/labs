.PHONY: publishlinux cplinux

all: cplinux

publishlinux:
	dotnet publish -r linux-x64 -c Release --self-contained -o ../build/linux_x64

cplinux: publishlinux
	cp -r linux/ build/linux_x64/;
	cp -r examples build/linux_x64/

