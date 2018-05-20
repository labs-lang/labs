.PHONY: publishlinux cplinux

all: cplinux

publishlinux:
	dotnet publish -r linux-x64 -c Release --self-contained

cplinux: publishlinux
	cp -r linux/ LabsTranslate/bin/Release/netcoreapp2.0/linux-x64/publish/

