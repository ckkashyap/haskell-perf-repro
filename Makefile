all:
	@echo "Building..."
#@ghc -rtsopts driver.hs
	@ghc -rtsopts -prof -auto-all -caf-all driver.hs
	@if [ ! -e 'data' ]; then mkdir data; for i in  {1..100}; do cp template.xml data/$$i.xml ; done; fi
	@mkdir -p html
	@echo "Done"

clean:
	rm -f *.hi *.o *.exe
