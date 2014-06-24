all: 
	@echo "Building..."
#@ghc -rtsopts driver.hs
	@ghc driver.hs
	@if [ ! -e 'data' ]; then mkdir data; for i in  {1..3000}; do cp template.txt data/$$i.xml ; done; fi
	@echo "Done"



clean:
	rm -f *.hi *.o *.exe driver
