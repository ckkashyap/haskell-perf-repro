all: 
	@echo "Building..."
#@ghc -rtsopts driver.hs
	@ghc -rtsopts -prof -auto-all -caf-all driver.hs
	@if [ ! -e 'data' ]; then mkdir data; for i in  {1..3000}; do cp template.xml data/$$i.xml ; done; fi
	@mkdir -p html
	@echo "Done"


data:
	mkdir data
	cp template.xml data/file_1.xml
	cp template.xml data/file_2.xml
	cp template.xml data/file_3.xml
	cp template.xml data/file_4.xml
	cp template.xml data/file_5.xml
	cp template.xml data/file_6.xml
	cp template.xml data/file_7.xml
	cp template.xml data/file_8.xml
	cp template.xml data/file_9.xml
	cp template.xml data/file_10.xml
	cp template.xml data/file_11.xml
	cp template.xml data/file_12.xml
	cp template.xml data/file_13.xml
	cp template.xml data/file_14.xml
	cp template.xml data/file_15.xml
	cp template.xml data/file_16.xml
	cp template.xml data/file_17.xml
	cp template.xml data/file_18.xml
	cp template.xml data/file_19.xml
	cp template.xml data/file_20.xml
	cp template.xml data/file_21.xml
	cp template.xml data/file_22.xml
	cp template.xml data/file_23.xml
	cp template.xml data/file_24.xml
	cp template.xml data/file_25.xml
	cp template.xml data/file_26.xml
	cp template.xml data/file_27.xml
	cp template.xml data/file_28.xml
	cp template.xml data/file_29.xml
	cp template.xml data/file_30.xml
	cp template.xml data/file_31.xml
	cp template.xml data/file_32.xml
	cp template.xml data/file_33.xml
	cp template.xml data/file_34.xml
	cp template.xml data/file_35.xml
	cp template.xml data/file_36.xml
	cp template.xml data/file_37.xml
	cp template.xml data/file_38.xml
	cp template.xml data/file_39.xml
	cp template.xml data/file_40.xml
	cp template.xml data/file_41.xml
	cp template.xml data/file_42.xml
	cp template.xml data/file_43.xml
	cp template.xml data/file_44.xml
	cp template.xml data/file_45.xml
	cp template.xml data/file_46.xml
	cp template.xml data/file_47.xml
	cp template.xml data/file_48.xml
	cp template.xml data/file_49.xml
	cp template.xml data/file_50.xml



clean:
	rm -f *.hi *.o *.exe
