default : reST-test-out.html

Process.exe : Process.hs XmlToHtml.hs
	ghc --make $< -main-is Process -o $@

%-out.html : %.xml Process.exe
	./Process.exe $< $@

%.xml : %.rst
	rst2xml.py $< $@

%.html : %.rst
	rst2html.py $< $@