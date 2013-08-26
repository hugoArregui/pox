run:

	chicken-install -s
	awful --development-mode pox-app.scm
	#awful  pox-app.scm
	#create sql/schema.sq
	#run sql/view.sql

clean:

	rm -f **/*.so **/*.o **/*import.* **/*.c *.so *import.*
	sudo rm -f -r /usr/lib/chicken/6/pox*
