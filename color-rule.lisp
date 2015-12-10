(setf *color-rule*
      '(("cpc=[a-zA-Z]*" "orange")
	("tag=[a-zA-Z0-9\.\+]*" "red")
	;;("sip:[\\[\\]a-fA-F0-9:.]*:5060" "purple")
	("o=- [0-9]* [0-9]*" "red")
	("@[a-zA-Z.]+" "blue")
	("audio [0-9]*" "red")
	("P-Early-Media: supported" "green")
	("[0-9]+[.][0-9.]+[.][0-9.]+[.][0-9]+" "red")
	;;("IP[46] [\\[\\]a-fA-F0-9:.]*" "red")
	("This-is-a-test-call:" "green")
	("This-is-a-message:" "red")))





