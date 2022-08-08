
FORM = ui_mainview.py
MAIN = gui.py

.PHONY: all compile run clean test

all: compile

compile: $(FORM)

ui_%.py: %.ui
	pyuic5 $< -o $@

run: compile
	@DISPLAY=:0 ./$(MAIN) 2>/dev/null

clean:
	rm -rf __pycache__/
	rm -f $(FORM) *.pyc

test: compile
	(echo '{"event" : "all data",' \
	;echo '           "running":false,' \
	;echo '           "workers":[["w01","one","stud","(1+2)+3","1+2+3",0,0,0,"no",false],' \
	;echo '                      ["w02","two","misc","(5+6)+7","5+6+7",5,0,0,"no",false]],' \
	;echo '           "problems":[["prob0","",5000,""],["prob1","",6000,""]],' \
	;echo '           "problem idx":0,' \
	;echo '           "round":1,' \
	;echo '           "worker input":["[1,2,3]","42"],' \
	;echo '           "state":"empty"}' \
	;sleep 2 \
	;echo '{"event" : "worker input changed", "worker input":["[1,2,3,4]","42"]}' \
	;echo '{"event" : "worker updated", "worker data":["w01","(4+4)","4+4",0,0,0,"no",true]}' \
	;echo '{"event" : "worker updated", "worker data":["w02","(5+5)","5+5",0,0,0,"no",true]}' \
	;echo '{"event" : "round started", "round number":1}' \
	;sleep 3 \
	;echo '{"event" : "worker updated", "worker data":["w01","(1+2)","new new new",100,0,0,"no",true]}' \
	;sleep 0.5 \
	;echo '{"event" : "worker updated", "worker data":["w02","(5+2)","NEW NEW NEW",101,0,0,"no",true]}' \
	;sleep 0.5 \
	;echo '{"event" : "worker updated", "worker data":["w02","(6+2)","NEW NEW NEW",101,0,0,"no",false]}' \
	;sleep 0.5 \
	;echo '{"event" : "worker updated", "worker data":["w01","(7+2)","NEW!NEW!NEW!",102,0,0,"no",true]}' \
	;sleep 0.5 \
	;echo '{"event" : "round ended", "round number":1}' \
	;echo '{"event" : "worker updated", "worker data":["w01","(8+2)","NEW!NEW!NEW!",102,0,0,"no",false]}' \
	;echo '{"event" : "worker updated", "worker data":["w02","(9+2)","NEW NEW NEW",101,0,0,"no",false]}' \
	;sleep 3 \
	;echo '{"event" : "worker updated", "worker data":["w01","(0+2)","NEW!NEW!NEW!",102,0,0,{"idx":0},false]}' \
	;sleep 3 \
	;echo '{"event" : "problem chosen", "problem idx":1}' \
	;echo '{"event" : "problem state changed", "problem state":"new state"}' \
	) | ./$(MAIN)

test2: compile
	(echo '{"event" : "all data",' \
	;echo '           "running":false,' \
	;echo '           "workers":[["w01","no1", "stud","(1+2)+3","1+2+3",0,0,3,"no",     false]' \
	;echo '                     ,["w02","no2", "misc","(5+6)+7","5+6+7",4,0,8,"no",     false]' \
	;echo '                     ,["w03","idx0","misc","(5+6)+7","5+6+7",7,0,6,{"idx":0},false]' \
	;echo '                     ,["w04","no3", "misc","(5+6)+7","5+6+7",8,0,2,"no",     false]' \
	;echo '                     ,["w05","idx3","misc","(5+6)+7","5+6+7",6,0,5,{"idx":3},false]' \
	;echo '                     ,["w06","no4", "misc","(5+6)+7","5+6+7",9,0,4,"no",     false]' \
	;echo '                     ,["w07","idx1","misc","(5+6)+7","5+6+7",5,0,7,{"idx":1},false]' \
	;echo '                     ],' \
	;echo '           "problems":[["prob0","",5000,""],["prob1","",6000,""]],' \
	;echo '           "problem idx":0,' \
	;echo '           "round":1,' \
	;echo '           "worker input":["[1,2,3]","42"],' \
	;echo '           "state":"blub"}' \
	) | ./$(MAIN)
