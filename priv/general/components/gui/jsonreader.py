#!/usr/bin/python

import sys
import json
import re

from PyQt4 import QtCore


class JsonReader(QtCore.QThread):

   def __init__(self, parent=None):
      QtCore.QThread.__init__(self, parent)
      self.prevLines = ""

   def run(self):
      while True:
         currLine = sys.stdin.readline()
         if not currLine:
            break
         try:
            msg = json.loads(self.prevLines + currLine)

            event = msg['event']

            def handle_worker_data():
                d = msg['worker data']
                self.emit(QtCore.SIGNAL("worker_updated"), d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7])

            def handle_all_data():
                workerList = []
                for w in msg['workers']:
                    workerList.append((w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8], w[9]))

                problemList = []
                for p in msg['problems']:
                    problemList.append((p[0], p[1], p[2], p[3]))
                self.emit(QtCore.SIGNAL("all_data"), msg['running'], workerList, problemList,
                    msg['problem idx'], msg['round'], msg['worker input'], msg['state'])

            handlers = {
               'worker updated'        : handle_worker_data,
               'round started'         : (lambda: self.emit(QtCore.SIGNAL("round_started"),         msg['round number'])),
               'round ended'           : (lambda: self.emit(QtCore.SIGNAL("round_ended"),           msg['round number'])),
               'worker input changed'  : (lambda: self.emit(QtCore.SIGNAL("worker_input_changed"),  msg['worker input'])),
               'problem chosen'        : (lambda: self.emit(QtCore.SIGNAL("problem_chosen"),        msg['problem idx'])),
               'problem state changed' : (lambda: self.emit(QtCore.SIGNAL("problem_state_changed"), msg['problem state'])),
               'all data'              : handle_all_data,
               'save game state'       : (lambda: self.emit(QtCore.SIGNAL("save_game_state_reply"), msg['result'])),
               'load game state'       : (lambda: self.emit(QtCore.SIGNAL("load_game_state_reply"), msg['result']))
            }

            handlers[event]()

         except KeyError as e:
            sys.stderr.write("unknown event in message: " + self.prevLines + currLine + "\n")
            self.prevLines = ""

         except ValueError as e:
            # catch ValueError to check for incomplete JSON
            bufferLen = len(self.prevLines + currLine)-1
            p = re.compile("Expecting object: line [0-9]* column [0-9]*.*(char " + str(bufferLen) + ")")
            if p.match(str(e)):
               # incomplete JSON received: JSON message contains new line
               self.prevLines += currLine
            else:
               sys.stderr.write("--- last message ---\n" +
                                self.prevLines + currLine +
                                "--------------------\n")
               # re-raise the exception if the ValueError is not at the end of the message
               raise e

         else:
            self.emit(QtCore.SIGNAL("received_data"), self.prevLines + currLine)
            # reset line buffer if successfully parsed
            self.prevLines = ""
