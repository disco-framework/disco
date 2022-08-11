#!/usr/bin/python

import sys

from PyQt5 import QtCore, QtGui, QtWidgets
from ui_mainview import Ui_MainWindow

import json
from jsonreader import JsonReader


##################################################


class MainWindow(QtWidgets.QMainWindow):

   def __init__(self, parent=None):
      super(MainWindow, self).__init__(parent)
      self.ui = Ui_MainWindow()
      self.ui.setupUi(self)

      # status bar
      self.labelProblemSpec  = QtWidgets.QLabel()
      self.labelProblemTime  = QtWidgets.QLabel()
      self.labelCurrentRound = QtWidgets.QLabel()
      self.labelWorkerInput  = QtWidgets.QLabel()
      self.ui.statusbar.addWidget(self.labelProblemSpec,  1)
      self.ui.statusbar.addWidget(self.labelProblemTime,  1)
      self.ui.statusbar.addWidget(self.labelCurrentRound, 1)
      self.ui.statusbar.addWidget(self.labelWorkerInput,  1)

      # set menu shortcuts
      self.ui.actionLoadGameState.setShortcut(QtGui.QKeySequence(self.tr("Ctrl+O")))
      self.ui.actionSaveGameState.setShortcut(QtGui.QKeySequence(self.tr("Ctrl+S")))
      self.ui.actionQuit.setShortcut(QtGui.QKeySequence(self.tr("Ctrl+Q")))
      self.ui.actionStartRound.setShortcut(QtGui.QKeySequence(self.tr("Ctrl+R")))
      self.ui.actionAddScores.setShortcut(QtGui.QKeySequence(self.tr("Ctrl+A")))
      self.ui.actionKillAllWorkers.setShortcut(QtGui.QKeySequence(self.tr("Ctrl+K")))

      self.DataCollector = JsonReader(self)
      self.DataCollector.data_received.connect(self.received)
      self.DataCollector.worker_updated.connect(self.update_worker)
      self.DataCollector.round_started.connect(self.start_round)
      self.DataCollector.round_ended.connect(self.end_round)
      self.DataCollector.worker_input_changed.connect(self.update_worker_input)
      self.DataCollector.problem_chosen.connect(self.choose_problem)
      self.DataCollector.all_data.connect(self.update_all)
      self.DataCollector.save_game_state_reply.connect(self.save_game_state_reply)
      self.DataCollector.load_game_state_reply.connect(self.load_game_state_reply)
      self.DataCollector.start()

      self.problemAnswerTime = 0
      self.roundTimerRemaining = 0
      self.roundTimer = QtCore.QTimer()
      self.roundTimer.timeout.connect(self.roundTimer_tick)

      # file menu
      self.ui.actionLoadGameState.triggered.connect(self.btnLoadGameState_clicked)
      self.ui.actionSaveGameState.triggered.connect(self.btnSaveGameState_clicked)
      self.ui.actionReloadAllData.triggered.connect(self.btnReloadAllData_clicked)
      self.ui.actionQuit.triggered.connect(self.btnQuit_clicked)

      # round menu
      self.ui.actionStartRound.triggered.connect(self.btnStartRound_clicked)
      self.ui.actionAddScores.triggered.connect(self.btnAddScores_clicked)
      self.ui.actionKillAllWorkers.triggered.connect(self.btnKillAllWorkers_clicked)

      # worker tab
      self.ui.tableWorker.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
      self.ui.tableWorker.customContextMenuRequested.connect(self.tableWorker_requestContextMenu)

      # io tab
      self.ui.btnSend.clicked.connect(self.btnSend_clicked)
      self.ui.edtSend.returnPressed.connect(self.btnSend_clicked)

      # worker table header
      thh = self.ui.tableWorker.horizontalHeader()
      thh.setVisible(True)
      thh.resizeSection(0,  50)   # ranking group
      thh.resizeSection(1,  60)   # id
      thh.resizeSection(2, 170)   # name
      thh.resizeSection(3, 230)   # proposition
      thh.resizeSection(4, 100)   # points
      thh.resizeSection(5,  50)   # processed points
      thh.resizeSection(6, 100)   # problem points (accumulated over all rounds on this problem)
      thh.setSortIndicator(1, QtCore.Qt.AscendingOrder)

      tvh = self.ui.tableWorker.verticalHeader()
      tvh.setVisible(True)
      tvh.setSectionResizeMode(QtWidgets.QHeaderView.Fixed)

      self.reset_problem_list([])
      self.worker_blocked = {}


   def closeEvent(self, e):
      self.send(json.dumps({'action': 'quit program'}))
      self.DataCollector.terminate() # TODO: "This function is dangerous and its use is discouraged"
      self.DataCollector.wait()
      e.accept()
      app.exit()


   ###############################
   ##    main menu / buttons    ##
   ###############################

   ## file menu
   def btnLoadGameState_clicked(self):
      fileName = str(QtWidgets.QFileDialog.getOpenFileName())
      if fileName != "":
         self.send(json.dumps({'action': 'load game state', 'file path': fileName}))

   def btnSaveGameState_clicked(self):
      fileName = str(QtWidgets.QFileDialog.getSaveFileName())
      if fileName != "":
         self.send(json.dumps({'action': 'save game state', 'file path': fileName}))

   def btnReloadAllData_clicked(self):
      self.send(json.dumps({'action': 'get all data'}))

   def btnQuit_clicked(self):
      self.close()

   ## problems menu
   def btnChooseProblem_clicked(self, idx, action, oldChecked):
      action.setChecked(oldChecked) # undo auto check
      self.send(json.dumps({'action': 'choose problem', 'problem idx': idx}))

   ## round menu
   def btnStartRound_clicked(self):
      self.send(json.dumps({'action': 'start round'}))

   def btnAddScores_clicked(self):
      self.send(json.dumps({'action': 'add scores'}))
      self.ui.actionAddScores.setEnabled(False)

   def btnKillAllWorkers_clicked(self):
      self.send(json.dumps({'action': 'kill all workers'}))

   ## worker tab
   def tableWorker_requestContextMenu(self, position):
      if self.ui.tableWorker.currentRow() < 0:
         return
      workerId = str(self.ui.tableWorker.item(self.ui.tableWorker.currentRow(), 1).text())
      # create menu
      menu = QtWidgets.QMenu()
      actApply = menu.addAction("&Apply proposition")
      actBlock = None
      actUnblock = None
      if self.worker_blocked[workerId]:
         actUnblock = menu.addAction("Un&block worker '" + workerId + "'")
      else:
         actBlock = menu.addAction("&Block worker '" + workerId + "'")
      # execute menu synchronously
      action = menu.exec_(self.ui.tableWorker.viewport().mapToGlobal(position))
      if action != None:
         if action == actApply:
            if QtWidgets.QMessageBox.information(self, "Apply proposition", "Really apply proposition from " + workerId + "?",
                                                 QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
                                                 QtWidgets.QMessageBox.No) == QtWidgets.QMessageBox.Yes:
               self.send(json.dumps({'action': 'apply proposition', 'worker id': workerId}))
         elif action == actBlock:
            self.send(json.dumps({'action': 'block worker', 'worker id': workerId}))
         elif action == actUnblock:
            self.send(json.dumps({'action': 'unblock worker', 'worker id': workerId}))

   ## io tab
   def btnSend_clicked(self):
      msg = self.ui.edtSend.text()
      self.send(msg)
      self.ui.edtSend.clear()


   #######################
   ##    Round timer    ##
   #######################

   def roundTimer_tick(self):
      self.roundTimerRemaining -= self.roundTimer.interval()
      if self.roundTimerRemaining <= 0:
         self.roundTimer.stop()
         self.roundTimerRemaining = 0
      self.labelProblemTime.setText("Answer time remaining\n  " +
                                    str(self.roundTimerRemaining/1000) + "s")


   #######################
   ##    JSON events    ##
   #######################

   def update_worker(self, id, proposition, caption, score, processedScore, problemScore, blocked, working):
      row = self.get_worker_table_row(id)
      if proposition == None:
          proposition = ""
      if row != None:
         self.update_worker_by_row(row, id, proposition, caption, score, processedScore, problemScore, blocked, working)

   def start_round(self, round):
      self.ui.actionStartRound.setEnabled(False)
      self.ui.menuProblems.setEnabled(False)
      self.ui.actionAddScores.setEnabled(False)
      self.labelCurrentRound.setText("Round (running)\n  " + str(round))
      self.roundTimerRemaining = self.problemAnswerTime
      self.roundTimer.start(100)

   def end_round(self, round):
      self.ui.actionStartRound.setEnabled(True)
      self.ui.menuProblems.setEnabled(True)
      self.ui.actionAddScores.setEnabled(True)
      self.labelCurrentRound.setText("Round\n  " + str(round))
      self.roundTimerRemaining = 0
      self.roundTimer_tick()

   def update_worker_input(self, workerInput):
      def format_wi_line(line): return shorten_string(28, line)
      wiString = "\n".join(list(map(format_wi_line, workerInput)))
      self.labelWorkerInput.setText("Worker input for next round:\n" + wiString)

   def choose_problem(self, problemIdx):
      self.roundTimer.stop()
      self.reset_problem_list(self.problemList, problemIdx)
      probDesc, probSpec, answerTime, startState = self.problemList[problemIdx]
      self.labelProblemSpec.setText("Problem\n  " + probDesc)
      self.labelProblemTime.setText("Answer time\n  " + str(answerTime/1000.0) + "s")
      self.problemAnswerTime = answerTime
      self.labelCurrentRound.setText("")

   def update_all(self, running, workerList, problemList, problemIdx, round, workerInput, problemState):
      self.clear_worker_table()
      for id, name, group, proposition, caption, score, processedScore, problemScore, blocked, working in workerList:
         self.add_worker(id, name, group, proposition, caption, score, processedScore, problemScore, blocked, working)
      self.update_worker_input(workerInput)
      if running:
         self.start_round(round)
      else:
         self.end_round(round)
      self.problemList = problemList
      self.choose_problem(problemIdx)

   def save_game_state_reply(self, result):
      if result == "ok":
         msg = "Game state successfully saved."
         QtGui.QMessageBox.information(self, "Game state saved", msg, QtGui.QMessageBox.Ok)
      else:
         if   result == "enoent" : msg = "No such file or directory!"
         elif result == "enotdir": msg = "Not a directory!"
         elif result == "enospc" : msg = "No space left on device!"
         elif result == "eacces" : msg = "Permission denied!"
         elif result == "eisdir" : msg = "Illegal operation on a directory!"
         else                    : msg = "Unknown error: " + result
         QtGui.QMessageBox.warning(self, "Error saving game state", msg, QtGui.QMessageBox.Ok)

   def load_game_state_reply(self, result):
      if result == "ok":
         msg = "Game state successfully loaded."
         QtGui.QMessageBox.information(self, "Game state loaded", msg, QtGui.QMessageBox.Ok)
      else:
         if   result == "eformat": msg = "Invalid file format!"
         elif result == "enoent" : msg = "No such file or directory!"
         elif result == "enotdir": msg = "Not a directory!"
         elif result == "eacces" : msg = "Permission denied!"
         elif result == "eisdir" : msg = "Illegal operation on a directory!"
         else                    : msg = "Unknown error: " + result
         QtGui.QMessageBox.warning(self, "Error loading game state", msg, QtGui.QMessageBox.Ok)


   #############################
   ##    private functions    ##
   #############################

   def send(self, msg):
      self.ui.txtRecv.appendHtml("<span style='font-weight:bold;color:red'>send:</span> "
                                 + escape_html(msg).rstrip("\n").replace("\n","<br />"))
      print(msg)
      sys.stdout.flush()

   def received(self, msg):
      self.ui.txtRecv.appendHtml("<span style='font-weight:bold;color:blue'>recv:</span> "
                                 + escape_html(msg).rstrip("\n").replace("\n","<br />"))

   def get_worker_table_row(self, id):
      for row in range(0, self.ui.tableWorker.rowCount()):
         if self.ui.tableWorker.item(row, 1).text() == id:
            return row
      return None

   def clear_worker_table(self):
      self.worker_blocked = {}
      self.ui.tableWorker.clearContents()
      self.ui.tableWorker.setRowCount(0)

   def add_worker(self, id, name, group, proposition, propCaption, score, processedScore, problemScore, blocked, working):
      if proposition == None:
          proposition = ""

      self.worker_blocked[id] = blocked != "no"

      row = self.ui.tableWorker.rowCount()
      self.ui.tableWorker.setRowCount(row + 1)

      self.ui.tableWorker.setSortingEnabled(False)

      item = QtWidgets.QTableWidgetItem()
      item.setText(group)
      self.ui.tableWorker.setItem(row, 0, item)

      item = QtWidgets.QTableWidgetItem()
      item.setText(id)
      self.ui.tableWorker.setItem(row, 1, item)

      item = QtWidgets.QTableWidgetItem()
      item.setText(name)
      self.ui.tableWorker.setItem(row, 2, item)

      item = QtWidgets.QTableWidgetItem()
      self.ui.tableWorker.setItem(row, 3, item)

      item = CustomTableWidgetItem()
      item.setTextAlignment(int(QtCore.Qt.AlignRight|QtCore.Qt.AlignVCenter))
      self.ui.tableWorker.setItem(row, 4, item)

      item = CustomTableWidgetItem()
      item.setTextAlignment(int(QtCore.Qt.AlignRight|QtCore.Qt.AlignVCenter))
      self.ui.tableWorker.setItem(row, 5, item)

      item = CustomTableWidgetItem()
      item.setTextAlignment(int(QtCore.Qt.AlignRight|QtCore.Qt.AlignVCenter))
      self.ui.tableWorker.setItem(row, 6, item)

      self.update_worker_by_row(row, id, proposition, propCaption, score, processedScore, problemScore, blocked, working)

      self.ui.tableWorker.setSortingEnabled(True)

   def update_worker_by_row(self, row, id, proposition, propCaption, score, processedScore, problemScore, blocked, working):
      isBlocked = blocked != "no"
      blockedIdx = blocked["idx"] if "idx" in blocked else 0

      self.worker_blocked[id] = isBlocked

      self.ui.tableWorker.setSortingEnabled(False)

      brush = QtGui.QBrush(QtGui.QColor(190, 190, 190))
      if self.worker_blocked[id]:
         brush.setStyle(QtCore.Qt.SolidPattern)
      else:
         brush.setStyle(QtCore.Qt.NoBrush)

      self.ui.tableWorker.item(row, 0).setBackground(brush)

      self.ui.tableWorker.item(row, 1).setBackground(brush)

      self.ui.tableWorker.item(row, 2).setBackground(brush)

      item = self.ui.tableWorker.item(row, 3)
      item.setText(propCaption)
      item.setBackground(brush)

      item = self.ui.tableWorker.item(row, 4)
      item.setText(str(score))
      item.setCustomSortData(isBlocked, {False: int(score), True: blockedIdx}[isBlocked])
      item.setBackground(brush)

      item = self.ui.tableWorker.item(row, 5)
      item.setText(str(processedScore))
      item.setCustomSortData(isBlocked, {False: int(processedScore), True: blockedIdx}[isBlocked])
      item.setBackground(brush)

      item = self.ui.tableWorker.item(row, 6)
      item.setText(str(problemScore))
      item.setCustomSortData(isBlocked, {False: int(problemScore), True: blockedIdx}[isBlocked])
      item.setBackground(brush)

      if self.ui.tableWorker.cellWidget(row, 2) == None:
         if working:
            self.ui.tableWorker.setCellWidget(row, 2, WorkingWidget(self))
      else:
         if not working:
            self.ui.tableWorker.removeCellWidget(row, 2)

      self.ui.tableWorker.setSortingEnabled(True)

   def reset_problem_list(self, lst, checkedIdx=None):
      self.problemList = lst
      self.ui.menuProblems.clear()
      if lst == []:
         action = QtWidgets.QAction(self)
         action.setText("--- no problems ---")
         action.setEnabled(False)
         self.ui.menuProblems.addAction(action)
      else:
         for idx, (description, spec, answerTime, state) in enumerate(lst):
            action = QtWidgets.QAction(self)
            action.setText(description + "\t" + str(answerTime/1000.0) + "s")
            action.setCheckable(True)
            if checkedIdx == idx:
               action.setChecked(True)
            action.triggered.connect(lambda i=idx, a=action, chk=(checkedIdx==idx):
                                     self.btnChooseProblem_clicked(i, a, chk))
            self.ui.menuProblems.addAction(action)


##################################################


class WorkingWidget(QtWidgets.QLabel):

    def __init__(self, parent=None):
        super(WorkingWidget, self).__init__(parent)
        self.setAlignment(QtCore.Qt.AlignRight|QtCore.Qt.AlignVCenter)
        movie = QtGui.QMovie("./gears.gif")
        self.setMovie(movie)
        movie.start()


##################################################


class CustomTableWidgetItem(QtWidgets.QTableWidgetItem):

   def __init__(self):
      # call custom constructor with item type 'UserType'
      super(CustomTableWidgetItem, self).__init__(QtWidgets.QTableWidgetItem.UserType)
      self.blocked = False
      self.sortKey = 0

   def setCustomSortData(self, blocked, sortKey):
      self.blocked = blocked
      self.sortKey = sortKey

   # override the 'less than' operator
   def __lt__(self, other):
      if self.blocked == other.blocked:
         return self.sortKey > other.sortKey
      else:
         return self.blocked < other.blocked


##################################################


def shorten_string(chars, string):
    return (string[:(chars-3)] + '...') if len(string) > chars else string

def escape_html(str):
   return str.replace("&","&amp;").replace(">","&gt;").replace("<","&lt;")


##################################################


if __name__ == "__main__":
   app = QtWidgets.QApplication(sys.argv)
   win = MainWindow()
   win.show()
   sys.exit(app.exec_())
