#!/usr/bin/python

import sys

from PyQt5 import QtCore, QtGui, QtWidgets
from ui_squaresview import Ui_MainWindow

import json
from jsonreader import JsonReader

import re
import random


##################################################


class MainWindow(QtWidgets.QMainWindow):

   def __init__(self, parent=None):
      QtWidgets.QMainWindow.__init__(self, parent)
      self.ui = Ui_MainWindow()
      self.ui.setupUi(self)

      self.nextWorkerInput = ["[]", "1"]
      self.bestPropScore = -1
      self.clear_worker_table()

      self.DataCollector = JsonReader(self)
      self.DataCollector.data_received.connect(self.received)
      self.DataCollector.worker_updated.connect(self.update_worker)
      self.DataCollector.round_started.connect(self.start_round)
      self.DataCollector.worker_input_changed.connect(self.update_worker_input)
      self.DataCollector.problem_state_changed.connect(self.update_problem_state)
      self.DataCollector.all_data.connect(self.update_all)
      self.DataCollector.start()

      # proposition tab
      self.ui.cbxId.currentIndexChanged.connect(self.cbxId_indexChanged)

      self.paintBox = PaintBox(self.ui.tabProposition)
      sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Expanding)
      sizePolicy.setHorizontalStretch(0)
      sizePolicy.setVerticalStretch(0)
      sizePolicy.setHeightForWidth(self.paintBox.sizePolicy().hasHeightForWidth())
      self.paintBox.setSizePolicy(sizePolicy)
      self.ui.gridLayout_4.addWidget(self.paintBox, 1, 0, 1, 1)

      # io tab
      self.ui.btnSend.clicked.connect(self.btnSend_clicked)
      self.ui.edtSend.returnPressed.connect(self.btnSend_clicked)

   def closeEvent(self, e):
      self.send(json.dumps({'action': 'quit program'}))
      self.DataCollector.terminate() # TODO: "This function is dangerous and its use is discouraged"
      self.DataCollector.wait()
      e.accept()
      app.exit()


   ###############################
   ##    main menu / buttons    ##
   ###############################

   ## io tab
   def btnSend_clicked(self):
      msg = self.ui.edtSend.text()
      self.send(msg)
      self.ui.edtSend.clear()

   def cbxId_indexChanged(self, idx):
      ## DEBUG
      #sys.stderr.write(">> cbxId_indexChanged %d\n" % idx)
      #sys.stderr.flush()
      self.display_proposition_by_cbxId_index(idx)


   #######################
   ##    JSON events    ##
   #######################

   def update_worker(self, id, proposition, propCaption, propScore, processedScore, problemScore, blocked, working):
      ## DEBUG
      #sys.stderr.write("[json] update_worker:\n")
      #sys.stderr.write("       id         : %s\n" % id)
      #sys.stderr.write("       proposition: %s\n" % proposition)
      #sys.stderr.write("       caption    : %s\n" % propCaption)
      #sys.stderr.write("       score      : %s\n" % propScore)
      #sys.stderr.flush()
      if id in self.worker:
         (workerName, bestProposition, bestPropCaption, bestPropScore) = self.worker[id]
         self.worker[id] = (workerName, proposition, propCaption, propScore)
         if (blocked == "no"):
            ## valid proposition: check if better
            if propScore > self.bestPropScore:
               self.bestPropScore = propScore
               ## auto switch to best proposition
               newCbxIndex = self.ui.cbxId.findText(id)
               if newCbxIndex == self.ui.cbxId.currentIndex():
                  self.display_proposition_by_cbxId_index(newCbxIndex)
               else:
                  self.ui.cbxId.setCurrentIndex(newCbxIndex)
         else:
            if propScore < 0:
               ## invalid proposition: find and display the next better proposition
               if 1 == 1 or self.ui.cbxId.text() == id:
                  nextBestId = id
                  nextBestScore = -1
                  for i in self.worker.keys():
                     (n, p, c, s) = self.worker[i]
                     if s > nextBestScore:
                        nextBestId = i
                        nextBestScore = s
                  self.bestPropScore = nextBestScore
                  ## switch to next better proposition
                  newCbxIndex = self.ui.cbxId.findText(nextBestId)
                  if newCbxIndex == self.ui.cbxId.currentIndex():
                     self.display_proposition_by_cbxId_index(newCbxIndex)
                  else:
                     self.ui.cbxId.setCurrentIndex(newCbxIndex)

   def start_round(self, round):
      ## DEBUG
      #sys.stderr.write("[json] start_round:\n")
      #sys.stderr.write("       round: %s\n" % round)
      #sys.stderr.flush()
      ## activate proposition tab
      self.ui.tabWidget.setCurrentIndex(2)
      self.paintBox.setSquareSize(int(self.nextWorkerInput[1]))
      self.ui.lblInput.setText("<html><body>"
                               + "<p><b>Square: </b>" + self.nextWorkerInput[1] + "</p>"
                               + "<p><b>Squares </b>(" + str(self.nextWorkerInput[0].count(",")) + ")<b>: </b>" + self.nextWorkerInput[0].replace(",", ", ") + "</p></body></html>")
      self.bestPropScore = -1
      self.ui.lblWorker.setText("")
      self.ui.lblScore.setText("")
      for workerId in self.worker.keys():
         (name, proposition, propCaption, propScore) = self.worker[workerId]
         self.worker[workerId] = (name, "[]", "", 0)
      self.paintBox.drawProposition("[]")

   def update_worker_input(self, workerInput):
      ## DEBUG
      #sys.stderr.write("[json] update_worker_input:\n")
      #sys.stderr.write("       workerInput: %s\n" % workerInput)
      #sys.stderr.flush()
      self.nextWorkerInput = workerInput

   def update_problem_state(self, problemState):
      self.ui.lblState.setText(problemState.replace(",", ", "))

   def update_all(self, running, workerList, problemList, problemIdx, round, workerInput, problemState):
      ## DEBUG
      #sys.stderr.write("[json] update_all:\n")
      #sys.stderr.write("       running   : %s\n" % running)
      #sys.stderr.write("       workerList: %s\n" % workerList)
      #sys.stderr.write("       ...\n")
      #sys.stderr.flush()
      self.clear_worker_table()
      for id, name, group, proposition, caption, score, processedScore, problemScore, blocked, working in workerList:
         self.add_worker(id, name, group, proposition, caption, score, processedScore, problemScore, blocked, working)
      self.update_worker_input(workerInput)
      self.update_problem_state(problemState)
      if running:
         self.start_round(round)


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

   def clear_worker_table(self):
      self.worker = {}
      self.ui.cbxId.clear()

   def add_worker(self, id, name, group, proposition, propCaption, propScore, processedScore, problemScore, blocked, working):
      self.worker[id] = (name, proposition, propCaption, propScore)
      self.ui.cbxId.clear()
      self.ui.cbxId.addItems(sorted(self.worker.keys()))

   def display_proposition_by_cbxId_index(self, idx):
      ## DEBUG
      #sys.stderr.write(">> display_proposition_by_cbxId_index %d\n" % idx)
      #sys.stderr.flush()
      if idx > -1:
         workerId = str(self.ui.cbxId.itemText(idx))
         if workerId in self.worker:
            ## DEBUG
            #sys.stderr.write(">>                    %s\n" % workerId)
            #sys.stderr.flush()
            (workerName, lastProposition, lastPropCaption, lastPropScore) = self.worker[workerId]
            self.ui.lblWorker.setText("<html><body><p style='font-size:12pt'><b>" + workerName + "</b></p></body></html>")
            self.ui.lblScore.setText("<html><body><p style='font-size:12pt'><b>Score: </b>" + str(lastPropScore) + "</p></body></html>")
            self.paintBox.drawProposition(lastProposition)


##################################################


class PaintBox(QtWidgets.QWidget):

   MARGIN = 5
   BG_COLOR = QtGui.qRgb(242, 242, 242)

   def __init__(self, parent=None):
      QtWidgets.QWidget.__init__(self, parent)
      ## Set StaticContents to enable minimal repaints on resizes.
      self.setAttribute(QtCore.Qt.WA_StaticContents)
      self.squareSize = None
      self.setSquareSize(1)
      self.image = QtGui.QImage()
      self.resizeImage(self.size())

   def setSquareSize(self, len):
      ## DEBUG
      #print(">> setSquareSize  ", len)
      #print("  - self.image.size() = (" + str(self.image.size().width()) + ", " + str(self.image.size().height()) + ")")
      #print("  - self.size()       = (" + str(self.size().width()) + ", " + str(self.size().height()) + ")")
      self.squareSizeOld = self.squareSize
      self.squareSize = len
      self.lastProposition = "[]"
      self.lastWorkerId = ""
      self.lastPropScore = 0

   def resizeImage(self, newSize):
      ## DEBUG
      #print(">> resizeImage  (", newSize.width(), ",", newSize.height(), ")")
      #print("  - newsize           = (" + str(newSize.width()) + ", " + str(newSize.height()) + ")")
      #print("  - self.squareSize   = " + str(self.squareSize))
      #print("  - self.squareScale  = " + str(self.squareScale))
      #print("  - self.squareOffset = (" + str(self.squareOffset.x()) + ", " + str(self.squareOffset.y()) + ")")
      if self.image.size() == newSize:
         return
      self.calcOffset(newSize)
      ## this resizes the canvas without resampling the image
      newImage = QtGui.QImage(newSize, QtGui.QImage.Format_RGB32)
      newImage.fill(self.BG_COLOR)
      self.image = newImage
      self.drawProposition(self.lastProposition)

   def calcOffset(self, widgetSize):
      minLen = min(widgetSize.width(), widgetSize.height())
      self.squareScale = (minLen - 2 * self.MARGIN) / self.squareSize
      if self.squareSize < 400:
         self.squareScale = int(self.squareScale)
      squareLen = self.squareSize * self.squareScale
      self.squareOffset = QtCore.QPoint((widgetSize.width()  - squareLen) / 2,
                                        (widgetSize.height() - squareLen) / 2)
      
   def drawProposition(self, proposition):
      ## DEBUG
      #print(">>   drawProposition", proposition)
      if self.squareSizeOld != self.squareSize:
         self.calcOffset(self.size())

      self.lastProposition = proposition
      self.image.fill(self.BG_COLOR)
      painter = QtGui.QPainter(self.image)

      ## draw bounding square
      painter.setPen(QtGui.QPen(QtCore.Qt.black))
      painter.setBrush(QtGui.QColor(255, 255, 255, 255))
      a = self.squareSize * self.squareScale
      painter.drawRect(self.squareOffset.x(), self.squareOffset.y(), a, a)

      # draw square size
      font = QtGui.QFont()
      font.setPointSizeF(12)
      metrics = QtGui.QFontMetricsF(font)
      painter.setFont(font)
      text = "size: " + str(self.squareSize)
      rect = metrics.boundingRect(text)
      painter.drawText(self.image.width() - rect.width(), self.image.height(), text)

      ## draw proposition squares
      if proposition != None:
         color = QtGui.QColor()
         random.seed(4)
         for x,y,a in re.findall('\( *(\d+) *, *(\d+) *, *(\d+) *\)', proposition):
            text = str(a)
            x = int(x) * self.squareScale + self.squareOffset.x()
            y = int(y) * self.squareScale + self.squareOffset.y()
            a = int(a) * self.squareScale
            color.setHsv(random.randrange(  0,360,20),   # hue
                         random.randrange(190,250,30),   # saturation
                         random.randrange(160,220,30),   # value
                         190)                            # alpha
            painter.setBrush(color)
            painter.setPen(QtGui.QPen(QtCore.Qt.black))
            painter.drawRect(x, y, a, a)
            fontRect = metrics.boundingRect(text)
            painter.setPen(QtGui.QPen(QtCore.Qt.white))
            painter.drawText(x - (fontRect.width()  - a) / 2 - 1,
                             y + (fontRect.height() + a) / 2 - 3,
                             text)

      self.update()

   def paintEvent(self, event):
      painter = QtGui.QPainter(self)
      painter.drawImage(event.rect(), self.image)

   def resizeEvent(self, event):
      self.resizeImage(event.size())
      QtWidgets.QWidget.resizeEvent(self, event)


##################################################


def escape_html(str):
   return str.replace("&","&amp;").replace(">","&gt;").replace("<","&lt;")


##################################################


if __name__ == "__main__":
   app = QtWidgets.QApplication(sys.argv)
   win = MainWindow()
   win.show()
   sys.exit(app.exec_())
