let child_process = require("child_process");
let fs = require("fs")
let EventEmitter = require('events');

const RUNNING = Symbol("running");
const BUSY = Symbol("BUSY");
const STOPED = Symbol("stoped");

class Operator extends EventEmitter {
  constructor(id) {
    super();
    this.id = id;
  }
}

class Report {
  constructor(task, tryTimes, costTime, result) {
    this.task = task
    this.tryTimes = tryTimes;
    this.costTime = costTime;
    this.result = result;
  }

  Show() {
    console.log('Report: ', this.task.operator.id, this.tryTimes, this.costTime);
  }
}

class Task  {
  constructor(operator, command, outfile) {
    this.operator = operator;
    this.command = command;
    this.outfile = outfile;
    this.tryTimes = 0;
    this.startTime = new Date();
    this.endTime = null;
  }

  Show() {
    console.log(this.operator.id, this.command, this.outfile);
  }

  AddTimes() {
    return this.tryTimes++;
  }

  Finish() {
    this.endTime = new Date();
  }

  Report(result) {
    return new Report(this, this.tryTimes, this.endTime - this.startTime, result);
  }
}


class BatchStation {

  constructor(funcCommand, option) {
    this.taskList = new Array();
    this.status = STOPED;
    this.funcCommand = funcCommand;
    this.engine = null;
    this.debug = option.debug || false;
    this.maxErrTimes = option.maxErrTimes || 20;

    // private
    this.lastFinishTime = new Date();
  }

  // TODO:
  //      - [ ] add comments
  //      - [ ] add these codes to emacs_client
  OperatedBy(operator, command, outfile) {
    if (this.status === STOPED) {
      this.StartStation();
    }

    this.taskList.push(new Task(operator, command, outfile));
  }

  StartStation() {
    this.status = RUNNING;
    this.engine = () => {
      if(this.status === STOPED) return;

      this.loop();
      setTimeout(this.engine, 10);
    }

    this.engine();
    console.log('BatchStation started.')
  }

  StopStation() {
    clearTimeout(this.engine);
    this.status = STOPED;
    console.log('BatchStation stoped.')
  }

  hexoRenderByEmacsClient(task) {
    return new Promise((resolve, reject) => {
      this.status = BUSY;
      task.Show();
      let proc = child_process.spawn(this.funcCommand, task.command);

      proc.on("exit", (code , signal) => {
        if (code !== 0) {
          reject(`Error: process return with code ${code}`);
        }

        resolve();
      })


      proc.stdout.on("data", data => {
        if (this.debug) console.log("Stdout: ", data.toString());
      });
      proc.stderr.on("data", data => {
        if (this.debug) console.log("Stderr: ", data.toString());
      });

      proc.on("error", (err) => {
        reject(err);
      });
    });
  }

  readRenderResultFromFile(task) {
    return new Promise((resolve, reject) => {
      fs.readFile(task.outfile, (err, data) => {
        if (err) reject(err);
        task.Finish();
        resolve(task.Report(data));
      });
    })
  }

  emitRenderResultReport(report) {
    report.task.operator.emit("finish", report);
  }

  emitRenderErrorTask(task, err) {
    task.operator.emit("Error: ", err)
  }

  loop() {
    if (this.status === BUSY) {
      return;
    }
    if (this.taskList.length <= 0) {
      // close batchStation if idle time is longer than 2 ms
      let now = new Date();
      if (now - this.lastFinishTime > 2000) {
        this.StopStation();
      }
      return;
    }

    let task = this.taskList.shift();

    this.hexoRenderByEmacsClient(task)
      .then(filename => {
        this.status = RUNNING;
        this.lastFinishTime = new Date();
        this.loop();
        return this.readRenderResultFromFile(task);
      })
      .then(this.emitRenderResultReport)
      .catch( err => {
        if (this.debug) console.error(err);
        this.status = RUNNING;
        // redo task when trying times is less than maxErrTimes
        if (task.AddTimes() <= this.maxErrTimes) {
          this.taskList.unshift(task);
          this.loop();
          return;
        }

        this.emitRenderErrorTask(task, err)
      })
  }
}

module.exports = {
  RUNNING,
  STOPED,
  Operator,
  Report,
  Task,
  BatchStation
};
