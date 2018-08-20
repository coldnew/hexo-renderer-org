let child_process = require("child_process");
let fs = require("fs")
let EventEmitter = require('events');

const RUNNING = symbol("running");
const STOPED = symbol("stoped");

class Operater extends EventEmitter {
  constructor() {}
}

class Report {
  constructor(task, tryTimes, costTime, result) {
    this.task = task
    this.tryTimes = tryTimes;
    this.costTime = costTime;
    this.result = result;
  }

  get tryTimes() {
    return this.tryTimes;
  }

  get costTime() {
    return this.costTime;
  }

  get result() {
    return this.result;
  }
}

class Task  {
  constructor(operater, command, outfile) {
    this.operater = operater;
    this.command = command;
    this.outfile = outfile;
    this.tryTimes = 0;
    this.startTime = new Date();
    this.endTime = null;
  }

  get operater() {
    return this.operater;
  }

  get command() {
    return this.command;
  }

  get outfile() {
    return this.outfile;
  }

  get startTime() {
    return this.startTime;
  }

  get endTime  () {
    return this.endTime;
  }

  AddTimes() {
    this.tryTimes++;
  }

  Finish() {
    this.endTime = new Date();
  }

  Report(result) {
    return new Report(this, this.tryTimes, this.endTime - this.startTime, result);
  }
}


class BatchStation {

  constructor(funcCommand) {
    this.taskList = new Array();
    this.status = STOPED;
    this.funcCommand = funcCommand;
    this.engine = null;
  }

  // TODO:
  //      - [ ] add comments
  //      - [ ] add these codes to emacs_client
  OperatedBy(operater, command, outfile) {
    if (this.status === STOPED) {
      this.StartStation();
    }

    this.taskList.push(new Task(operater, command, outfile));
  }

  StartStation() {
    this.engine = () => {
      this.loop();
      setTimeout(this.engine, 10);
    }

    this.engine();
    this.status = RUNNING;
  }

  StopStation() {
    clearTimeout(this.engine);
    this.status = STOPED;
  }

  hexoRenderByEmacsClient(task) {
    return new Promise((resolve, reject) => {
      let proc = child_process.spawn(this.funcCommand, task.command);

      proc.on("exit", (code , signal) => {
        if (code !== 0) {
          reject(`Error: process return with code ${code}`);
        }

        resolve();
      })


      proc.stdout.on("data", data => {
        if (config.org.debug) console.log("Stdout: ", data);
      });
      proc.stderr.on("data", data => {
        if (config.org.debug) console.log("Stderr: ", data);
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
        resolve(task.Report(data));
      });
    })
  }

  emitRenderResultReport(report) {
    report.task.operater.emit("finish", report);
  }

  loop() {
    if (this.taskList.length <= 0) {
      return;
    }

    let task = this.taskList.shift();

    this.hexoRenderByEmacsClient(task)
      .then(filename => {
        loop();
        return readRenderResultFromFile(task);
      })
      .then(emitRenderResultReport)
      .catch( err => {
        if (config.org.debug) console.error(err);
        task.AddTimes();
        this.taskList.unshift(task);
        loop();
      })
  }
}
