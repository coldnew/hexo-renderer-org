let child_process = require('child_process');
let slash = require('slash');
let fs = require('fs-extra');
let tmp = require('tmp');
let path = require('path');
let retry = require('retry');
let {Operator, STOPED, BatchStation} = require('./batchStation');

// A variable to save current config.org.emacsclient info
let emacsclient = "emacsclient";

const EMACS_SERVER_OFF = 0;
const EMACS_SERVER_NOT_LOAD_HEXO = 1;
const EMACS_SERVER_ON = 2;

let emacs_server_status = EMACS_SERVER_OFF;

// Emacs daemon server-file (full path)
let server_file = "~/.emacs.d/server/server";

let batch_station = null;

function fix_filepath(s) {
  if (s.length <= 0) return s;

  if (s[0] === '~') {
    s = path.join(process.env.HOME, s.slice(1));
  }

  // unix way of filepath
  s = slash(s);
  // convert " -> \"
  s = s.replace(/[\""]/g, '\\"');
  return s;
}

function fix_elisp(s) {
  // remove lisp's comments
  s = s.replace(/^[\s\t]*;.*$/gm, "");

  // remove trailing garbage to prevent emacs eval fail
  s = s.replace(/\r?\n|\r/g, "");

  return s;
}

// convert true, false => t, nil
function elisp_bool(b) {
  return (b == true) ? "t" : "nil";
}

function format_hexo_configs(hexo_config, user_config) {
  // Find emacs entry point hexo-renderer-org.el
  let init_el = path.join(process.cwd(), "emacs", "hexo-renderer-org.el");
  if (!fs.existsSync(init_el))
    init_el = fix_filepath(path.join(process.cwd(), "node_modules", "hexo-renderer-org", "emacs", "hexo-renderer-org.el"));

  let debug_file = tmp.fileSync();
  let emacs_lisp = `
  (progn
    ;; Setup user's config
    (setq hexo-renderer-org-cachedir     "${fix_filepath(hexo_config.org.cachedir) || ""}")
    (setq hexo-renderer-org-user-config  "${fix_filepath(user_config) || ""}")
    (setq hexo-renderer-org-theme        "${hexo_config.org.theme || ""}")
    (setq hexo-renderer-org-common-block "${hexo_config.org.common.replace(/\n/g, "\\n")}")
    (setq hexo-renderer-org--debug-file  "${fix_filepath(debug_file.name)}")
    (setq hexo-renderer-org--use-htmlize  ${elisp_bool((hexo_config.org.htmlize))})
    (setq org-hexo-use-htmlize            ${elisp_bool((hexo_config.org.htmlize))})
    (setq org-hexo-use-line-number        ${elisp_bool(hexo_config.org.line_number)})
    ;; load init.el
    (load "${init_el}"))
  `;

  return emacs_lisp;
}

function get_emacs_server_socket_file(hexo) {
  return fix_filepath(hexo.config.org.server_file || server_file);
}

function emacs_server_check(hexo) {
  // it has been checked
  if (emacs_server_status !== EMACS_SERVER_OFF)
    return Promise.resolve(hexo);

  let socket_file = get_emacs_server_socket_file(hexo);
  return new Promise((resolve, reject) => {
    fs.stat(socket_file, (err, stats) => {
      // file does not exist
      if (err) return reject(err);
      // file type is not socket
      if (!stats.isSocket()) return reject(`File Type Error: type of ${server_file} is not socket.`);

      // socket file exist, change emacs_server_status
      emacs_server_status = EMACS_SERVER_NOT_LOAD_HEXO;
      resolve(hexo)
    })
  })
}

function emacs_server_load_config(hexo) {
  // it has been loaded
  if (emacs_server_status !== EMACS_SERVER_NOT_LOAD_HEXO)
    return Promise.resolve(hexo);

  let config = hexo.config;
  let emacs_lisp = format_hexo_configs(config, "");

  if (config.org.debug) {
    console.log("\n--------------load hexo config----------------");
    console.log("emacs: ", config.org.emacsclient);
    console.log("emacs_lisp: \n", emacs_lisp);
    console.log("\n--------------load hexo config----------------");
  }

  // Remove triling garbages
  emacs_lisp = fix_elisp(emacs_lisp);

  let socket_file = get_emacs_server_socket_file(hexo);
  let exec_args = ['-s', socket_file, '-e', emacs_lisp];

  return new Promise((resolve, reject) => {
    // load hexo-renderer-org.el via emacsclient
    let proc = child_process.spawn(config.org.emacsclient, exec_args, {
      stdio: 'inherit'
    });

    proc.on('exit', (code, signal) => {
      if (code !== 0) reject(`Emacs Load Config Error: emacsclient exit with ${code}`);

      emacs_server_status = EMACS_SERVER_ON;
      resolve(hexo);
    });

    proc.on('error', (err) => {
      reject(`Emacs Load Config Error: ${err}`);
    });
  })
}

function emacs_client(hexo, data) {
  console.log("emacs client")
  let config = hexo.config;

  if(batch_station === null || batch_station.status === STOPED) {
    // start batch station if it null
    batch_station = new BatchStation(config.org.emacsclient, config.org.debug);
    batch_station.StartStation();
  }

  config.highlight = config.highlight || {};
  let emacs_path = config.org.emacs;
  let output_file = tmp.fileSync();
  let emacs_lisp = `
(progn
  ;; render file according to args
  (hexo-renderer-org '(:file         "${fix_filepath(data.path)}"
                       :output-file  "${fix_filepath(output_file.name)}"
                       )))
`;

  // Enable this for debugging
  if (config.org.debug) {
    console.log("\n--------------hexo render----------------");
    console.log("emacsclient: ", config.org.emacsclient);
    console.log("emacs_lisp: \n", emacs_lisp);
    console.log("\n--------------hexo render----------------");
  }

  // Remove triling garbages
  emacs_lisp = fix_elisp(emacs_lisp);
  let socket_file = get_emacs_server_socket_file(hexo);

  let exec_args = ['-s', socket_file, '-e', emacs_lisp];

  let hexo_operator = new Operator(path.basename(data.path));
  batch_station.OperatedBy(hexo_operator, exec_args, output_file.name);

  // if (config.org.export_cfg != '')
  //    exec_args.splice(1,0,'--execute', config.org.export_cfg);

  return new Promise((resolve, reject) => {
    hexo_operator.on('finish', report => {
      report.Show();
      resolve(report.result);
    })
  })
}


// For emacs process method:
// 1. use user's ~/.emacs.d 's org-mode files
// 2. use emacs process to renderer file
// 3. does not support htmlize syntax higlight
// 4. maybe more slow
// 5. just for some system can't use emacs daemon method
function emacs_process(hexo, data) {
  console.log("emacs process")
  let config = hexo.config;
  config.highlight = config.highlight || {};

  let emacs_path = config.org.emacs;

  // Find emacs entry point hexo-renderer-org.el
  let init_el = path.join(process.cwd(), "emacs", "hexo-renderer-org.el");
  if (!fs.existsSync(init_el))
    init_el = path.join(process.cwd(), "node_modules", "hexo-renderer-org", "emacs", "hexo-renderer-org.el");

  init_el = fix_filepath(init_el);

  let debug_file = tmp.fileSync();
  let output_file = tmp.fileSync();

  // convert user_config to absolute path
  let user_config = "";
  if (config.org.user_config)
    user_config = path.join(process.cwd(), path.normalize(config.org.user_config));

  let emacs_lisp = `
(progn
  ;; Setup user's config
  (setq hexo-renderer-org-daemonize nil)
  (setq hexo-renderer-org-cachedir     "${fix_filepath(config.org.cachedir) || ""}")
  (setq hexo-renderer-org-user-config  "")
  (setq hexo-renderer-org-theme        "")
  (setq hexo-renderer-org-common-block "${config.org.common.replace(/\n/g, "\\n")}")
  (setq hexo-renderer-org--debug-file  "${fix_filepath(debug_file.name)}")
  (setq hexo-renderer-org--use-htmlize  nil)
  (setq org-hexo-use-htmlize            nil)
  (setq org-hexo-use-line-number        ${elisp_bool(config.org.line_number)})
  ;; load init.el
  (load "${init_el}")
  ;; render file according to args
  (hexo-renderer-org '(:file         "${fix_filepath(data.path)}"
                       :output-file  "${fix_filepath(output_file.name)}"))
  ;; kill emacs
  (kill-emacs))
`;

  // Enable this for debugging
  if (config.org.debug) {
    console.log("\n------------------------------");
    console.log("emacs: ", config.org.emacs);
    console.log("type: emacs process");
    console.log("emacs_lisp: \n", emacs_lisp);
    console.log("\n------------------------------");
  }

  // Remove triling garbages
  emacs_lisp = fix_elisp(emacs_lisp);

  let exec_args = ['--batch', '--eval', emacs_lisp];

  // if (config.org.export_cfg != '')
  //    exec_args.splice(1,0,'--execute', config.org.export_cfg);

  return new Promise((resolve, reject) => {
    let proc = child_process.spawn(config.org.emacs, exec_args);

    proc.on('exit', (code, signal) => {
      if (config.org.debug)
        console.log("DONE: ", data.path);

      let result = fs.readFileSync(output_file.name, 'utf8');
      resolve(result); // return callback
    });
  })

}

module.exports = {

  server: {
    check: function(hexo) {
      return emacs_server_check(hexo);
    },
    load_config: function(hexo) {
      return emacs_server_load_config(hexo);
    }
  },

  client: function(hexo, data, callback) {
    return emacs_client(hexo, data, callback);
  },

  process: function(hexo, data, callback) {
    return emacs_process(hexo, data, callback);
  }
};
