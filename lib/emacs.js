'use strict';

var child_process = require('child_process');
var slash = require('slash');
var fs = require('fs-extra');
var tmp = require('tmp');
var path = require('path');
var retry = require('retry');
var md5 = require('md5');

// A variable to save current config.org.emacsclient info
var emacsclient = "emacsclient";

// A variable to save current emacs server status
var emacs_server_is_dead = false;

// Set false to disable emacs server
var use_emacs_server = true;

// NOTE: Maybe make this variable configurable ?
// emacs daemon server-name
var server_name = "hexo-renderer-org";

// NOTE: Maybe make this variable configurable ?
// Directory to place server socket file
var server_dir = "./emacs.d/server";

// Emacs daemon server-file (full path)
var server_file = undefined;

function fix_filepath(s)
{
    // unix way of filepath
    s = slash(s);
    // convert " -> \"
    s = s.replace(/[\""]/g, '\\"');
    return s;
}

function fix_elisp(s)
{
    // remove lisp's comments
    s = s.replace(/^[\s\t]*;.*$/gm, "");

    // remove trailing garbage to prevent emacs eval fail
    s = s.replace(/\r?\n|\r/g, "");

    return s;
}

// convert true, false => t, nil
function elisp_bool(b)
{
    return (b == true) ? "t" : "nil";
}

function update_server_file(hexo) {
    var config = hexo.config;
    var cachedir = config.org.cachedir;
    server_file = path.join(process.cwd(), cachedir, server_dir, server_name);
    server_file = fix_filepath(server_file);
    if (config.org.debug)
        console.log("Update server_file =", server_file);
    return server_file;
}

// The file path of hexo-renderer-org may be too long for emacs --daemon, short it with md5
function get_server_name(hexo) {
    return md5(get_server_file(hexo))
}

function get_server_file(hexo) {
    return server_file || update_server_file(hexo);
}

function ensure_server_dir(hexo) {
    // NOTE: Ensure server-dir exist
    // It seems that emacs doesn't prepare the directory for the server-file,
    // and if the dir doesn't exist, emacs cannot create the server file properly,
    // though the server daemon is running, but emacsclient could never get the
    // connection done, because the inaccessibility to the server-file, this
    // behavior is confirmed on Windows MSYS2 MinGW64 Shell, with GNU Emacs 25.2.1,
    // installed using command pacman -S mingw-w64-x86_64-emacs.
    var dir = path.dirname(get_server_file(hexo));
    fs.mkdirsSync(dir);
    return fs.existsSync(dir);
}

// pass hexo.config to this function
function emacs_server_start(hexo)
{
    var config = hexo.config;
    config.highlight = config.highlight || {};

    if (!config.org.daemonize) {
        use_emacs_server = false;
        return;
    }

    // save config.org.emacsclient info
    emacsclient = config.org.emacsclient;

    // Find emacs entry point hexo-renderer-org.el
    var init_el = path.join(process.cwd(), "emacs", "hexo-renderer-org.el" );
    if (!fs.existsSync(init_el))
        init_el = path.join(process.cwd(), "node_modules", "hexo-renderer-org", "emacs", "hexo-renderer-org.el" );

    init_el = fix_filepath(init_el);

    var debug_file = tmp.fileSync();

    // convert user_config to absolute path
    var user_config = "";
    if (config.org.user_config)
        user_config = path.join(process.cwd(), path.normalize(config.org.user_config));

    // Ensure server-dir exist
    if (!ensure_server_dir(hexo)) {
        if (config.org.debug)
            console.log("Warning, directory to emacs server-file doesn't exist.");
        // FIXME: Maybe add some code here to avoid inaccessible emacs daemon.
    }

    var emacs_lisp = `
(progn
  ;; Setup user's config
  (setq hexo-renderer-org-cachedir     "${fix_filepath(config.org.cachedir) || ""}")
  (setq hexo-renderer-org-user-config  "${fix_filepath(user_config) || ""}")
  (setq hexo-renderer-org-theme        "${config.org.theme || ""}")
  (setq hexo-renderer-org-common-block "${config.org.common.replace(/\n/g, "\\n")}")
  (setq hexo-renderer-org--debug-file  "${fix_filepath(debug_file.name)}")
  (setq hexo-renderer-org--use-htmlize  ${elisp_bool((config.org.htmlize))})
  (setq org-hexo-use-htmlize            ${elisp_bool((config.org.htmlize))})
  (setq org-hexo-use-line-number        ${elisp_bool(config.org.line_number)})
  ;; load init.el
  (load "${init_el}"))
`;

    if (config.org.debug) {
        console.log("\n------------------------------");
        console.log("emacs: ", config.org.emacs);
        console.log("emacs_lisp: \n", emacs_lisp);
        console.log("\n------------------------------");
    }

    // Remove triling garbages
    emacs_lisp = fix_elisp(emacs_lisp);

    var exec_args = ['-Q','--daemon=' + get_server_name(hexo), '--eval', emacs_lisp];

    var proc = child_process.spawn(config.org.emacs, exec_args, {
        stdio: 'inherit'              // emacs's htmlize package need tty
    });

    proc.on('exit', function(code) {
        try {
            var oops = JSON.parse(fs.readFileSync(debug_file.name, "utf8"));
            console.error(oops.message);
            emacs_server_is_dead = true;
            hexo.exit(-1);      // FIXME: why this can't really make 'hexo s' stop ?
        }
        catch(e) {
            // forget about it :)
        }
    });

    return proc;
}

function emacs_server_stop(hexo)
{
    return new Promise((resolve,reject) => {
        var config = hexo.config;

        if (emacs_server_is_dead)
            return resolve();

        if (!use_emacs_server)
            return resolve();

        var proc = child_process.spawn(emacsclient, ['-s', get_server_name(hexo), '-e', '(kill-emacs)'], {
            // detached: true
        });

        proc.on('exit', function(code) {
            if (code != 0) {
                // FIXME:
                // What timeout interval is better, or spinning with no timeout ?
                setTimeout(() => {
                    if (config.org.debug)
                        console.log("Wait for emacs daemon exit!!");
                    resolve(emacs_server_stop(hexo));
                }, 100);
                return;
            }
            // FIXME:
            // What if emacsclient doesn't successfully connect to emacs daemon (wrong server file name),
            // or emacs daemon doesn't successfully initialize, the emacs daemon becomes a zombie process
            // without actually serving as a server. In these situation the command '(kill-emacs)' will
            // never reach to emacs daemon, thus causing endless wait.
            resolve();
        });
    });
}

function emacs_server_wait(hexo)
{
    return new Promise((resolve,reject) => {
        var config = hexo.config;
        if (emacs_server_is_dead)
            return resolve();

        if (!use_emacs_server)
            return resolve();

        var proc = child_process.spawn(emacsclient, ['-s', get_server_name(hexo), '-e', '(message "ping")'], {
        });

        proc.on('exit', function(code) {
            if (code != 0) {
                // FIXME:
                // What timeout interval is better, or spinning with no timeout ?
                setTimeout(() => {
                    if (config.org.debug)
                        console.log("Wait for emacs daemon startup!!");
                    resolve(emacs_server_wait(hexo));
                }, 100);
                return;
            }
            // FIXME:
            // What if emacsclient doesn't successfully connect to emacs daemon (wrong server file name),
            // or emacs daemon doesn't successfully initialize, the emacs daemon becomes a zombie process
            // without actually serving as a server. In these situation the command '(kill-emacs)' will
            // never reach to emacs daemon, thus causing endless wait.
            resolve();
        });
    });
}

function emacs_client(hexo, data, callback)
{
    var config = hexo.config;
    if (emacs_server_is_dead)
        return;

    config.highlight = config.highlight || {};

    var emacs_path = config.org.emacs;

    var output_file = tmp.fileSync();

    var emacs_lisp = `
(progn
  ;; render file according to args
  (hexo-renderer-org '(:file         "${fix_filepath(data.path)}"
                       :output-file  "${fix_filepath(output_file.name)}"
                       )))
`;

    // Enable this for debugging
    if (config.org.debug) {
        console.log("\n------------------------------");
        console.log("emacsclient: ", config.org.emacsclient);
        console.log("emacs_lisp: \n", emacs_lisp);
        console.log("\n------------------------------");
    }

    // Remove triling garbages
    emacs_lisp = fix_elisp(emacs_lisp);

    var exec_args = ['-s', get_server_name(hexo), '-e', emacs_lisp];

    // if (config.org.export_cfg != '')
    //    exec_args.splice(1,0,'--execute', config.org.export_cfg);

    var operation = retry.operation( {
        retries: 100,
        factor: 2,
        minTimeout: 100,
        maxTimeout: 1000,
        randomize: true
    });

    operation.attempt(function (currentAttempt) {

        var proc = child_process.spawn(config.org.emacsclient, exec_args, {
            stdio: 'inherit'
        });

        function retryOrExit(err) {
            if (config.org.debug)
                console.log("RETRY: ", data.path);

            if (emacs_server_is_dead)
                callback("");

            const retrying = operation.retry(err);
            if (!retrying) {

                if (config.org.debug)
                    console.log("DONE: ", data.path);

                var result = fs.readFileSync(output_file.name, 'utf8');
                callback(result); // return callback
            }
        }

        proc.on('exit', (code, signal) => {
            retryOrExit(code !== 0);
        });

        proc.on('error', (err) => {
            retryOrExit(err);
        });
    });
}


// For emacs process method:
// 1. use user's ~/.emacs.d 's org-mode files
// 2. use emacs process to renderer file
// 3. does not support htmlize syntax higlight
// 4. maybe more slow
// 5. just for some system can't use emacs daemon method
function emacs_process(hexo, data, callback)
{
    var config = hexo.config;
    config.highlight = config.highlight || {};

    var emacs_path = config.org.emacs;

    // Find emacs entry point hexo-renderer-org.el
    var init_el = path.join(process.cwd(), "emacs", "hexo-renderer-org.el" );
    if (!fs.existsSync(init_el))
        init_el = path.join(process.cwd(), "node_modules", "hexo-renderer-org", "emacs", "hexo-renderer-org.el" );

    init_el = fix_filepath(init_el);

    var debug_file = tmp.fileSync();
    var output_file = tmp.fileSync();

    // convert user_config to absolute path
    var user_config = "";
    if (config.org.user_config)
        user_config = path.join(process.cwd(), path.normalize(config.org.user_config));

    var emacs_lisp = `
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

    var exec_args = ['--batch', '--eval', emacs_lisp];

    // if (config.org.export_cfg != '')
    //    exec_args.splice(1,0,'--execute', config.org.export_cfg);

    var proc = child_process.spawn(config.org.emacs, exec_args);

    proc.on('exit', (code, signal) => {
        if (config.org.debug)
            console.log("DONE: ", data.path);

        var result = fs.readFileSync(output_file.name, 'utf8');
        callback(result); // return callback
    });
}

module.exports = {

    server: {
        start: function(hexo) {
            return emacs_server_start(hexo);
        },
        stop: function(hexo) {
            return emacs_server_stop(hexo);
        },
        wait: function(hexo) {
            return emacs_server_wait(hexo);
        }
    },

    client: function(hexo, data, callback) {
        return emacs_client(hexo, data, callback);
    },

    process: function(hexo, data, callback) {
        return emacs_process(hexo, data, callback);
    }
};
