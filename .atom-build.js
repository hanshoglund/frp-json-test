var T =
  { cmd: "stack build --install-ghc --fast"
  , name: "stack build"
  , cwd: "{PROJECT_PATH}/simple"
  , keymap: "cmd-U"
  , atomCommandName: "user:build"
  , preBuild: function() { console.log("prebuild") }
  , postBuild: function(res) {
      if (res) { atom.notifications.addSuccess('Build successifull', {dismissable: true}); }
      // else { atom.notifications.addError('Build failed', {dismissable: true}); }
    }
  , functionMatch: function(termout) {
      var matches = [];
      var stripProjectRoot = function(s){
        var pr = __dirname;
        var x = s.replace(/^ */, '').replace(pr, '');
        if (x[0] == '/') {
          return x.substr(1);
        } else {
          return x;
        }

      }
      termout.split(/\n/).forEach(function (rawline, rawlinenum, rawlines) {
        var match = rawline.match(/(.+\.hs):(\d+):(\d+):/);
        if (match) {
          var ctx = rawlines.slice(rawlinenum+1, rawlinenum + 2);
          var ctx2 = ["Error was:"]; // rawlines.slice(rawlinenum+1, rawlinenum + 6);
          var prefixLen = function(s) { if (s) {return s.match(/^ */)[0].length } else {return 0}};
          for (var i = rawlinenum+1; i<rawlinenum+55; i++) {
            if(prefixLen(rawlines[i]) > 4) { ctx2.push(rawlines[i]); }
          }

          matches.push({
              file: stripProjectRoot(match[1])
            , line: match[2]
            , col: match[3]
            , message: ctx.join("\n")
            , trace: [{ file: stripProjectRoot(match[1])
                      , line: match[2]
                      , col: match[3]
                      , message: ctx2.join("\n")
                      }]
          });
        }
      });
      return matches;
    }
  }

module.exports = T
