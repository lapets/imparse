<?php /**************************************************************
**
** imparse.php
**
**   A cross-platform parser library.
**
**   Web:     imparse.org
**   Version: 0.0.0.2
**
*/

include 'imparse.php';

class Generate() {

  /////////////////////////////////////////////////////////////////////
  // Grammar used to parse BNF

  // Useful regex strings
  $reserved = '#\$\*\+\(\)\{\}\[\]\|';
  $comment = '#[^#]+';
  $regExpr = '\$[^$]+\$';
  $production = '[A-Z][A-Za-z0-9]*';
  $label = '[A-Z][A-Za-z0-9]*';
  $nonterminal = '`[A-Z][A-Za-z0-9]*';
  $terminal = '[^A-Z' + $reserved + '][^\s' + $reserved + ']*';
  
  genUxadt = Grammar(array(
    // Comments
    Production('Comment', array(
      Choices(array(
        Choice('Comment', AssocNone(), array(
          RegExpr('/' + $comment + '/')
          ))
        ))
      )),
  
    // Grammar
    Production('Grammar', array(
      Choices(array(
        Choice('Grammar', AssocNone(), array(
          Many(array(Nonterminal('Production')))
          ))
        ))
      )),
  
    // Productions
    Production('Production', array(
      Choices(array(
        Choice('Production', AssocNone(), array(
          May(array(Nonterminal('Comment'))), RegExpr('/' + $production + '/'), 
          Terminal('::='), Many(array(Nonterminal('Choice')))
          ))
        ))
      )),
  
    // Choice 
    Production('Choice', array(
      Choices(array(
        Choice('Choice', AssocNone(), array(
          RegExpr('/' + $label + '/'), Terminal('|'), Many(array(Nonterminal('Expression')))
          ))
        ))
      )),
  
    // Expression 
    Production('Expression', array(
      Choices(array(
        // May
        Choice('May', AssocNone(), array(
          Terminal('['), Many(array(Nonterminal('Expression'))), Terminal(']')
          )),
        // Many
        Choice('MayMany', AssocNone(), array(
          Nonterminal('Expression'), Terminal('+')
          )),
        // MayMany
        Choice('MayMany', AssocNone(), array(
          Nonterminal('Expression'), Terminal('*')
          )),
        Choice('MayMany', AssocNone(), array(
          Terminal('{'), Many(array(Nonterminal('Expression'))), Terminal('}')
          )),
        // Group expressions
        Choice('Group', AssocNone(), array(
          Terminal('('), Many(array(Nonterminal('Expression'))), Terminal(')')
          )),
        )),
  
      Choices(array(
        // Regular expression
        Choice('RegExpr', AssocNone(), array(
          RegExpr('/' + $regExpr + '/')
          )),
        // Nonterminals
        Choice('Nonterminals', AssocNone(), array(
          RegExpr('/' + $nonterminal + '/')
          )),
        // Empty string 
        Choice('EmptyStr', AssocNone(), array(
          Terminal('\"\"')
          )),
        // Nonterminals
        Choice('Terminals', AssocNone(), array(
          RegExpr('/\"[' + $reserved + ']\"/')
          )),
        // Nonterminals
        Choice('Nonterminals', AssocNone(), array(
          RegExpr('/' + $terminal + '/')
          ))
        ))
      ))
    ));

  static function bnfToUxadt($bnfFile) {
    $tokens = bnfTokenize($bnfFile);
    $r = imparse::parser($genUxadt, $tokens);
    if ($r != null) {
      // pretty print
      $r = toUxadt($r);
      return $r;
    }
    return null;
  }

  static function bnfTokenize($bnfFile) {
    $bnf = open($bnfFile); // Open file

    $termRes = '\\\"[' + $reserved + ']\\\"'; // Reserved characters as terminals
    $termNT = '::=\[^\s' + $reserved + ']+'; // Terminals and nonterminals

    $regex = join('|', array( // FIX
      $comment,
      $regExpr,
      ('[' + $reserved + ']'),
      $termRes,
      $termNT
      );
    $regex = compile(regex); // FIX

    $tmp = array();
    $tokens = array();
    foreach ($bnf as $line) {
      $tmp[] = regex.findall($line);
    }
    foreach ($tmp as $t) {
      //if ($t != null || strlen(trim($t)) == 0 || strlen($t) != 0)
      if ($t != null || strlen(trim($t)) != 0) {
        $tokens[] = $t;
      }
    }

    $bnf.close(); // close file
    return $tokens;
  }


  static function toUxadt($ptree) {
    $ps = $ptree['Grammar'];
    $ps2 = array();

    foreach ($ps as $p) {
      $pid = $p['Production'][0];
      $cs = $p['Production'][1];
      $cb = array();

      foreach ($cs as $c) {
        $label = '';
        $seq = array();

        for (i = 0; i < count($c['Choice']); i++) {
          if (i == 0) {
            // Shorten this part
            $label = $c['Choice'][0];
            if $label == 'None' {
              $label = null;
            }
          } else {
            $es = $c['Choice'][i];
            $r = toUxadtExpr($es);
            if ($r != null) {
              $seq[] = $r;
            } else {
              break;
            }
          }
        }
        $cb[] = Choice($label, AssocNone(), $seq);
      }
    $ps2[] = Production($pid, array(Choices($cb)));
    }

    return Grammar($ps2)
  }


  static function toUxadtExpr($es) {
    if (type($es) == string) {
      return Terminal($es);
    }

    $cs = array();
    $ty = array_keys($es)[0]; // ?????
    $x = $es[$ty][0];

    // Comment
    if ($ty == 'Comment') {
      while ($x[0] == '#' || strlen(trim($x[0])) == 0) {
        array_shift($x);
      }
      if ($x[-1] == '\n') {
        array_shift($x);
      }
      $cs[] = Comment($x);
    // Terminal
    } else if ($ty == 'Terminal') {
      if (strlen($x == 3 && $x[0] == '\"' && $x[-1] == '\"') {
        $x = $x[1];
      }
      $cs[] = Terminal($x);
    } else if ($ty == 'EmptyStr') {
      $cs[] = Terminal($'\"\"');
    // Regular Expression
    } else if ($ty == 'RegExpr') {
      $x = '/' + array_shift($x) + '/'; // Also subtract the last element of x
      $cs[] = RegExpr($x);
    // Nonterminal
    } else if ($ty == 'Nonterminal') {
      $cs[] = Nonterminal(array_shift($x));
    // One
    } else if ($ty == 'One') {
      $cs[] = One(toUxadtExpr($x));
    // May
    } else if ($ty == 'May') {
      $m = array();
      foreach ($es[$ty] as $e) {
        $m[] = toUxadtExpr($e);
      }
      $cs[] = May($m);
    // Many
    } else if ($ty == 'Many') {
      $m = array();
      foreach ($es[$ty] as $e) {
        $m[] = toUxadtExpr($e);
      }
      $cs[] = Many($m);
    // MayMany
    } else if ($ty == 'MayMany') {
      $m = array();
      foreach ($es[$ty] as $e) {
        $m[] = toUxadtExpr($e);
      }
      $cs[] = MayMany($m);
    // Group
    } else if ($ty == 'Group') {
      $m = array();
      foreach ($es[$ty] as $e) {
        $cs[] = toUxadtExpr($e);
      }
    }
    return $cs;
  }



  /////////////////////////////////////////////////////////////////////
  // Write UxADT grammar to a file
  static function writeUxadt($u, $name = 'gen', $indent = 2) {
  // Add a parameter to choose output file time - eg. .py, .php
    $s = uxadtToStr($u, $indent);
    $f = open($name + '.php', 'w');
    $f.write('
/////////////////////////////////////////////////////////////////////
//
// UxADT grammar generated by Imparse.
//
/////////////////////////////////////////////////////////////////////
      ');
    $f.write('$' + $name, + 'Uxadt = ');
    $f.write($s);
    $f.close();
  }

  static function uxadtToStr($u, $indent = 2) {
    $indent = str_repeat(' ', $indent);
    $ps = $u->match(Grammar(_), function($ps) { return $ps; })->end;
    $st = 'Grammar(array(';
    
    foreach (get_object_vars($ps) as $p) {
      $p = $p->match(Production(_, _), function($nt, $cbs) { return array('nt' => $nt, 'cbs' => $cbs); })->end;
      $st .= '\n' . $indent . 'Production(\'' . $p['nt'] . '\', array(';

      foreach (get_object_vars($p['cbs']) as $cb) {
        $cs = $cb->match(Choices(_), function($cs) { return $cs; })->end;
        $st .= '\n' . str_repeat($indent, 2) . 'Choices(array(';

        foreach (get_object_vars($cs) as $c) {
          $c = $c->match(Choice(_, _, _), function($l, $a, $seq) { return array('label' => $l, 'seq' => $seq); })->end;
          if ($c['label'] == null) {
            $st .= '\n' . str_repeat($indent, 3) . 'Choice(null, AssocNone(), array(';
          } else {
            $st .= '\n' . str_repeat($indent, 3) . 'Choice(\'' . $c['label'] . '\', AssocNone(), array(';
          }
          $r = uxadtSeqToStr($seq);
          $st .= '\n' . str_repeat($indent, 4) . $r . '\n' . str_repeat($indent, 4) . '),';
        }
        $st .= '\n' . str_repeat($indent, 3) . '),';
      }
      $st .= '\n' . str_repeat($indent, 2) . '),';
    }
    $st .= '\n' . $indent . ')';

    return $st;
  }

  static function uxadtSeqToStr($seq) {
    $s = '';
    foreach (get_object_vars($seq) as $e) {
      $e = etype($e);

      // One
      if (array_key_exists('One', $e)) {
        $r = uxadtSeqToStr($e['One']);
        $s .= 'One(\'' . $r . '\'), ';
      // May
      } else if (array_key_exists('May', $e)) {
        $r = uxadtSeqToStr($e['May']);
        $s .= 'May(\'' . $r . '\'), ';
      // Many
      } else if (array_key_exists('Many', $e)) {
        $r = uxadtSeqToStr($e['Many']);
        $s .= 'Many(\'' . $r . '\'), ';
      // MayMany
      } else if (array_key_exists('MayMany', $e)) {
        $r = uxadtSeqToStr($e['MayMany']);
        $s .= 'MayMany(\'' . $r . '\'), ';
      // Terminal
      } else if (array_key_exists('Terminal', $e)) {
        $s .= 'Terminal(\'' . $e['Terminal'] . '\'), ';
      // Regular Expression
      } else if (array_key_exists('RegExpr', $e)) {
        $s .= 'RegExpr(\'' . $e['RegExpr'] . '\'), ';
      // Nonterminal
      } else if (array_key_exists('Nonterminal', $e)) {
        $s .= 'Nonterminal(\'' . $e['Nonterminal'] . '\'), ';
      }
    }
    
    return $s;
  }
}

?>
