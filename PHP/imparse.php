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

/////////////////////////////////////////////////////////////////////
// Data type definitions 

eval(uxadt::definition(array(
  'Grammar' => array(_),
  )));

eval(uxadt::definition(array(
  'Normal' => array(),
  'IndentPresentation' => array(),
  'IndentRequired' => array(),
  )));

eval(uxadt::definition(array(
  'Production' => array(_, _),
  )));

eval(uxadt::definition(array(
  'Choices' => array(_),
  )));

eval(uxadt::definition(array(
  'Choice' => array(_, _, _)
  )));

eval(uxadt::definition(array(
  'Just' => array(_),
  'Nothing' => array()
  )));

eval(uxadt::definition(array(
  'AssocNone' => array(),
  'AssocLeft' => array(),
  'AssocRight' => array(),
  'AssocFlat' => array()
  )));

eval(uxadt::definition(array(
  'One' => array(_),
  'May' => array(_),
  'Many' => array(_),
  'MayMany' => array(_)
  )));

eval(uxadt::definition(array(
  'Nonterminal' => array(_),
  'RegExpr' => array(_),
  'Terminal' => array(_),
  )));


/////////////////////////////////////////////////////////////////////
// Functionality

class imparse {
  $ps = null;
  // ADD ALL THE FUNCTIONS HERE YO
}

/////////////////////////////////////////////////////////////////////
// Tokenizer
function tokenize($ps, $s) {
  $choices = array();
  foreach (get_object_vars($ps) as $p) {
    $cbs = $p->match(Production(_, _), function($nt, $cbs) {
      foreach (get_object_vars($cbs) as $cb) {
        $cb = $cb->match(Choices(_), function($cs) {
          foreach (get_object_vars($cb) as $c)
            $choices[] = $c;
        })->end;
      }
    })->end;
  }

  // Get terminals
  $terminals = array();
  foreach (get_object_vars($choices) as $c) {
    $seq = $c->match(Choice(_, _, _), function($l, $a, $seq) { return $seq; })->end;
    $r = tok($seq);
    if (len($r) > 0) {
      foreach ($r as $t) {
        if ($t not in $terminals)
          $terminals[] = $t;
      }
    }
  }
  //}}}

  // Split
  $tmp = array();
  $tokens = array();
  echo $tokens;
  return $tokens;
}

function tok($seq) {
  $terminals = array();
  foreach (get_object_vars($seq) as $x) {
    $e = etype($x);
    if (array_key_exists('Terminal', $e) {
      // t = re.escape()
      if (!in_array($t, $terminals))
        $terminals[] = $t;
    } else if (array_key_exists(array('May', 'Many', 'MayMany'), $e) {
      $r = tok($e);
      if (count($r) > 0)
        $terminals = array_merge($terminals, $r);
    }
  }
  return $terminals;
}


/////////////////////////////////////////////////////////////////////
// Parser
function parser($grammar, $s) {
  $ps = $grammar->match(Grammar(_), function($ps) { return $ps; })->end;
  if (gettype($s) == 'string') {
    $tokens = tokenize($ps, $s);
  } else {
    $tokens = $s;
  }
  // echo $tokens;

  $r = parse($ps, $tokens);
  if (!$r == null) {
    ($ptree, $tokens) = $r;
    if (count($tokens) == 0) {
      echo 'prettyprint tokens';
      return  $ptree;
    }
  echo 'Syntax error occurred, input could not be parsed.';
  return null;
  }
}

function parse($ps, $tokens, $nt, $leftFactor) {
  foreach (get_object_vars($ps) as $p) {
    $pnt = $p->match(Production(_, _), function($nt, $cbs) { return $nt; })->end;
    if ($pnt != $nt) {
      if ($nt != null) {
        continue;
      }
    }

    $cbs = $p->match(Production(_, _), function($nt, $cbs) { return $cbs; })->end;
    foreach (get_object_vars($cbs) as $cb) {
      $cs = $cb->match(Choices(_), function($cs) { return $cs; })->end;
      foreach (get_object_vars($cs) as $c) {
        $label = $c->match(Choice(_, _, _), function($label, $a, $seq) { return $label; })->end;
        $seq = $c->match(Choice(_, _, _), function($label, $a, $seq) { return $seq; })->end;
        $ts = 0;

        if ((count($tokens) == 0) && (count($seq) == 0)) {
            return ($label, array());
          }
        }

        $r = parseExpr($ps, $tokens, $seq, ($nt, $pnt), $leftFactor);
        if (($r != null) && ($r != true)) {
          return $r;
        }
      }
    }
  }
}

function parseExpr($ps, $tokens, $seq, $label = null, $nt = null, $leftFactor = false) {
  $pnt = null;
  if ($nt != null) {
    ($nt, $pnt) = $nt;
  }

  $ts = 0;
  $es = array();
  $inseq = 0;
  foreach (get_object_vars($seq) as $x) {
    $expr = etype($x);

    // One
    if (array_key_exists('One', $expr) {
      $seq2 = $expr;
      foreach (get_object_vars($seq2) as $s) {
        $r = parseExpr($ps, $tokens, $s);
        if ($r != null) {
          $e = $r[0];
          $tokens = $r[1];
          if ($e == array()) {
            $ts++;
            break;
          } else {
            $es[] = $e;
            break;
          }
        }
      }
      if ($r == null) {
        break;
      }

    // May, Many, MayMany
    } else if (some_key_exists(array('May', 'Many', 'MayMany'), $expr) {
      $may = some_key_exists(array('May', 'MayMany'), $expr);
      $seq2 = $expr;
      $r = parseExpr($ps, $tokens, $seq2);
      if ($r != null) {
        $e = $r[0];
        $tokens = $r[1];
        if ($e == array()) {
          $ts++;
        } else {
          $es[] = $e;
        }
      } else { // $r is null
        if ($may) {
          $ts++;
        } else {
          break;
        }
      }
      if (some_key_exists(array('Many', 'MayMany'), $expr)) {
        while ($r != null && count($tokens) > 0) {
          $r = parseExpr($ps, $tokens, $seq2);
          if ($r != null) {
            $e = $r[0];
            $tokens = $r[1];
            if ($e == array()) {
              $ts++;
              $inseq++;
            } else {
              $es[] = $e;
              $inseq++;
            }
          }
        }
      }
    
    // Terminal
    } else if (array_key_exists('Terminal', $expr)) {
      if (count($tokens) > 0 && $tokens[0] == $expr['Terminal']) {
        array_shift($tokens);
        $ts++;
      } else {
        break;
      }

    // Regular Expression
    } else if (array_key_exists('RegExpr', $expr)) {
      if (substr($expr['RegExpr'], 0) == '/' && substr($expr['RegExpr'], -1) == '/') {
        if (count($tokens) > 0 && /*regexpr check */) {
          $es[] = $tokens[0];
          array_shift($tokens);
        } else {
          break;
        }
      }

    // Nonterminal
    } else if (array_key_exists('Nonterminal', $expr)) {
      if ($ts + count($es) == 0) {
        if ($expr['Nonterminal'] == $nt && $leftFactor) { // Skipping pattern
          break;
        } else if ($expr['Nonterminal'] == $pnt) {
          $r = parseExpr($ps, $tokens, $expr, true);
        } else {
          $r = parseExpr($ps, $tokens, $expr, false);
        }
      } else { // Pattern does not start with a nonterminal
        $r = parseExpr($ps, $tokens, $expr, false);
      }
      if ($r != null) {
        $e = $r[0];
        $tokens = $r[1];
        $leftFactor = false;
      } else {
        break;
      }
    }
  }

  if ($ts + count($es) == count($seq) + $inseq) {
    if ($label == null && count($es) == 1) {
      return ($es[0], $tokens);
    } else if ($label == null) {
      return ($es, $tokens);
    } else {
      if (count($es) > 0) {
        return array(array($label => $es), $tokens);
      } else {
        return array($label, $tokens);
      }
    }
  } else {
    return null;
  }
}


/////////////////////////////////////////////////////////////////////
// Interact
function interact($u = null) {
  echo 'Interactive parser. Submit \':q\' or \':quit\' to exit.';
  if ($u != null) {
    $grammar = $u;
  }
  // Interactive loop.
  while (true) {
    // Prompt the user for a query.
    $s = input('> ');
    if ($s == ':q' || $s == ':quit') {
      break;
    }

    // Parse the query.
    $r = parser($grammar, $s);
    if ($r != null) {
      // pretty print $r
    } else {
      echo 'Unknown input.';
    } 
    echo '';

  }
}


/////////////////////////////////////////////////////////////////////
// Helper functions 
function etype($e) {
  return $e
    ->match(Terminal(_), function($t) { return array('Terminal' => $t); })
    ->match(RegExpr(_), function($r) { return array('RegExpr' => $r); })
    ->match(Nonterminal(_), function($nt) { return array('Nonterminal' => $nt); })
    ->match(One(_), function($seq) { return array('One' => $seq); })
    ->match(May(_), function($seq) { return array('May' => $seq); })
    ->match(Many(_), function($seq) { return array('Many' => $seq); })
    ->match(MayMany(_), function($seq) { return array('MayMany' => $seq); })
    ->end;

// Checks whether some key in $keys is found in $array.
function some_key_exists($keys, $array) {
  foreach ($keys as $k) {
    if (array_key_exists($k, $array)
      return true;
  return false;
}

// EOF
?>
