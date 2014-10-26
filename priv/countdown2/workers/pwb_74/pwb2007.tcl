#!/usr/bin/tclsh
##########   Programmierwettbewerb SS 2007 - Florian Grabbe (modified version for WS2013/14)  ##########

proc jachthund {lst a str k} {
    global result maxk
    set used [list]
    foreach b $lst {
        if {[lsearch -exact -integer $used $b] < 0} {
            lappend used $b
            set l [newlst $lst $b];               jachthund $l [expr {$b * $a}]         "($b * $str)"   [expr {$k + $b + ($b * $a)}]
                                                  jachthund $l [expr {$b + $a}]         "($b + $str)"   [expr {$k + $b + ($b + $a)}]
            if {[expr {$a % $b}] == 0}          { jachthund $l [expr {$a / $b}]         "($str / $b)"   [expr {$k + $b + ($a / $b)}] }
            if {[expr {$b % $a}] == 0}          { jachthund $l [expr {$b / $a}]         "($b / $str)"   [expr {$k + $b + ($b / $a)}] }
#           set x [expr {$a % $b}]; if {$x > 0} { jachthund $l $x                       "($str % $b)"   [expr {$k + $b + $x       }] }
#           set x [expr {$b % $a}]; if {$x > 0} { jachthund $l $x                       "($b % $str)"   [expr {$k + $b + $x       }] }
                                                  jachthund $l [expr {($a+$b)**2}]      "($str <+> $b)" [expr {$k + $b + (($a+$b)**2)}]
                                                  jachthund $l [expr {($b+$a)**2}]      "($b <+> $str)" [expr {$k + $b + (($b+$a)**2)}]
            if {$a > $b}                        { jachthund $l [expr {($a-$b)**2}]      "($str <-> $b)" [expr {$k + $b + (($a-$b)**2)}]
                                                  jachthund $l [expr {($a+$b)*($a-$b)}] "($str <*> $b)" [expr {$k + $b + (($a+$b)*($a-$b))}]
                                                  jachthund $l [expr {$a - $b}]         "($str - $b)"   [expr {$k + $b + ($a - $b)}] }
            if {$a < $b}                        { jachthund $l [expr {($b-$a)**2}]      "($b <-> $str)" [expr {$k + $b + (($b-$a)**2)}]
                                                  jachthund $l [expr {($b+$a)*($b-$a)}] "($b <*> $str)" [expr {$k + $b + (($b+$a)*($b-$a))}]
                                                  jachthund $l [expr {$b - $a}]         "($b - $str)"   [expr {$k + $b + ($b - $a)}] }
        }
    }
    if {($a == $result) && ($k > $maxk)} {
        set maxk $k
        regsub {^\((.+)\)|(.+)$} $str {\1\2} str
        puts stdout "$str"
        puts stderr "\[k=$k\]  $str"
    }
}

proc newlst {lst e} {
    set i [lsearch -exact -integer $lst $e]
    return [lreplace $lst $i $i]
}

# erste Zeile lesen: Liste der Zahlen
set nums [gets stdin]
# Das erste ([) und letzte (]) Zeichen abschneiden und als abwärts sortierte Liste speichern.
set n [lsort -integer -decreasing [split [string range $nums 1 [expr [string length $nums] - 2]] ,]]

# zweite Zeile lesen: Ergebnis
set result [gets stdin]

set maxk 0

# Liste vorsortieren (die höchste Zahl abwechselnd an den Anfang / das Ende haengen): 1 2 3 4 5 6 -> 6 4 2 1 3 5
set num ""; set i -1; foreach {x y} $n { set num [linsert $num [incr i] $x $y] }
# ggf. durchs Sortieren entstandene leere Elmente entfernen
while {[set i [lsearch -exact $num {}]] >= 0} { set num [lreplace $num $i $i] }

# Liste durchgehen (jede Zahl nur einmal)
set used [list]
foreach x $num {
    if {[lsearch -exact -integer $used $x] < 0} {
        lappend used $x
        jachthund [newlst $num $x] $x "$x" $x
    }
}
