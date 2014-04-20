##
# unit conversion
#
# auth: tommy balboa (tbalboa)
# date: 2013-10-19
#

namespace eval ::convert {
	variable version 1.0

	settings_add_str "convert_enabled_channels" ""

	signal_add msg_pub !convert ::convert::main
	signal_add msg_pub .convert ::convert::main

	variable map [dict create]
	variable synonyms [dict create]

	foreach {unit synonym_list} [list \
		meter [list meters m] foot [list feet ft] \
		kilometer [list kilometers km kmh kph] mile [list miles mph] \
		centimeter [list centimeters cm] inch [list inches in] \
		kilogram [list kilograms kg kgs] pound [list pounds lb lbs] \
		celsius [list c] fahrenheit [list f] \
	] {
		dict set synonyms $unit $unit
		foreach synonym $synonym_list {
			dict set synonyms $synonym $unit
		}
	}

	# use meters for base distance
	dict set map meter [list]
	dict set map mile [list * 1609.34]
	dict set map foot [list * 0.3048]
	dict set map kilometer [list * 1000.0]
	dict set map centimeter [list * 0.01]
	dict set map inch [list * 0.0254]

	# use kilograms for base weight
	dict set map kilogram [list]
	dict set map pound [list * 0.453592]

	# use celsius for base temperature
	dict set map celsius [list]
	dict set map fahrenheit [list - 32 * 0.555555]
}

proc ::convert::main {server nick uhost chan argv} {
	if {![str_in_settings_str "convert_enabled_channels" $chan]} {
		return
	}

	lassign [::convert::parse $argv] value pre_unit post_unit

	if {![string is double $value]} {
		return
	}

	if {![dict exist $::convert::synonyms $pre_unit]} {
		return
	}

	if {![dict exist $::convert::synonyms $post_unit]} {
		return
	}

	set base [dict get $::convert::map [dict get $::convert::synonyms $pre_unit]]
	set inverse [dict get $::convert::map [dict get $::convert::synonyms $post_unit]]

	set post_value $value
	foreach {op ratio} $base {
		set post_value [::tcl::mathop::$op $post_value $ratio]
	}

	foreach {ratio op} [lreverse $inverse] {
		set inverse_op [string map {* / / * + - - +} $op]
		set post_value [::tcl::mathop::$inverse_op $post_value $ratio]
	}
	set post_value [format "%0.2f" $post_value]

	putchan $server $chan "${nick}: $value $pre_unit is $post_value $post_unit"
}

proc ::convert::parse {argv} {
	set argv [string tolower $argv]

	set value ""
	set i 0
	while {$i < [string length $argv]} {
		set char [string index $argv $i]
		if {![string is digit $char] && ![string match $char "."] && ![string match $char "-"]} {
			break
		}

		append value $char
		incr i
	}

	set units [split [string trim [string range $argv $i end] " "]]
	
	return [list $value [lindex $units 0] [lindex $units 2]]
}

irssi_print "convert.tcl v $::convert::version loaded (c) tbalboa 2013"
