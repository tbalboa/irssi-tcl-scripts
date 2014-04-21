##
# stock ticker lookup
#
# auth: tommy balboa (tbalboa)
#       will storey (horgh)
# date: 2006-02-14
#

package require http

namespace eval ::stock {
	variable version 1.0

	settings_add_str "stock_enabled_channels" ""

	variable url "http://download.finance.yahoo.com/d/quotes.csv"
	variable market_list "^IXIC INDU ^GSPC ^GSPTSE"
	variable euro_list "^FTSE ^FCHI ^GDAXI"
	variable asia_list "^N225 ^HSI"

	signal_add msg_pub "!ret" ::stock::return_handler
	signal_add msg_pub "!return" ::stock::return_handler

	signal_add msg_pub "!q" ::stock::quote_handler

	signal_add msg_pub "!markets" ::stock::markets
	signal_add msg_pub "!euro" ::stock::euro
	signal_add msg_pub "!asia" ::stock::asia

	signal_add msg_pub "!currency" ::stock::currency
}

proc ::stock::get_symbol_data {symbol server chan} {
	set symbol_built [::stock::format_symbol $symbol]

	# real time version
	#                                [join {s n k1 c6 k2 g h v x t1 d1 w} {}] s $symbol_built]
	set query [::http::formatQuery f [join {s n l1 c1 p2 g h v x t1 d1 w} {}] s $symbol_built]

	set token [::http::geturl ${::stock::url} -query $query -timeout 60000 -command "::stock::http_callback $server $chan"]
}

proc ::stock::http_callback {server chan token} {
	set data [::http::data $token]
	::http::cleanup $token

	foreach line [::stock::format_symbol_data $data] {
		putchan $server $chan $line
	}
}

proc ::stock::format_symbol {symbol} {
	if {[regexp {[a-zA-Z0-9\. ]+} $symbol]} {
		return [string map {{ } +} $symbol]
	}

	return ""
}

# use index (eg +1.23 or -1.23) to colour str
proc ::stock::colour {index {str {}}} {
	if {[string length $str] == 0} {
		set str $index
	}

	if {[string index $index 0] == "-"} {
		return "\00304${str}\017"
	} elseif {[string index $index 0] == "+"} {
		return "\00309${str}\017"
	}

	return $str
}

# use index (eg +1.23 or -1.23) to choose arrow
proc ::stock::arrow {index} {
	if {[string index $index 0] == "-"} {
		return " [::stock::colour $index \u2193]"
	} elseif {[string index $index 0] == "+"} {
		return " [::stock::colour $index \u2191]"
	}

	return ""
}

# rewrite this so its not such a hack
proc ::stock::parse_csv {str} {
	set temp ""
	set open 0
	set alist [list]

	for {set i 0} {$i < [string length $str]} {incr i} {
		set char [string index $str $i]
		if {$char == "\""} {
			set open [expr $open == 0]
		} elseif {$char == ","} {
			if {$open} {
				append temp $char
			} else {
				lappend alist $temp
				set temp ""
			}
		} else {
			append temp $char
		}
	}
	lappend alist $temp
	return $alist
}

proc ::stock::format_symbol_data {data} {
	set output [list]

	foreach quote [split $data "\r\n"] {
		if {$quote == ""} {
			continue
		}

		foreach {symbol name last change percent range_lo range_hi volume exchange last_time last_date year_range} [::stock::parse_csv $quote] {
			if {$last == 0} {
				lappend output "Invalid symbol."
				continue
			}

			#set line "${symbol} ($name) Last: $\00311last [::stock::colour $change]"
			set line "\002${symbol}\002 ($name) Last:\00311 ${last}\017[::stock::arrow $change]"
			if {$change != "N/A"} {
				set line "$line [::stock::colour $change]"
			}
			if {$percent != "N/A"} {
				set line "$line [::stock::colour $percent]"
			}
			if {$volume != "N/A"} {
				set line "$line Volume: [::stock::number_format $volume]"
			}
			if {$range_lo != "N/A" && $range_hi != "N/A"} {
				set line "$line Daily Range: ${range_lo}-${range_hi}"
			}
			if {![regexp {N/A} $year_range]} {
				set year_range [regsub -all -- {\s} $year_range ""]
				set line "$line Yearly Range: $year_range"
			}
			if {$exchange != "N/A"} {
				set line "$line $exchange"
			}
			set line "$line \00310${last_time} $last_date"
			lappend output $line
		}
	}

	return $output
}

proc ::stock::number_format {value} {
	set str [list]
	foreach {a b c} [lreverse [split $value {}]] {
		lappend str "$c$b$a"
	}
	return [join [lreverse $str] ","]
}

proc ::stock::rate_of_return {capital value} {
	if {![regexp {[0-9\.]+} $capital] || ![regexp {[0-9\.]+} $value]} {
		return 0
	}

	return [format %.2f [expr {($value - $capital) / double($capital) * 100}]]
}

proc ::stock::return_handler {server nick uhost chan argv} {
	if {![str_in_settings_str "stock_enabled_channels" $chan]} { return }
	set argv [split $argv]
	putchan $server $chan "[::stock::rate_of_return [lindex $argv 0] [lindex $argv 1]]%"
}

proc ::stock::quote_handler {server nick uhost chan argv} {
	if {![str_in_settings_str "stock_enabled_channels" $chan]} { return }
	::stock::get_symbol_data $argv $server $chan
}

proc ::stock::markets {server nick uhost chan argv} {
	if {![str_in_settings_str "stock_enabled_channels" $chan]} { return }
	::stock::quote_handler $server $nick $uhost $chan $::stock::market_list
}

proc ::stock::euro {server nick uhost chan argv} {
	if {![str_in_settings_str "stock_enabled_channels" $chan]} { return }
	::stock::quote_handler $server $nick $uhost $chan $::stock::euro_list
}

proc ::stock::asia {server nick uhost chan argv} {
	if {![str_in_settings_str "stock_enabled_channels" $chan]} { return }
	::stock::quote_handler $server $nick $uhost $chan $::stock::asia_list
}

proc ::stock::currency {server nick uhost chan argv} {
	if {![str_in_settings_str "stock_enabled_channels" $chan]} { return }

	set argv [split $argv]
	if {[llength $argv] != 2} {
		putchan $server $chan "Usage: !currency <from> <to>"
		return
	}

	::stock::quote_handler $server $nick $uhost $chan "[join $argv {}]=x"
}

irssi_print "stock.tcl v $::stock::version loaded (c) tbalboa, horgh 2006"
