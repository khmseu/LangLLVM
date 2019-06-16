#! /usr/bin/perl -w

use 5.026;
use strict;
no warnings qw( experimental::smartmatch );
$| = 1;

my $current;
my $comm;
my @rules;
my @rrules;
while (<>) {
	s/\s+$//s;
	given ($_) {
		when ( /^\s*$/ ) {
			# ignore
		};
		when ( '///' ) {
			next unless defined $current;
			if (@rrules) {
				print "$current $comm\n  = ( ", join("\n  / ", @rules), " )\n  ( ", join("\n  / ", @rrules), " )*\n";
			}
			else {
				print "$current $comm\n  = ", join("\n  / ", @rules), "\n";
			};
			$current = $comm = undef;
			@rules = @rrules = ();
		};
		when ( m{^(\w+)\s*((//.*)?)$} ) {
			die "Incomplete! |$current| at |$_|" if defined $current;
			$current = $1;
			$comm = $2;
		};
		when ( not defined $current ) {
			die "\$current not defined at |$_|";
		};
		when ( m{^\s+[=/]\s+$current\s+(.*)$} ) {
			push @rrules, $1;
		};
		when ( m{^\s+[=/]\s+(.*)$} ) {
			push @rules, $1;
		};
		default {
			die "What's this? |$_|";
		};
	};
};
die "Incomplete! |$current| at eof" if defined $current;
