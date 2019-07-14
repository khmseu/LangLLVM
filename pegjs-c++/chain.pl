#! /usr/bin/perl -w

use 5.026;
use strict;
no warnings qw( experimental::smartmatch );
use Data::Dumper::Simple;
$Data::Dumper::Useqq = 1;
$Data::Dumper::Sortkeys = 1;
$| = 1;

sub ruleparser(@);
sub ruleparser(@)
{
	#my @ruleparser = @_; print "-\n", Dumper(@ruleparser), "-\n";
	my @alts;
	my @alt;
	while (@_) {
		my $t = shift @_;
		given ($t) {
			when ( /^\w+$|^".*"$|^'.*'$|^\[.*\]$/ ) {
				push @alt, $t;
			};
			when ( '/' ) {
				push @alts, [ ' ', @alt ];
				@alt = ();
			};
			when ( [ '+', '*', '?' ] ) {
				my $m = pop @alt;
				push @alt, [ $t, $m ];
			};
			when ( '!' ) {
				my $m = shift @_;
				if ($m eq '(') {
					my @sub;
					my $level = 1;
					while (@_) {
						my $s = shift @_;
						#print "-\n", Dumper($level, $s, @sub), "-\n";
						given ($s) {
							when ( '(' ) {
								$level++;
								push @alt, $t;
							};
							when ( ')' ) {
								$level--;
								unless ($level) {
									$m = [ '()', ruleparser @sub ];
									#print "-\n", Dumper(@sub, $m), "-\n";
									last;
								};
								push @alt, $t;
							};
							default {
								push @sub, $s;
							};
						};
					};
				};
				#print "-\n", Dumper($t, $m), "-\n";
				push @alt, [ $t, $m ];
			};
			when ( '(' ) {
				my @sub;
				my $level = 1;
				while (@_) {
					my $s = shift @_;
					given ($s) {
						when ( '(' ) {
							$level++;
							push @alt, $t;
						};
						when ( ')' ) {
							$level--;
							unless ($level) {
								push @alt, [ '()', ruleparser @sub ];
								last;
							};
							push @alt, $t;
						};
						default {
							push @sub, $s;
						};
					};
				};
			};
			default {
				die "What is |$t|?";
			};
		};
	};
	push @alts, [ ' ', @alt ];
	return [ '|', @alts ];
};

my @tokens;
my %grammar;
my @comms;
my @grammar_seq;
while (<>) {
	s/\s+$//s;
	given ($_) {
		when ( m{^\s*$|^//} ) {
			push @comms, $_;
		};
		default {
			while (length $_) {
				s/^\s+//;
				if (m{^(//.*$|/\*.*?\*/|[=/();*?+!]|\w+\b|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|\[(?:[^][\\]|\\.)*\])(.*)$}) {
					my $token = $1;
					$_ = $2;
					given ($token) {
						when ( m{^/[/*]} ) {
							push @comms, $token;
						};
						when ( ';' ) {
							my $def = shift @tokens;
							push @grammar_seq, $def;
							$grammar{$def}{comms} = [ @comms ];
							my @head;
							push @head, shift @tokens until @head and $head[$#head] eq '=';
							pop @head;
							$grammar{$def}{head} = [ @head ];
							#print "-\n", Dumper($def, @comms, @head, @tokens), "-\n";
							$grammar{$def}{rules} = ruleparser @tokens;
							#$grammar{$def}{tokens} = [ @tokens ];
							@comms = ();
							@tokens = ();
						};
						default {
							push @tokens, $token;
						};
					};
					next;
				};
				die "What is |$_|?";
			};
		};
	};
};
die (Dumper(@comms, @tokens) . "Incomplete at eof") if @comms or @tokens;
#print "-\n", Dumper(%grammar), "-\n";

my %chain;
for my $d (keys %grammar) {
	my $r = $grammar{$d}->{rules};
	if (2 == @$r and $r->[0] eq '|' and 2 == @{$r->[1]} and $r->[1][0] eq ' ' and $r->[1][1] =~ /^\w+$/) {
		$chain{$d} = $r->[1][1];
	};
};
for my $c (keys %chain) {
	$chain{$c} = $chain{$chain{$c}} while exists $chain{$chain{$c}};
};
print "#\n", Dumper(%chain), "-\n";

sub unchain($);
sub unchain($) {
	my $r = shift @_;
	for my $t (@$r) {
		if (ref $t eq 'ARRAY') {
			unchain $t;
		}
		elsif ($t =~ /^\w+$/) {
			$t = $chain{$t} if exists $chain{$t};
		};
	};
};
for my $d (keys %grammar) {
	my $r = $grammar{$d}{rules};
	unchain $r;
};
#print "-\n", Dumper(%grammar), "-\n";

sub rec2str($$);
sub rec2str($$) {
	#print STDERR Dumper(@_);
	my ($ind, $r) = @_;
	#print STDERR "$ind " . Dumper($r);
	return $r if '' eq ref $r;
	my @r = @$r;
	my $rk = shift @r;
	my $pfx = ' ' x ($ind*2);
	given ($rk) {
		when ( '|' ) {
			return join ("\n$pfx/ ", map { rec2str $ind, $_ } @r);
		};
		when ( ' ' ) {
			return join (" ", map { rec2str $ind, $_ } @r);
		};
		when ( [qw( + * ? )] ) {
			return join (" ", map { rec2str $ind, $_ } @r) . $rk;
		};
		when ( '!' ) {
			return '!' . join (" ", map { rec2str $ind, $_ } @r);
		};
		when ( '()' ) {
			return "\n$pfx (\n$pfx    " . join (" ", map { rec2str $ind+1, $_ } @r) . "\n$pfx )";
		};
		default {
			die (Dumper($r, @r) . " What is »$rk«?");
		};
	};
};
for my $d (@grammar_seq) {
	unless (exists $chain{$d}) {
		my $g = $grammar{$d};
		my @c = @{$g->{comms}};
		push @c, '' if @c;
		print join("\n", @c);
		#print Dumper(@c);
		print join(" ", $d, @{$g->{head}}), "\n";
		print '  = ', rec2str(1, $g->{rules}), "\n  ;\n\n";;
	};
};

