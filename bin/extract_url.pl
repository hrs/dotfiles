#!/usr/bin/env perl

# License: BSD-2-Clause (simplified)
# URL: http://spdx.org/licenses/BSD-2-Clause
#
# Copyright (C) 2011-2013 Kyle Wheeler
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KYLE WHEELER "AS IS" AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KYLE WHEELER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

use MIME::Parser;
use HTML::Parser;
use Getopt::Std;
use Pod::Usage;
use Env;
use POSIX ":sys_wait_h";
use strict;
use warnings;

my $LICENSE = "BSD-2-Clause";
my $NAME = "extract_url";
my $version = "1.6.1";
my $txtonly = 0;
my $manual_quoted = 0;
my $list = '';
my $help = '';
my $configfile = '';

sub HELP_MESSAGE {
	pod2usage(0);
}
sub VERSION_MESSAGE {
	print "$NAME $version License:$LICENSE\n";
}

my $term_cols = 80;
my ($term_rows, $term_wpix, $term_hpix);
if (eval "use Term::Readkey") {
    ($term_cols, $term_rows, $term_wpix, $term_hpix) = GetTerminalSize();
} else {
    require 'sys/ioctl.ph';
    if (defined &TIOCGWINSZ and open(TTY, "+</dev/tty")) {
        my $winsize = '';
        unless (ioctl(TTY, &TIOCGWINSZ, $winsize)) {
            die sprintf "$0: ioctl TIOCGWINSZ (%08x: $!)\n", &TIOCGWINSZ;
        }
        ($term_rows, $term_cols, $term_wpix, $term_hpix) = unpack('S4', $winsize);
    }
}
my $list_width = $term_cols - 4; # 4 is for the border width on either side

my %options;
if (eval "use Getopt::Long; 1") {
	&GetOptions('Version' => sub { VERSION_MESSAGE(); exit; },
				'help' => sub { pod2usage(-exitval=>0,-verbose=>1); },
				'man' => sub { pod2usage(-exitval=>0, -verbose=>99); },
				'text' => \$txtonly,
				'quoted' => \$manual_quoted,
				'config=s' => \$configfile,
				'list!' => \$list) or pod2usage(-exitval=>2,-verbose=>1);
} else {
	$Getopt::Std::STANDARD_HELP_VERSION = 1;
	&getopts("c:hltqV",\%options) or pod2usage(-exitval=>2,-verbose=>1);
}
my $fancymenu = 1;
if ($options{'l'} || length $list) { $fancymenu = 0; }
if ($options{'V'}) { &VERSION_MESSAGE(); exit; }
if ($options{'h'}) { &HELP_MESSAGE(); }
if ($options{'q'}) { $manual_quoted = 1; }
if ($options{'t'}) { $txtonly = 1; }
if ($options{'c'}) { $configfile = $options{'c'}; }

# create a hash of html tag names that may have links
my %link_attr = (
	'a' => {'href'=>1},
	'applet' => {'archive'=>1,'codebase'=>1,'code'=>1},
	'area' => {'href'=>1},
	'blockquote' => {'cite'=>1},
	#'body'    => {'background'=>1},
	'embed'   => {'pluginspage'=>1, 'src'=>1},
	'form'    => {'action'=>1},
	'frame'   => {'src'=>1, 'longdesc'=>1},
	'iframe'  => {'src'=>1, 'longdesc'=>1},
	#'ilayer'  => {'background'=>1},
	#'img' => {'src'=>1},
	'input'   => {'src'=>1, 'usemap'=>1},
	'ins'     => {'cite'=>1},
	'isindex' => {'action'=>1},
	'head'    => {'profile'=>1},
	#'layer'   => {'background'=>1, 'src'=>1},
	'layer'   => {'src'=>1},
	'link'    => {'href'=>1},
	'object'  => {'classid'=>1, 'codebase'=>1, 'data'=>1, 'archive'=>1,
		'usemap'=>1},
	'q'       => {'cite'=>1},
	'script'  => {'src'=>1, 'for'=>1},
	#'table'   => {'background'=>1},
	#'td'      => {'background'=>1},
	#'th'      => {'background'=>1},
	#'tr'      => {'background'=>1},
	'xmp'     => {'href'=>1},
);

# find out the URLVIEW command
my $urlviewcommand="";
my $command_uses_shell = 1;
my $displaysanitized = 0; # means to display the pre-sanitized URL instead of the pretty one
my $shortcut = 0; # means open it without checking if theres only 1 URL
my $noreview = 0; # means don't display overly-long URLs to be checked before opening
my $persist  = 0; # means don't exit after viewing a URL (ignored if $shortcut == 0)
my $ignore_empty = 0; # means to throw out URLs that don't have text in HTML
my $default_view = "url"; # means what shows up in the list by default: urls or contexts
my $alt_select_key = 'k';
my $sanitize_reserved = 1;
sub read_extracturl_prefs
{
	my $f = shift(@_);
	open(PREFFILE, '<', $f) or return 0;
	while (<PREFFILE>) {
		my $lineread = $_;
		if ($lineread =~ /^ALTSELECT [A-Za-fh-z0-9,.<>?;:{}|!@#$%^&*()_=+-`~]$/) {
			$lineread =~ /ALTSELECT (.)/; $alt_select_key = $1;
		} elsif ($lineread =~ /^SHORTCUT$/)          { $shortcut = 1;
		} elsif ($lineread =~ /^NOREVIEW$/)          { $noreview = 1;
		} elsif ($lineread =~ /^PERSISTENT$/)        { $persist = 1;
		} elsif ($lineread =~ /^DISPLAY_SANITIZED$/) { $displaysanitized = 1;
		} elsif ($lineread =~ /^IGNORE_EMPTY_TAGS$/) { $ignore_empty = 1;
		} elsif ($lineread =~ /^RAW_RESERVED$/)      { $sanitize_reserved = 0;
		} elsif ($lineread =~ /^COMMAND (.*)/) {
			$lineread =~ /^COMMAND (.*)/;
			$urlviewcommand=$1;
			chomp $urlviewcommand;
			$urlviewcommand =~ s/\$([A-Za-z][A-Za-z0-9]*)/$ENV{$1}/g;
			if ($urlviewcommand =~ m/%s/) { $command_uses_shell = 1; }
			else { $command_uses_shell = 0; }
		} elsif ($lineread =~ /^DEFAULT_VIEW (.*)/) {
			$lineread =~ /^DEFAULT_VIEW (.*)/;
			if ($1 =~ /^context$/) {
				$default_view = "context";
			} else {
				$default_view = "url";
			}
		} elsif ($lineread =~ /^HTML_TAGS (.*)/) {
			$lineread =~ /^HTML_TAGS (.*)/;
			my @tags = split(',', $1);
			my %tags_hash;
			foreach my $tag (@tags) {
				$tags_hash{lc $tag} = 1;
			}
			foreach my $tag (keys %link_attr) {
				delete $link_attr{$tag} if (! exists($tags_hash{$tag}));
			}
		}
	}
	close PREFFILE;
	return 1;
}
sub read_urlview_prefs
{
	my $f = shift(@_);
	open(URLVIEW,'<',$ENV{HOME}."/.urlview") or return 0;
	while (<URLVIEW>) {
		if (/^COMMAND (.*)/) {
			$urlviewcommand=$1;
			chomp $urlviewcommand;
			$urlviewcommand =~ s/\$([A-Za-z][A-Za-z0-9]*)/$ENV{$1}/g;
			if ($urlviewcommand =~ m/%s/) { $command_uses_shell = 1; }
			else { $command_uses_shell = 0; }
			last;
		}
	}
	close URLVIEW;
}
sub getprefs
{
	if ($configfile ne '') {
		&read_extracturl_prefs($configfile) or die "Could not read config file ($configfile): $!\n";
	} elsif (exists $ENV{HOME}) {
		&read_extracturl_prefs($ENV{HOME}."/.extract_urlview") or
		&read_urlview_prefs($ENV{HOME}."/.urlview");
	}
	if ($urlviewcommand eq "") {
		if (exists $ENV{BROWSER}) {
			$urlviewcommand=$ENV{BROWSER};
		} else {
			$urlviewcommand = "open";
		}
		if ($urlviewcommand =~ m/%s/) { $command_uses_shell = 1; }
		else { $command_uses_shell = 0; }
	}
}

my %link_hash;
my %orig_text;
my $newlink = 1;
sub foundurl {
	my ($uri) = @_;
	#$uri =~ s/mailto:(.*)/$1/;
	if (! $link_hash{$uri}) {
		$link_hash{$uri} = $newlink++;
	}
}
my $foundurl_text_curindex = 0;
my $foundurl_text_lastindex = 0;
my $foundurl_text_prevurl = "";
my $foundurl_text_text;

sub foundurl_text {
	my ($uri,$orig) = @_;
	$uri = &renderuri($uri);
	$foundurl_text_curindex = index($$foundurl_text_text, $orig, $foundurl_text_lastindex);
	my $sincelast;
	if ($foundurl_text_curindex >= 0) {
		# this is the expected behavior
		$sincelast = &tidytext(substr($$foundurl_text_text,$foundurl_text_lastindex,($foundurl_text_curindex-$foundurl_text_lastindex)));
	} else {
		# something odd is going on. What's happened is that our URL finder has
		# found a URL that isn't in the text following the last URL it found.
		# It *may* be doing things out of order... but that's really strange.
		# We rely on it finding URLs in order of appearance in order to get
		# context information. I'll try to recover but whatever happens, we
		# can't get context information for this URL, and our context info for
		# other URLs may be seriously messed up!
		$foundurl_text_curindex = index($$foundurl_text_text, $orig);
		if ($foundurl_text_curindex >= 0) {
			# okay, we can recover... we'll just pretend that *everything* is
			# the sincelast text
			$sincelast = &tidytext(substr($$foundurl_text_text, 0, $foundurl_text_curindex));
		} else {
			# Very strange... I can't even find the URL! The best we can do is
			# continue without *any* context... but there's *SERIOUS* weirdness
			# going on, and expectations have been *majorly* violated. Let's
			# just hope the URL is already closed (and so already has context
			# information). I'm setting the curindex so that it'll be zero for
			# the next URL (i.e. we can pretend that everything up to the next
			# url is "sincelast")
			$foundurl_text_curindex = 0 - length($orig);
		}
		$sincelast = "";
	}
	$sincelast =~ s/<$//;
	$sincelast =~ s/^>//;
	&foundurl($uri);
	&process_sincelast($uri, $foundurl_text_prevurl, $sincelast);
	$foundurl_text_lastindex = $foundurl_text_curindex + length($orig);
	$foundurl_text_prevurl = $uri;
}
sub unfindurl {
	my($uri) = @_;
	delete($link_hash{$uri});
	delete($orig_text{$uri});
}
sub renderuri {
	my($uri) = @_;
	$uri =~ s/&amp;/&/gs;
	$uri =~ s/%([0-7][a-fA-F0-9])/chr(hex($1))/egs;
	return $uri;
}
sub sanitizeuri {
	my($uri) = @_;
	if ($sanitize_reserved) {
		$uri =~ s/([^a-zA-Z0-9_.~%:\/-])/sprintf("%%%X",ord($1))/egs;
	} else {
		if ($command_uses_shell) {
			$uri =~ s/([^a-zA-Z0-9_.~%:\/!*();\@&=+\$,\?#[]-])/sprintf("%%%X",ord($1))/egs;
		} else {
			$uri =~ s/([^a-zA-Z0-9_.~%:'\\\/!*();\@&=+\$,\?#[]-])/sprintf("%%%X",ord($1))/egs;
		}
	}
	return $uri;
}

my $parser = new MIME::Parser;

my %closedurls;

sub context_char_count
{
    # 4 is for the border width on either side
    return ($list_width - length(" =>URL<= "))/2;
}

sub process_sincelast
{
	my($url,$prev,$sincelast) = @_;
	if (length($prev) > 0 && ! exists($closedurls{$prev})) {
        # Fill the remaining space with context
        my $remaining_space = $list_width - length($orig_text{$prev});
		$orig_text{$prev} .= " ".substr($sincelast,0,$remaining_space);
		$closedurls{$prev} = 1;
		#print "URL(".$link_hash{$prev}.":".$newlink."): $prev ->\n\t".$orig_text{$prev}."\n\n";
	}
	if (! exists($closedurls{$url})) {
		my $beforetext = substr $sincelast, -1 * &context_char_count();
		if (length($beforetext)) {
            $orig_text{$url} = "$beforetext <|URL|>";
		} else {
			$orig_text{$url} = "<|URL|>";
		}
	}
}

sub extract_url_from_text {
	($foundurl_text_text) = @_;
	# The idea here is to eliminate duplicate URLs - I want the
	# %link_hash to be full of URLs. My regex (in the else statement)
	# is decent, but imperfect. URI::Find is better.
	my $fancyfind=1;
	eval "use URI::Find::Schemeless; 1" or $fancyfind=0;
	if ($fancyfind == 1) {
		#print "FancyFind\n";
		my $finder = URI::Find::Schemeless->new(\&foundurl_text);
		$finder->find($foundurl_text_text);
	} else {
		#print "ManualFind\n";
		$$foundurl_text_text =~ s{(((mms|ftp|http|https)://|news:)[][A-Za-z0-9_.~!*'();:@&=+,/?%#-]+[^](,.'">;[:space:]]|(mailto:)?[-a-zA-Z_0-9.+]+@[-a-zA-Z_0-9.]+)}{
			&foundurl_text($1,$1);
		}eg;
		$$foundurl_text_text =~ s{(((mms|ftp|http|https)://|news:)?[][A-Za-z0-9_.~!*'();:@&=+,/?%#-]+\.(com|net|org|gov)(\.[a-zA-Z]{2})?([][A-Za-z0-9_.~!*'();:@&=+,/?%#-]+[^](,.'">;[:space:]])?|(mailto:)?[-a-zA-Z_0-9.+]+@[-a-zA-Z_0-9.]+)}{
			&foundurl_text($1,$1);
		}eg;
	}
}

my $seenstart = 0;
my $seenurl = "";
my $beforetext = "";
my $extendedskipped = "";
my $last10words = "";
my $words_since_link_end = "";

sub tidytext
{
	my ($text) = @_;
	my %rendermap = (
		'[\n]' => ' ',
		'[\r]' => ' ',
		'&#[0-9]+;' => '',
		'&#x[0-9a-f]+;' => '',
		'&nbsp;' => ' ',
		'&copy;' => '(c)',
		'&mdash;' => '---',
		'&quot;' => '"',
		'&apos;' => "'",
		'&lt;' => '<',
		'&gt;' => '>',
		'&([ACEINOUY])(grave|acute|circ|tilde|uml|ring|cedil);' => '$1',
		'&amp;' => '&',
		'\s\s+' => ' ',
	);
	foreach my $entity (keys %rendermap) {
		my $construct = '$text =~ s/$entity/'.$rendermap{$entity}.'/ig';
		eval $construct;
	}
	$text =~ s/^\s+//;
	$text =~ s/\s+$//;
	return $text;
}

sub subwords
{
	my ($string, $minlen) = @_;
	my @words = split(/\s+/, $string);
	return "" if @words == 0;
	my $retstr = $words[0];
	my $wordcount = 1;
	while (length($retstr) < $minlen && $wordcount < @words) {
		$retstr .= " " . $words[$wordcount];
		$wordcount++;
	}
	return $retstr;
}

sub sublastwords
{
	my ($string, $minlen) = @_;
	my @words = split(/\s+/, $string);
	return "" if @words == 0;
	my $retstr = $words[@words-1];
	my $wordcount = 1;
	while (length($retstr) < $minlen && $wordcount < @words) {
		$wordcount++;
		$retstr = $words[@words - $wordcount] . " $retstr";
	}
	return $retstr;
}

sub find_urls_rec
{
	my($ent) = @_;
	#print "type: " . $ent->mime_type . " ... parts: ".$ent->parts."\n";
	if ($ent->parts >= 1 or $ent->mime_type eq "multipart/mixed") {
		for (my $i=0;$i<$ent->parts;$i++) {
			find_urls_rec($ent->parts($i));
		}
	} else {
		#print "type: " . $ent->mime_type . "\n";
		if ($ent->mime_type eq "message/rfc822") { &find_urls_rec($ent->parts()); }
		elsif ($ent->mime_type eq "text/html" ) {
			my $parser = HTML::Parser->new(api_version=>3);
			my $skipped_text = "";
			#$parser->unbroken_text(1);
			$parser->handler(start => sub {
					my($tagname,$pos,$text) = @_;
					if (my $link_attr = $link_attr{$tagname}) {
						while (4 <= @$pos) {
							my($k_offset, $k_len, $v_offset, $v_len) = splice(@$pos,-4);
							my $attrname = lc(substr($text, $k_offset, $k_len));
							next unless exists($link_attr->{$attrname});
							next unless $v_offset; # 0 v_offset means no value

							# This is REALLY hack-ish and fragile, but can
							# sometimes be invaluable
							&extract_url_from_text(\$skipped_text) if (length($skipped_text) > 0);

							my $v = substr($text, $v_offset, $v_len);
							$v =~ s/^([\'\"])(.*)\1$/$2/;
							$v = &renderuri($v);
							&foundurl($v);

							$words_since_link_end .= " $skipped_text";
							$last10words = &tidytext("$last10words $skipped_text");
							$last10words = &sublastwords($last10words, 50);
							$skipped_text = "";

							$words_since_link_end = &tidytext($words_since_link_end);
							if (length($seenurl) > 0 && ! exists($closedurls{$seenurl})) {
								my $since_words = &subwords($words_since_link_end, 40);
								if (length($since_words) > 0) {
									my $space = " ";
									$space = "" if ($since_words =~ /^[.,;!?)-]/);
									$orig_text{$seenurl} .= "$space$since_words";
								}
								$closedurls{$seenurl} = 1;
							}

							$beforetext = &sublastwords($last10words, 30);
							$seenstart = 1;
							$seenurl = $v;
						}
					}
				},
				"tagname, tokenpos, text");
			$parser->handler(end => sub {
					my ($text) = @_;
					$last10words = &tidytext("$last10words $skipped_text");
					$last10words = &sublastwords($last10words, 50);
					if ($seenstart == 1) {
						if (! exists($closedurls{$seenurl})) {
							my $mtext = "=>$skipped_text<=";
							if (length($beforetext)) {
								my $space = " ";
								$space = "" if ($beforetext =~ /[(-]$/);
								$orig_text{$seenurl} = "$beforetext$space$mtext";
							} else {
								$orig_text{$seenurl} = "$mtext";
							}
						}
						if (length($skipped_text) == 0 && $ignore_empty == 1 && ! exists($closedurls{$seenurl})) {
							&unfindurl($seenurl);
						}
						$seenstart = 0;
						$extendedskipped .= " $skipped_text";
						$words_since_link_end = "";
					} else {
						$words_since_link_end .= " $skipped_text";
					}
					$skipped_text = "";
				},"text");
			# the "text" handler is used, rather than skipped_text because
			# otherwise blocks of text at the beginning of a "lightly html-ified"
			# document can be ignored.
			$parser->handler(text => sub {
					my ($text) = @_;
					$skipped_text = &tidytext("$skipped_text $text");
				}, "text");
			$parser->parse($ent->bodyhandle->as_string);
			$parser->eof;
			if (length($words_since_link_end) > 0) {
				# This is REALLY hack-ish and fragile, but can
				# sometimes be invaluable
				&extract_url_from_text(\$words_since_link_end);
			}
			if (length($skipped_text) > 0) {
				&extract_url_from_text(\$skipped_text);
			}
		} elsif ($ent->mime_type =~ /text\/.*/) {
			$ent->head->unfold;
			my $ctype = $ent->head->get('Content-type');
			if (defined($ctype) and $ctype =~ m/format=flowed/) {
				my @lines = $ent->bodyhandle->as_lines;
				chomp(@lines);
				my $body = "";
				my $delsp;
				if ($ctype =~ /delsp=yes/) {
					#print "delsp=yes!\n";
					$delsp=1;
				} else {
					#print "delsp=no!\n";
					$delsp=0;
				}
				for (my $i=0;$i<@lines;$i++) {
					my $col = 0;
					my $quotetext = "";
					#print "=> " . $lines[$i] . "\n";
					while (substr($lines[$i],$col,1) eq ">") {
						$quotetext .= ">";
						$col++;
					}
					if ($col > 0) { $body .= "$quotetext "; }
					while ($lines[$i] =~ / $/ && $lines[$i] =~ /^$quotetext[^>]/ && $lines[$i+1] =~ /^$quotetext[^>]/) {
						my $line;
						if ($delsp) {
							$line = substr($lines[$i],$col,length($lines[$i])-$col-1);
						} else {
							$line = substr($lines[$i],$col);
						}
						$line =~ s/^\s+//;
						$body .= $line;
						$i++;
					}
					if ($lines[$i] =~ /^$quotetext[^>]/) {
						my $line = substr($lines[$i],$col);
						$line =~ s/^\s+//;
						$body .= $line."\n";
					}
				}
				&extract_url_from_text(\$body);
			} else {
				&extract_url_from_text(\$ent->bodyhandle->as_string);
			}
		}
	}
}

sub urlwrap {
	my($subseq,$text,$linelen,$breaker) = @_;
	my $len = length($text);
	my $i = 0;
	my $output = "";
	while ($len > $linelen) {
		if ($i > 0) { $output .= $subseq; }
		my $breakpoint = -1;
		my $chunk = substr($text,$i,$linelen);
		my @chars = ("!","*","'","(",")",";",":","@","&","=","+",",","/","?","%","#","[","]","-","_");
		foreach my $chr ( @chars ) {
			my $pt = rindex($chunk,$chr);
			if ($breakpoint < $pt) { $breakpoint = $pt; }
		}
		if ($breakpoint == -1) { $breakpoint = $linelen; }
		else { $breakpoint += 1; }
		$output .= substr($text,$i,$breakpoint) . $breaker;
		if ($i == 0) { $linelen -= length($subseq); }
		$len -= $breakpoint;
		$i += $breakpoint;
	}
	if ($i > 0) { $output .= $subseq; }
	$output .= substr($text,$i);
	return $output;
}

sub isOutputScreen {
	use POSIX;
	return 0 if POSIX::isatty( \*STDOUT) eq "" ; # pipe
	return 1; # screen
} # end of isOutputScreen

&getprefs();
$parser->output_to_core(1);
my $filecontents;
if ($#ARGV == 0) {
	open(INPUT, "<$ARGV[0]") or die "Couldn't open input file $ARGV[0]: $!";
	$filecontents = join('',<INPUT>);
	close(INPUT);
} else {
	die "no input provided!\n" if POSIX::isatty( \*STDIN) ne "" ; # pipe
	$filecontents = join('',<STDIN>);
}

if (not $txtonly) {
	my $entity = $parser->parse_data($filecontents);
	&find_urls_rec($entity);
	if (scalar(keys %link_hash) == 0) {
		&extract_url_from_text(\$filecontents);
	}
} else {
	if ($manual_quoted) {
		if (eval "use MIME::QuotedPrint; 1") {
			$filecontents = decode_qp($filecontents);
		} else {
			$filecontents =~ s/=\r?\n//g;
			$filecontents =~ s/=([A-Fa-f0-9]{2})/chr(hex($1))/egs;
		}
	}
	&extract_url_from_text(\$filecontents);
}

if (&isOutputScreen) {
	if ($fancymenu == 1) {
		eval "use Curses::UI; 1" or $fancymenu = 0;
	}
} else {
	$fancymenu = 0;
}

if ($fancymenu == 1) {
	#use strict;

	# This is the shortcut...
	if ($shortcut == 1 && 1 == scalar keys %link_hash) {
		my ($url) = each %link_hash;
		$url = &sanitizeuri($url);
		if ($command_uses_shell) {
			$urlviewcommand =~ s/%s/'$url'/g;
			system $urlviewcommand;
		} else {
			# This technique means the URL will not be subject to shell escaping
			my @args = ($urlviewcommand, $url);
			system $urlviewcommand @args;
		}
		exit 0;
	}

	# Curses support really REALLY wants to own STDIN
	close(STDIN);
	open(STDIN,"/dev/tty"); # looks like a hack, smells like a hack...

	my $cui = new Curses::UI(
		-color_support => 1,
		-clear_on_exit => 1
	);
	my $wrapwidth = $cui->width() - 2;
	my %listhash_url;
	my %listhash_context;
	my @listvals;
	# $link_hash{url} = ordering of the urls in the document as first-seen
	foreach my $url (sort {$link_hash{$a} <=> $link_hash{$b} } keys(%link_hash)) {
		push(@listvals,$link_hash{$url});
		if ($displaysanitized) {
			$listhash_url{$link_hash{$url}} = &sanitizeuri($url);
		} else {
			$listhash_url{$link_hash{$url}} = $url;
		}
		$listhash_context{$link_hash{$url}} = $orig_text{$url};
	}

	my @menu = (
		{ -label => 'Keys: q=quit m=menu s=switch-view c=context g=top G=bottom', 
			-submenu => [
			{ -label => 'About              a', -value => \&about },
			{ -label => 'Show Command       C', -value => \&show_command },
			{ -label => 'Switch List View   s', -value => \&switch_list },
			{ -label => 'Exit              ^q', -value => \&exit_dialog  }
			],
		},
	);
	my $menu = $cui->add(
                'menu','Menubar', 
                -menu => \@menu,
        );
	my $win1 = $cui->add(
			'win1', 'Window',
			-border => 1,
			-y    => 1,
			-bfg  => 'green',
		);
	sub about()
	{
		$cui->dialog(
			-message => "$NAME $version License:$LICENSE"
		);
	}
	sub show_command()
	{
		# This extra sprintf work is to ensure that the title
		# is fully displayed even if $urlviewcommand is short
		my $title = "The configured URL viewing command is:";
		my $len = length($title);
		my $cmd = sprintf("%-${len}s",$urlviewcommand);
		$cui->dialog(
			-title => "The configured URL viewing command is:",
			-message => $cmd,
		);
	}
	sub exit_dialog()
	{
		my $return = $cui->dialog(
			-message   => "Do you really want to quit?",
			-buttons   => ['yes', 'no'],
		);
		exit(0) if $return;
	}

	my $listbox_labels;
	if ($default_view eq "url") {
		$listbox_labels = \%listhash_url;
	} else {
		$listbox_labels = \%listhash_context;
	}
	my $listbox = $win1->add(
		'mylistbox', 'Listbox',
		-values    => \@listvals,
		-labels    => $listbox_labels,
		);
	$cui->set_binding(sub {$menu->focus()}, "\cX");
	$cui->set_binding(sub {$menu->focus()}, "m");
	$cui->set_binding( sub{exit}, "q" );
	$cui->set_binding( \&exit_dialog , "\cQ");
	$cui->set_binding( sub{exit} , "\cc");
	$cui->set_binding(\&switch_list, "s");
	$cui->set_binding(\&about, "a");
	$cui->set_binding(\&show_command, "C");
	$listbox->set_binding( 'option-last', "G");
	$listbox->set_binding( 'option-first', "g");
	sub switch_list()
	{
		if ($listbox_labels == \%listhash_url) {
			$listbox->labels(\%listhash_context);
			$listbox_labels = \%listhash_context;
		} elsif ($listbox_labels == \%listhash_context) {
			$listbox->labels(\%listhash_url);
			$listbox_labels = \%listhash_url;
		}
		$listbox->focus();
	}
	sub madeselection_sub {
		my ($stayopen) = @_;
		my $rawurl = $listhash_url{$listbox->get_active_value()};
		my $url = &sanitizeuri($rawurl);
		my $command = $urlviewcommand;
		my @commandargs;
		if ($command_uses_shell) {
			$command =~ s/%s/'$url'/g;
		} else {
			@commandargs = ($command, $url);
		}
		my $return = 1;
		if ($noreview != 1 && length($rawurl) > ($cui->width()-2)) {
				$return = $cui->dialog(
					-message => &urlwrap("  ",$rawurl,$cui->width()-8,"\n"),
					-title => "Your Choice:",
					-buttons => ['ok', 'cancel'],
				);
		}
		if ($return) {
			if ($command_uses_shell) {
				system $command;
			} else {
				system $command @commandargs;
			}
			if ($stayopen == 0) {
				exit 0 if ($persist == 0);
			} else {
				exit 0 unless ($persist == 0);
			}
		}
	}
	sub madeselection { &madeselection_sub(0); }
	sub altexit_madeselection { &madeselection_sub(1); }
	$cui->set_binding( \&madeselection, " ");
	$listbox->set_routine('option-select',\&madeselection);
	$cui->set_binding( \&altexit_madeselection, $alt_select_key);
	use Text::Wrap;
	sub contextual {
		my $rawurl = $listhash_url{$listbox->get_active_value()};
		$Text::Wrap::columns = $cui->width()-8;
		if (exists($orig_text{$rawurl}) && length($orig_text{$rawurl}) > 1) {
			$cui->dialog(
				-message => wrap('','',$orig_text{$rawurl}),
				-title => "Context:",
				-buttons => ['ok'],
			);
		} else {
			$cui->error(
				-message => "Sorry, I don't have any context for this link",
				-buttons => ['ok'],
				-bfg => 'red',
				-tfg => 'red',
				-fg => 'red',
			);
		}
	}
	$cui->set_binding( \&contextual, "c");

	$listbox->focus();
	$cui->mainloop();
} else {
	# using this as a pass-thru to URLVIEW
	$command_uses_shell = 0;
	foreach my $value (sort {$link_hash{$a} <=> $link_hash{$b} } keys %link_hash)
	{
		$value = &sanitizeuri($value);
		print "$value\n";
	}
}

__END__

=pod

=head1 NAME

extract_url -- extract URLs from email messages

=head1 SYNOPSIS

extract_url [options] I<file>

=head1 DESCRIPTION

This is a Perl script that extracts URLs from correctly-encoded
I<MIME> email messages. This can be used either as a pre-parser for
I<urlview>, or to replace I<urlview> entirely.

I<Urlview> is a great program, but has some deficiencies. In particular,
it isn't particularly configurable, and cannot handle URLs that have
been broken over several lines in I<format=flowed delsp=yes> email
messages.  Nor can it handle I<quoted-printable> email messages. Also,
I<urlview> doesn't eliminate duplicate URLs. This Perl script handles
all of that.  It also sanitizes URLs so that they can't break out of the
command shell.

This is designed primarily for use with the I<mutt> emailer. The idea is
that if you want to access a URL in an email, you pipe the email to a
URL extractor (like this one) which then lets you select a URL to view
in some third program (such as Firefox). An alternative design is to
access URLs from within mutt's pager by defining macros and tagging the
URLs in the display to indicate which macro to use. A script you can use
to do that is I<tagurl.pl>.  

=head1 OPTIONS

=over 4

=item B<-h, --help>

Display this help and exit.

=item B<-m, --man>

Display the full man page documentation.

=item B<-l, --list>

Prevent use of Ncurses, and simply output a list of extracted URLs.

=item B<-t, --text>

Prevent MIME handling; treat the input as plain text.

=item B<-q, --quoted>

Force a quoted-printable decode on plain text.

=item B<-c, --config>

Specify a config file to read.

=item B<-V, --version>

Output version information and exit.

=back

=head1 DEPENDENCIES

Mandatory dependencies are B<MIME::Parser> and B<HTML::Parser>.  These usually
come with Perl. 

Optional dependencies are B<URI::Find> (recognizes more exotic URL
variations in plain text (without HTML tags)), B<Curses::UI> (allows it
to fully replace I<urlview>), B<MIME::Quoted> (does a more standardized decode
of quoted-printable characters in plain text), and B<Getopt::Long> (if present,
B<extract_url.pl> recognizes long options --version and --list).

=head1 EXAMPLES

This Perl script expects a valid email to be either piped in via STDIN or in a
file listed as the script's only argument. Its STDOUT can be a pipe into
I<urlview> (it will detect this). Here's how you can use it:

    cat message.txt | extract_url.pl
    cat message.txt | extract_url.pl | urlview
    extract_url.pl message.txt
    extract_url.pl message.txt | urlview

For use with B<mutt 1.4.x>, here's a macro you can use:

    macro index,pager \cb "\
    <enter-command> \
    unset pipe_decode<enter>\
	<pipe-message>extract_url.pl<enter>" \
    "get URLs"

For use with B<mutt 1.5.x>, here's a more complicated macro you can use:

    macro index,pager \cb "\
    <enter-command> set my_pdsave=\$pipe_decode<enter>\
    <enter-command> unset pipe_decode<enter>\
    <pipe-message>extract_url.pl<enter>\
    <enter-command> set pipe_decode=\$my_pdsave<enter>" \
    "get URLs"

Here's a suggestion for how to handle I<encrypted email>:

    macro index,pager ,b "\
    <enter-command> set my_pdsave=\$pipe_decode<enter>\
    <enter-command> unset pipe_decode<enter>\
    <pipe-message>extract_url.pl<enter>\
    <enter-command> set pipe_decode=\$my_pdsave<enter>" \
    "get URLs"

    macro index,pager ,B "\
    <enter-command> set my_pdsave=\$pipe_decode<enter>\
    <enter-command> set pipe_decode<enter>\
    <pipe-message>extract_url.pl<enter>\
    <enter-command> set pipe_decode=\$my_pdsave<enter>" \
    "decrypt message, then get URLs"

    message-hook .  'macro index,pager \cb ,b "URL viewer"'
    message-hook ~G 'macro index,pager \cb ,B "URL viewer"'

=head1 CONFIGURATION

If you're using it with B<Curses::UI> (i.e. as a standalone URL
selector), this Perl script will try and figure out what command to use
based on the contents of your F<~/.urlview> file. However, it also has
its own configuration file (F<~/.extract_urlview>) that will be used
instead, if it exists. So far, there are eight kinds of lines you can
have in this file:

=over 8

=item COMMAND ...

This line specifies the command that will be used to view URLs.  This
command CAN contain a I<%s>, which will be replaced by the URL inside
single-quotes. If it does not contain a I<%s>, the URL will simply be
appended to the command. If this line is not present, the command is
taken from the environment variable $BROWSER. If BROWSER is not set, the
command is assumed to be "open", which is the correct command for MacOS X
systems.

=item SHORTCUT

This line specifies that if an email contains only 1 URL, that URL will
be opened without prompting. The default (without this line) is to
always prompt.

=item NOREVIEW

Normally, if a URL is too long to display on screen in the menu, the
user will be prompted with the full URL before opening it, just to make
sure it's correct. This line turns that behavior off.

=item PERSISTENT

By default, when a URL has been selected and viewed from the menu,
B<extract_url.pl> will exit. If you would like it to be ready to view
another URL without re-parsing the email (i.e. much like standard
I<urlview> behavior), add this line to the config file.

=item IGNORE_EMPTY_TAGS

By default, the script collects all the URLs it can find.  Sometimes,
though, HTML messages contain links that don't correspond to any text
(and aren't normally rendered or accessible). This tells the script to
ignore these links.

=item RAW_RESERVED

By default, the script sanitizes URLs pretty thoroughly, eliminating all characters that are not part of the Unreserved class (per RFC 3986). Sometimes, though, this is not desirable. This tells the script to leave the Reserved Characters un-encoded (with the exception of the single quote).

=item HTML_TAGS ...

This line specifies which HTML tags will be examined for URLs. By
default, the script is very generous, looking in I<a>, I<applet>,
I<area>, I<blockquote>, I<embed>, I<form>, I<frame>, I<iframe>,
I<input>, I<ins>, I<isindex>, I<head>, I<layer>, I<link>, I<object>,
I<q>, I<script,> and I<xmp> tags for links. If you would like it to
examine just a subset of these (e.g. you only want a tags to be
examined), merely list the subset you want. The list is expected to be a
comma-separated list. If there are multiple of these lines in the config
file, the script will look for the minimum set of specified tags.

=item ALTSELECT ...

This line specifies a key for an alternate url viewing behavior.  By
default, B<extract_url.pl> will quit after the URL viewer has been
launched for the selected URL. This key will then make B<extract_url.pl>
launch the URL viewer but will not quit. However, if I<PERSISTENT> is
specified in the config file, the opposite is true: normal selection of
a URL will launch the URL viewer and will not cause B<extract_url.pl> to
exit, but this key will. This setting defaults to I<k>.

=item DEFAULT_VIEW {url|context}

This line specifies whether to show the list of URLs at first or to show
the url contexts when the program is run. By default, B<extract_url.pl>
shows a list of URLs.

=back

Here is an example config file:

    SHORTCUT
    COMMAND mozilla-firefox -remote "openURL(%s,new-window)"
    HTML_TAGS a,iframe,link
    ALTSELECT Q
    DEFAULT_VIEW context

=head1 STANDARDS

None.

=head1 AVAILABILITY

http://www.memoryhole.net/~kyle/extract_url/

=head1 SEE ALSO

mutt(1)
urlview(1)
urlscan(1)

=head1 CAVEATS

All URLs have any potentially dangerous shell characters (namely a
single quote and a dollar sign) removed (transformed into
I<percent-encoding>) before they are used in a shell. This should
eliminate the possibility of a bad URL breaking the shell.

If using B<Curses::UI>, and a URL is too big for your terminal, when you
select it, B<extract_url.pl> will (by default) ask you to review it in a
way that you can see the whole thing. 

=head1 AUTHOR

Program was written by Kyle Wheeler <kyle@memoryhole.net>

Released under license BSD-2-Cluase (simplified) For more information
about the license, visit <http://spdx.org/licenses/BSD-2-Clause>.

=cut
