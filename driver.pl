#!/usr/bin/perl

use strict;
use warnings;

my$outputDir = "data";
my$rawExt = ".xml";

sub processFileContent {
	my($str)=@_;
	return (length $str);
}

sub getBugs{
	my@files = glob("$outputDir/*.xml");
	my@result;
	for my $file (@files) {
		open FILE, $file;
		undef local $/;
		my$slurp=<FILE>;
		push @result, (processFileContent($slurp));
	}
	return \@result;
}

my$res=getBugs();
print "@$res";

