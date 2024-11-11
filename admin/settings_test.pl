#!/usr/bin/env perl

use strict;
use warnings FATAL => 'all';

my ( $GITMODULES, $SETTINGS ) = @ARGV;

sub read_file {
    my $file = shift;
    open my $fh, '<', $file or die "Could not open $file: $!";
    my @lines = <$fh>;
    return @lines;
}

my @gitmodules = read_file($GITMODULES);    # .gitmodules
my @settings   = read_file($SETTINGS);      # settings.yaml

my @git_modules =
  grep { !m!\.wiki$! }
  map  { m!\turl = https://github.com/TokTok/([^/]+)\n! ? lc $1 : () }
  @gitmodules;
my @settings_modules =
  map { m!^    name: "?([^"]+)"?\n! ? lc $1 : () } @settings;

# This is the container, so isn't in .gitmodules.
push @git_modules, 'toktok-stack';

# Not in settings.yaml because this is just a storage repo.
# We don't need checks for it.
push @settings_modules, 'toktok-fuzzer';

my $ok = 1;
for my $module (@settings_modules) {
    if ( !grep { $_ eq $module } @git_modules ) {
        print "Module $module is in settings.yaml but not in .gitmodules\n";
        $ok = 0;
    }
}

for my $module (@git_modules) {
    if ( !grep { $_ eq $module } @settings_modules ) {
        print "Module $module is in .gitmodules but not in settings.yaml\n";
        $ok = 0;
    }
}

exit !$ok;
