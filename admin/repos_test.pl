#!/usr/bin/env perl

use strict;
use warnings FATAL => 'all';

my ( $GITMODULES, $REPOS ) = @ARGV;

sub read_file {
    my $file = shift;
    open my $fh, '<', $file or die "Could not open $file: $!";
    my @lines = <$fh>;
    return @lines;
}

my @gitmodules = read_file($GITMODULES);    # .gitmodules
my @repos      = read_file($REPOS);         # repos.yaml

my @git_modules =
  grep { !m!\.wiki$! }
  map  { m!\turl = https://github.com/TokTok/([^/]+)\n! ? lc $1 : () }
  @gitmodules;
my @repos_modules =
  map { m!^    name: "?([^"]+)"?\n! ? lc $1 : () } @repos;

# This is the container, so isn't in .gitmodules.
push @git_modules, 'toktok-stack';

# Not in repos.yaml because this is just a storage repo.
# We don't need checks for it.
push @repos_modules, 'toktok-fuzzer';

my $ok = 1;
for my $module (@repos_modules) {
    if ( !grep { $_ eq $module } @git_modules ) {
        print "Module $module is in repos.yaml but not in .gitmodules\n";
        $ok = 0;
    }
}

for my $module (@git_modules) {
    if ( !grep { $_ eq $module } @repos_modules ) {
        print "Module $module is in .gitmodules but not in repos.yaml\n";
        $ok = 0;
    }
}

exit !$ok;
